{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Attoparsec.Text hiding (take)
import Data.Attoparsec.Text qualified as P
import Data.FileEmbed (embedFileRelative)
import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Deque.Strict qualified as DQ
import Rebase.Prelude hiding (check, left, matchM, matchS, right, rotate, takeWhile)
import Prelude ()

main :: IO ()
main = do
  putStr "Part 1 (debug, expect 1928): "
  either error (print . part1) $ parseOnly parseInput debug

  putStr "part 1: "
  either error (print . part1) $ parseOnly parseInput input

  putStr "part 2 (debug, expect 2858): "
  either error (print . part2) $ parseOnly parseInput debug

  putStr "part 2: "
  either error (print . part2) $ parseOnly parseInput input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

debug :: T.Text
debug = decodeUtf8Lenient $(embedFileRelative "./debug")

parseInput :: Parser (DQ.Deque Block)
parseInput = do
  pairs <- many1' blockPair
  lastBlock <- block
  let withIDs = zipWith setIdx (pairs ++ [(lastBlock, Empty 0)]) [0 ..]
  return . fromList $ concatMap (\(p, q) -> [p, q]) withIDs
 where
  setIdx :: (Block, Block) -> Int -> (Block, Block)
  setIdx (Block _ sizeBlock, bEmpty) n = (Block n sizeBlock, bEmpty)
  setIdx _ _ = undefined

-- Block Index Length
type Index = Int
type Length = Int
data Block = Block Index Length | Empty Length deriving (Show)

pair :: Parser [Char]
pair = count 2 digit

block :: Parser Block
block = Block 0 . digitToInt <$> digit

emptyBlock :: Parser Block
emptyBlock = Empty . digitToInt <$> digit

blockPair :: Parser (Block, Block)
blockPair = (,) <$> block <*> emptyBlock

-- applyUntil :: Eq b => [] -> Parser b -> Parser [a]

part1 :: DQ.Deque Block -> Int
part1 = checksum

part2 :: DQ.Deque Block -> Int
part2 = checksum2

uncurry3 :: (a, b, c) -> (a -> b -> c -> d) -> d
uncurry3 (a, b, c) f = f a b c

checksum :: DQ.Deque Block -> Int
checksum b = uncurry3 (consnoc b) (go 0 0)
 where
  go :: Int -> Int -> Maybe Block -> DQ.Deque Block -> Maybe Block -> Int
  go cs _ Nothing _ Nothing = cs
  go cs idx (Just front) q (Just (Block _ 0)) = uncurry (go cs idx (Just front)) (getBack q)
  go cs idx (Just front) q (Just (Empty _)) = uncurry (go cs idx (Just front)) (getBack q)
  go cs idx (Just (Block _ 0)) q back = uncurry (go cs idx) (getFront q) back
  go cs idx (Just (Empty 0)) q back = uncurry (go cs idx) (getFront q) back
  -- -- reached the end
  go cs idx (Just (Block idFront lenFront)) _ Nothing = cs + (sum . map (idFront *) $ [idx .. (idx + lenFront - 1)]) -- reached the end
  go cs idx Nothing _ (Just (Block idBack lenBack)) = cs + (sum . map (idBack *) $ [idx .. (idx + lenBack - 1)]) -- reached the end
  go cs idx (Just (Block idFront lenFront)) q back = go (cs + idx * idFront) (idx + 1) (Just (Block idFront (lenFront - 1))) q back
  go cs idx (Just (Empty lenFront)) q (Just (Block idBack lenBack)) =
    go (cs + idBack * idx) (idx + 1) (Just (Empty (lenFront - 1))) q (Just (Block idBack (lenBack - 1)))
  go cs _ (Just (Empty _)) _ Nothing = cs
  go _ _ _ _ _ = undefined

getFront :: DQ.Deque Block -> (Maybe Block, DQ.Deque Block)
getFront q = (fst <$> DQ.uncons q, maybe (fromList []) snd $ DQ.uncons q)

getBack :: DQ.Deque Block -> (DQ.Deque Block, Maybe Block)
getBack q = (maybe (fromList []) snd $ DQ.unsnoc q, fst <$> DQ.unsnoc q)

checksum2 :: DQ.Deque Block -> Int
checksum2 qinit = uncurry (go 0 0 Set.empty) (getFront qinit)
 where
  go :: Int -> Int -> HashSet Int -> Maybe Block -> DQ.Deque Block -> Int
  go cs _ _ Nothing _ = cs
  go cs idx s (Just b@(Block blockIdx len)) q
    | blockIdx `Set.member` s = go cs idx s (Just (Empty len)) q
    | otherwise = uncurry (go (cs + blocksum idx b) (idx + len) (Set.insert blockIdx s)) (getFront q)
  go cs idx s (Just (Empty 0)) q = uncurry (go cs idx s) (getFront q)
  go cs idx s (Just (Empty len)) q = go' cs idx s len q (findUnseenBack len s q)

  go' :: Int -> Int -> HashSet Int -> Int -> DQ.Deque Block -> Maybe Block -> Int
  go' cs idx s len q Nothing = uncurry (go cs (idx + len) s) (getFront q)
  go' cs idx s len q (Just b@(Block blockIdx blockLen)) = go (cs + blocksum idx b) (idx + blockLen) (blockIdx `Set.insert` s) (Just (Empty (len - blockLen))) q
  go' _ _ _ _ _ (Just (Empty _)) = error "cannot find empty when searching"

-- find the first block that fits size from back
findUnseenBack :: Int -> HashSet Int -> DQ.Deque Block -> Maybe Block
findUnseenBack size seen = DQ.reverse >>> toList >>> find matches
 where
  matches :: Block -> Bool
  matches (Empty _) = False
  matches (Block idx len) = len <= size && not (idx `Set.member` seen)

blocksum :: Int -> Block -> Int
blocksum _ (Empty _) = 0
blocksum idx (Block idxBlock len) = -- trace ("summing block " ++ show idxBlock ++ " of size " ++ show len ++ " from " ++ show idx) $ 
  sum $ map (idxBlock *) [idx .. (idx + len - 1)]

consnoc :: DQ.Deque a -> (Maybe a, DQ.Deque a, Maybe a)
consnoc q = go (DQ.uncons q)
 where
  go Nothing = (Nothing, fromList [], Nothing)
  go (Just (frst, q')) = go' frst (DQ.unsnoc q')

  go' frst Nothing = (Just frst, fromList [], Nothing)
  go' frst (Just (lst, q'')) = (Just frst, q'', Just lst)
