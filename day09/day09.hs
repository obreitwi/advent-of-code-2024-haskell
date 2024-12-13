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

{-
 -   putstr "part 2 (debug, expect 34): "
 -   either error (print . part2) $ parseonly parseinput debug
 -
 -   putstr "part 2: "
 -   either error (print . part2) $ parseonly parseinput input
 -}

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

uncurry3 :: (a, b, c) -> (a -> b -> c -> d) -> d
uncurry3 (a, b, c) f = f a b c

checksum :: DQ.Deque Block -> Int
checksum b = uncurry3 (consnoc b) (go 0 0)
 where
  go :: Int -> Int -> Maybe Block -> DQ.Deque Block -> Maybe Block -> Int
  go cs _ Nothing _ Nothing = trace "reached end" cs
  go cs idx (Just front) q (Just (Block _ 0)) = uncurry (go cs idx (Just front)) (getBack q)

  go cs idx (Just front) q (Just (Empty _)) = uncurry (go cs idx (Just front)) (getBack q)
  go cs idx (Just (Block _ 0)) q back = uncurry (go cs idx) (getFront q) back
  go cs idx (Just (Empty 0)) q back = uncurry (go cs idx) (getFront q) back 

  -- -- reached the end
  go cs idx (Just (Block idFront lenFront)) _ Nothing = trace "only front" $ cs + (sum . map (idFront *) $ [idx .. (idx + lenFront - 1)]) -- reached the end
  go cs idx Nothing _ (Just (Block idBack lenBack)) = trace "only back" $ cs + (sum . map (idBack *) $ [idx .. (idx + lenBack - 1)]) -- reached the end

  go cs idx (Just (Block idFront lenFront)) q back = go (cs + idx * idFront) (idx + 1) (Just (Block idFront (lenFront - 1))) q back

  go cs idx (Just (Empty lenFront)) q (Just (Block idBack lenBack)) =
     go (cs + idBack * idx) (idx + 1) (Just (Empty (lenFront - 1))) q (Just (Block idBack (lenBack - 1)))

  go cs _ (Just (Empty _)) _ Nothing = cs

  go _ _ _ _ _  = undefined

  getFront :: DQ.Deque Block -> (Maybe Block, DQ.Deque Block)
  getFront q = (fst <$> DQ.uncons q, maybe (fromList []) snd $ DQ.uncons q)

  getBack :: DQ.Deque Block -> (DQ.Deque Block, Maybe Block)
  getBack q = (maybe (fromList []) snd $ DQ.unsnoc q, fst <$> DQ.unsnoc q)

consnoc :: DQ.Deque a -> (Maybe a, DQ.Deque a, Maybe a)
consnoc q = go (DQ.uncons q)
 where
  go Nothing = (Nothing, fromList [], Nothing)
  go (Just (frst, q')) = go' frst (DQ.unsnoc q')

  go' frst Nothing = (Just frst, fromList [], Nothing)
  go' frst (Just (lst, q'')) = (Just frst, q'', Just lst)
