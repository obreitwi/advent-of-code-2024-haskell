{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Attoparsec.Text hiding (take)
import Data.FileEmbed (embedFileRelative)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Rebase.Prelude hiding (IntMap, check, left, matchM, matchS, right, rotate, takeWhile)
import Prelude ()

main :: IO ()
main = do
  putStr "Part 1 (debug, expect 1930): "
  either error (print . part1) $ parseOnly parseInput debug

  putStr "Part 1 (debug2, expect 772): "
  either error (print . part1) $ parseOnly parseInput debug2

  putStr "Part 1: "
  either error (print . part1) $ parseOnly parseInput input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

debug :: T.Text
debug = decodeUtf8Lenient $(embedFileRelative "./debug")

debug2 :: T.Text
debug2 = decodeUtf8Lenient $(embedFileRelative "./debug2")

type Position = (Int, Int)
type CharMap = HashMap Position Char

parseInput :: Parser CharMap
parseInput = parseCharMap

parseCharMap :: Parser CharMap
parseCharMap = Map.fromList . flatten . enumerate <$> many1' row
 where
  row :: Parser [(Int, Char)]
  row = enumerate <$> (many1' (notChar '\n') <* endOfLine)

  flatten :: [(Int, [(Int, Char)])] -> [((Int, Int), Char)]
  flatten = concatMap (\(j, l) -> map (\(i, c) -> ((i, j), c)) l)

  enumerate :: [a] -> [(Int, a)]
  enumerate = go 0
   where
    go _ [] = []
    go i (l : ll) = (i, l) : go (i + 1) ll

uniqChars :: CharMap -> HashSet Char
uniqChars = Map.elems >>> Set.fromList

perimeter :: HashSet Position -> Int
perimeter s = s & (Set.toList >>> map calcPerimeter >>> foldl' (+) 0)
 where
  calcPerimeter :: Position -> Int
  calcPerimeter = neighbors >>> filter (`Set.member` s) >>> length >>> (4 -)

cost :: HashSet Position -> Int
cost s = area s * perimeter s

area :: HashSet Position -> Int
area = Set.size

neighbors :: Position -> [Position]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

part1 :: CharMap -> Int
part1 m = m & (uniqRegions >>> Set.toList >>> map cost >>> foldl' (+) 0)

uniqRegionsWithChar :: CharMap -> HashSet (Char, HashSet (HashSet Position))
uniqRegionsWithChar m = m & (uniqChars >>> Set.map (\c -> (c, c & (charToRegion m >>> disjointRegions >>> Set.fromList))))

uniqRegions :: CharMap -> HashSet (HashSet Position)
uniqRegions m =
  m
    & ( uniqChars
          >>> Set.map (charToRegion m)
          >>> Set.map disjointRegions
          >>> Set.map Set.fromList
          >>> Set.foldl' Set.union Set.empty
      )

debugInfo :: HashSet Position -> String
debugInfo r = show (area r) ++ " x " ++ (show . perimeter $ r) ++ " = " ++ (show . cost $ r)

-- return all positions with the given char
charToRegion :: CharMap -> Char -> HashSet Position
charToRegion m c = Map.filter (== c) m & (Map.keys >>> Set.fromList)

disjointRegions :: HashSet Position -> [HashSet Position]
disjointRegions remaining
  | Set.null remaining = []
  | otherwise =
      let
        completed = completeRegion remaining (Set.toList remaining & head)
       in
        completed : disjointRegions (remaining `Set.difference` completed)

completeRegion :: HashSet Position -> Position -> HashSet Position
completeRegion region initPos = go (Set.singleton initPos) initPos
 where
  go :: HashSet Position -> Position -> HashSet Position
  go seen pos = newCandidates seen pos & foldl' collect seen

  collect :: HashSet Position -> Position -> HashSet Position
  collect seen pos = go (Set.insert pos seen) pos

  newCandidates :: HashSet Position -> Position -> [Position]
  newCandidates seen = neighbors >>> filter (`notMember` seen) >>> filter (`Set.member` region)

notMember :: (Hashable a) => a -> HashSet a -> Bool
notMember e = Set.member e >>> not
