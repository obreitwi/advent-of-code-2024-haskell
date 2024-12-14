{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Attoparsec.Text hiding (take)
import Data.Attoparsec.Text qualified as P
import Data.FileEmbed (embedFileRelative)
import Data.HashMap.Strict ((!?))
import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Deque.Strict qualified as DQ
import Rebase.Prelude hiding (IntMap, check, left, matchM, matchS, right, rotate, takeWhile)
import Prelude ()

main :: IO ()
main = do
  putStr "Part 1 (debug, expect 36): "
  either error (print . part1) $ parseOnly parseInput debug

  putStr "part 1: "
  either error (print . part1) $ parseOnly parseInput input

  putStr "part 2 (debug, expect 81): "
  either error (print . part2) $ parseOnly parseInput debug

  putStr "part 2: "
  either error (print . part2) $ parseOnly parseInput input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

debug :: T.Text
debug = decodeUtf8Lenient $(embedFileRelative "./debug")

parseInput :: Parser IntMap
parseInput = parseIntMap

type Position = (Int, Int)
type IntMap = HashMap Position Int

parseIntMap :: Parser IntMap
parseIntMap = Map.fromList . flatten . enumerate <$> many1' row
 where
  row :: Parser [(Int, Int)]
  row = enumerate <$> (many1' (digitToInt <$> notChar '\n') <* endOfLine)

  flatten :: [(Int, [(Int, Int)])] -> [((Int, Int), Int)]
  flatten = concatMap (\(j, l) -> map (\(i, c) -> ((i, j), c)) l)

  enumerate :: [a] -> [(Int, a)]
  enumerate = go 0
   where
    go _ [] = []
    go i (l : ll) = (i, l) : go (i + 1) ll

part1 :: IntMap -> Int
part1 m = m & (startingPositions >>> map (countTrailheads m) >>> sum)

part2 :: IntMap -> Int
part2 m = m & (startingPositions >>> map (countDistinctTrailheads m) >>> sum)

startingPositions :: IntMap -> [Position]
startingPositions = Map.filter (== 0) >>> Map.toList >>> map fst

countTrailheads :: IntMap -> Position -> Int
countTrailheads m start = Set.size $ go (m !? start) start
 where
  go :: Maybe Int -> Position -> HashSet Position
  go Nothing = const Set.empty
  go (Just 9) = Set.singleton
  go (Just n) = neighbors >>> map (\q -> (m !? q, q)) >>> filter (valid (n + 1)) >>> map (uncurry go) >>> foldl' Set.union Set.empty

  valid :: Int -> (Maybe Int, Position) -> Bool
  valid target (Just next, _) | target == next = True
  valid _ _ = False

countDistinctTrailheads :: IntMap -> Position -> Int
countDistinctTrailheads m start = go (m !? start) start
 where
  go :: Maybe Int -> Position -> Int
  go Nothing = const 0
  go (Just 9) = const 1
  go (Just n) = neighbors >>> map (\q -> (m !? q, q)) >>> filter (valid (n + 1)) >>> map (uncurry go) >>> sum

  valid :: Int -> (Maybe Int, Position) -> Bool
  valid target (Just next, _) | target == next = True
  valid _ _ = False

neighbors :: Position -> [Position]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
