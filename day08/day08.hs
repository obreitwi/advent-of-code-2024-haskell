{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.FileEmbed (embedFileRelative)
import Data.HashMap.Strict qualified as Map
import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Data.List (takeWhile)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Rebase.Prelude hiding (check, left, matchM, matchS, right, rotate, takeWhile)
import Prelude ()

main :: IO ()
main = do
  putStr "Part 1 (debug, expect 14): "
  either error (print . part1) $ parseOnly parseInput debug

  putStr "part 1: "
  either error (print . part1) $ parseOnly parseInput input

  putStr "Part 2 (debug, expect 34): "
  either error (print . part2) $ parseOnly parseInput debug

  putStr "Part 2: "
  either error (print . part2) $ parseOnly parseInput input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

debug :: T.Text
debug = decodeUtf8Lenient $(embedFileRelative "./debug")

parseInput :: Parser CharMap
parseInput = parseCharMap

part1 :: CharMap -> Int
part1 m = u & (Set.map fAntiNodes >>> Set.foldl' Set.union Set.empty >>> Set.filter withinLimits >>> Set.size)
 where
  u :: HashSet [Position]
  u = m & (uniqFreqs >>> Set.map (\f -> Map.filter (== f) (hmap m) & Map.keys))

  fAntiNodes :: [Position] -> HashSet Position
  fAntiNodes = pairs >>> Set.map antiNodes >>> Set.foldl' Set.union Set.empty

  withinLimits :: Position -> Bool
  withinLimits (x, y) = 0 <= x && x < width m && 0 <= y && y < height m

part2 :: CharMap -> Int
part2 m = u & (Set.map fAntiNodes >>> Set.foldl' Set.union Set.empty >>> Set.filter withinLimits >>> Set.size)
 where
  u :: HashSet [Position]
  u = m & (uniqFreqs >>> Set.map (\f -> Map.filter (== f) (hmap m) & Map.keys))

  fAntiNodes :: [Position] -> HashSet Position
  fAntiNodes = pairs >>> Set.map (antiNodesLine m) >>> Set.foldl' Set.union Set.empty

  withinLimits :: Position -> Bool
  withinLimits (x, y) = 0 <= x && x < width m && 0 <= y && y < height m

uniqFreqs :: CharMap -> HashSet Char
uniqFreqs = hmap >>> Map.elems >>> Set.fromList

antiNodes :: (Position, Position) -> HashSet Position
antiNodes (p1, p2) = Set.fromList [p2 `add` (p1 `fromTo` p2), p1 `add` (p2 `fromTo` p1)]

antiNodesLine :: CharMap -> (Position, Position) -> HashSet Position
antiNodesLine m (p, q) = Set.fromList $ lpq ++ lqp
  where
    dpq = p `fromTo` q
    dqp = q `fromTo` p

    lpq = [0..] & (map (mult dpq) >>> map (add q) >>> takeWhile withinLimits)
    lqp = [0..] & (map (mult dqp) >>> map (add p) >>> takeWhile withinLimits)

    withinLimits :: Position -> Bool
    withinLimits (x, y) = 0 <= x && x < width m && 0 <= y && y < height m


pairs :: [Position] -> HashSet (Position, Position)
pairs [] = Set.empty
pairs [_] = Set.empty
pairs (p : pp) = Set.fromList (map (p,) pp) `Set.union` pairs pp

add :: Position -> Position -> Position
add (px, py) (qx, qy) = (qx + px, qy + py)

mult :: Position -> Int -> Position
mult (x, y) n = (n * x, n * y)

fromTo :: Position -> Position -> Position
fromTo (px, py) (qx, qy) = (qx - px, qy - py)

type Position = (Int, Int)
data CharMap = CharMap
  { hmap :: HashMap Position Char
  , height :: Int
  , width :: Int
  }
  deriving (Show)

parseCharMap :: Parser CharMap
parseCharMap = do
  hmap <- Map.fromList . flatten . enumerate <$> many1' row
  return
    $ CharMap
      { hmap = Map.filter (/= '.') hmap
      , width = hmap & (Map.toList >>> map (fst >>> fst) >>> foldl1' max >>> (+) 1)
      , height = hmap & (Map.toList >>> map (fst >>> snd) >>> foldl1' max >>> (+) 1)
      }
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
