{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Attoparsec.Text hiding (take)
import Data.FileEmbed (embedFileRelative)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Rebase.Prelude hiding (left, matchM, matchS, right, takeWhile)
import Prelude ()

main :: IO ()
main = do
  putStr "Part 1: "
  either error (print . part1) $ parseOnly parseInput input
  putStr "Part 2 (debug, expect 9): "
  either error (print . part2) $ parseOnly parseInput debug
  putStr "Part 2: "
  either error (print . part2) $ parseOnly parseInput input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

debug :: T.Text
debug = decodeUtf8Lenient $(embedFileRelative "./debug")

parseInput :: Parser (Map (Int, Int) Char)
parseInput = Map.fromList . flatten . enumerate <$> many1' row
 where
  row :: Parser [(Int, Char)]
  row = enumerate <$> (many1' (notChar '\n') <* endOfLine)

  flatten :: [(Int, [(Int, Char)])] -> [((Int, Int), Char)]
  flatten = concatMap (\(i, l) -> map (\(j, c) -> ((i, j), c)) l)

  enumerate :: [a] -> [(Int, a)]
  enumerate = go 0
   where
    go _ [] = []
    go i (l : ll) = (i, l) : go (i + 1) ll

part1 :: Map (Int, Int) Char -> Int
part1 m = sum . map numMatches $ startingPos
 where
  startingPos :: [(Int, Int)]
  startingPos = map fst . filter ((== 'X') . snd) . Map.toList $ m

  numMatches :: (Int, Int) -> Int
  numMatches start = length . filter (\f -> f start) . map (findXMAS m) $ directions

part2 :: Map (Int, Int) Char -> Int
part2 m = length . filter matchMAS $ startingPos
 where
  startingPos :: [(Int, Int)]
  startingPos = map fst . filter ((== 'A') . snd) . Map.toList $ m

  matchMAS :: (Int, Int) -> Bool
  matchMAS (x, y) =
    lu (x, y)
      == Just 'A'
      && ( (lu (x - 1, y - 1) == Just 'M' && lu (x + 1, y + 1) == Just 'S') || (lu (x - 1, y - 1) == Just 'S' && lu (x + 1, y + 1) == Just 'M')
         )
      && ( (lu (x - 1, y + 1) == Just 'M' && lu (x + 1, y - 1) == Just 'S') || (lu (x - 1, y + 1) == Just 'S' && lu (x + 1, y - 1) == Just 'M')
         )

  lu :: (Int, Int) -> Maybe Char
  lu = flip Map.lookup m

directions :: [(Int, Int)]
directions =
  [ (0, 1)
  , (1, 0)
  , (1, 1)
  , (1, -1)
  , (0, -1)
  , (-1, 0)
  , (-1, -1)
  , (-1, 1)
  ]

findXMAS :: Map (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Bool
findXMAS m (directionX, directionY) (startX, startY) = matchX && matchM && matchA && matchS
 where
  getM :: (Int, Int) -> Char
  getM k = fromMaybe '?' $ Map.lookup k m

  matchX = getM (startX, startY) == 'X'
  matchM = getM (startX + directionX, startY + directionY) == 'M'
  matchA = getM (startX + 2 * directionX, startY + 2 * directionY) == 'A'
  matchS = getM (startX + 3 * directionX, startY + 3 * directionY) == 'S'
