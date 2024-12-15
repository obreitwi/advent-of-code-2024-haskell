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
  putStr "Part 1 (debug, expect 55312): "
  -- either error (print . part1) $ parseOnly parseInput $ debug
  print . part1 $ [125, 17]

  putStr "part 1: "
  either error (print . part1) $ parseOnly parseInput input

  putStr "part 2 (debug, expect 81): "
  -- either error (print . part2) $ parseOnly parseInput debug
  print . part2 $ [125, 17]

  putStr "part 2: "
  either error (print . part2) $ parseOnly parseInput input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

debug :: T.Text
debug = decodeUtf8Lenient $(embedFileRelative "./debug")

type Stone = Int

parseInput :: Parser [Stone]
parseInput = decimal `sepBy1'` (void space <|> void endOfInput)

evolve :: [Stone] -> [Stone]
evolve = concatMap go
 where
  go :: Stone -> [Stone]
  go 0 = [1]
  go n
    | even len = [read $ take (len `div` 2) asChar, asChar & (drop (len `div` 2) >>> dropWhile (== '0') >>> readMaybe >>> fromMaybe 0)]
    | otherwise = [n * 2024]
   where
    asChar = show n
    len = length asChar

part1 :: [Stone] -> Int
part1 = evolveHashN 25

part2 :: [Stone] -> Int
part2 = evolveHashN 75

evolveN :: Int -> [Stone] -> Int
evolveN 0 = length
evolveN n =  evolve >>> evolveN (n - 1)

evolveHashN :: Int -> [Stone] -> Int
evolveHashN numIterations = map (,1) >>> Map.fromList >>> go numIterations
  where
    go 0 = Map.foldl' (+) 0
    go n = Map.toList >>> concatMap evolveWithCount >>> Map.fromListWith (+) >>> go (n-1)

evolveWithCount :: (Stone, Int) -> [(Stone, Int)]
evolveWithCount (s, c) = evolve [s] & map (, c)


