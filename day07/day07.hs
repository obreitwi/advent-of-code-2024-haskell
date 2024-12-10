{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Attoparsec.Text hiding (take)
import Data.FileEmbed (embedFileRelative)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Rebase.Prelude hiding (check, left, matchM, matchS, right, rotate, takeWhile)
import Prelude ()

main :: IO ()
main = do
  putStr "Part 1 (debug, expect 3749): "
  either error (print . part1) $ parseOnly parseInput debug

  putStr "part 1: "
  either error (print . part1) $ parseOnly parseInput input

  putStr "Part 2 (debug, expect 11387): "
  either error (print . part2) $ parseOnly parseInput debug

  putStr "Part 2: "
  either error (print . part2) $ parseOnly parseInput input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

debug :: T.Text
debug = decodeUtf8Lenient $(embedFileRelative "./debug")

data Equation = Equation Int [Int] deriving (Show)
result :: Equation -> Int
result (Equation r _) = r

items :: Equation -> [Int]
items (Equation _ i) = i

parseInput :: Parser [Equation]
parseInput = (Equation <$> (decimal <* char ':' <* char ' ') <*> (decimal `sepBy1'` char ' ')) `sepBy1'` endOfLine

part1 :: [Equation] -> Int
part1 = filter check >>> map result >>> sum

part2 :: [Equation] -> Int
part2 = filter check2 >>> map result >>> sum

check :: Equation -> Bool
check eq = go (result eq) 0 (items eq)
 where
  go :: Int -> Int -> [Int] -> Bool
  go target current [] = target == current
  go target current _ | current > target = False
  go target current (i : ii) = go target (current + i) ii || go target (current * i) ii

check2 :: Equation -> Bool
check2 eq = go (result eq) 0 (items eq)
 where
  go :: Int -> Int -> [Int] -> Bool
  go target current [] = target == current
  go target current _ | current > target = False
  go target current (i : ii) = go target (current + i) ii || go target (current * i) ii || go target (read $ show current ++ show i) ii
