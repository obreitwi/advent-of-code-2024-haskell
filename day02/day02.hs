{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Attoparsec.Text hiding (take)
import Data.FileEmbed (embedFileRelative)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Rebase.Prelude hiding (left, right, takeWhile)
import Prelude ()

main :: IO ()
main = do
  putStr "Part 1: "
  either error (print . part1) $ parseOnly parseInput1 input
  putStr "Part 2: "
  either error (print . part2) $ parseOnly parseInput1 input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

part1 :: ([Int], [Int]) -> Int
part1 (left, right) = sum $ map abs $ zipWith (-) (sort left) (sort right)

part2 :: ([Int], [Int]) -> Int
part2 (left, right) = sum $ map (\l -> l * length (filter (l ==) right)) left

parseInput1 :: Parser ([Int], [Int])
parseInput1 = do
  pairs <- many1' $ parsePair <* endOfLine
  return . unzip $ pairs
  where
    parsePair :: Parser (Int, Int)
    parsePair = (,) <$> (decimal <* skipSpace) <*> decimal
