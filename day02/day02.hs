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
  either error (print . part1) $ parseOnly parseInput input
  putStr "Part 2: "
  either error (print . part2) $ parseOnly parseInput input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

part1 :: [[Int]] -> Int
part1 = length . filter (\l -> isValidAscending l || isValidDescending l)

part2 :: [[Int]] -> Int
part2 = length . filter (\l -> drop1Valid isValidAscending l || drop1Valid isValidDescending l)

isValidAscending :: [Int] -> Bool
isValidAscending l = all (\e -> e > 0 && e < 4) $ zipWith (-) (drop 1 l) l

isValidDescending :: [Int] -> Bool
isValidDescending l = all (\e -> e < 0 && e > -4) $ zipWith (-) (drop 1 l) l

dropNth :: [Int] -> Int -> [Int]
dropNth [] _ = []
dropNth (_ : ll) 1 = ll
dropNth (l : ll) n = l : dropNth ll (n - 1)

drop1Valid :: ([Int] -> Bool) -> [Int] -> Bool
drop1Valid isValid l = any (isValid . dropNth l) [1 .. length l]

parseInput :: Parser [[Int]]
parseInput = (decimal `sepBy1'` char ' ') `sepBy1'` endOfLine
