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
  putStr "Debug: "
  print $ parseOnly parseInput "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  putStr "Part 1: "
  either error (print . part1) $ parseOnly parseInput input
  putStr "Part 2: "
  either error (print . part2) $ parseOnly parseInput input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

part1 :: [Operation] -> Int
part1 = sum . map go
 where
  go (Mul l r) = l * r
  go _ = 0

part2 :: [Operation] -> Int
part2 = go 0 True
 where
  go total _ [] = total
  go total _ (Do : ops) = go total True ops
  go total _ (Dont : ops) = go total False ops
  go total False (Mul _ _ : ops) = go total False ops
  go total True (Mul l r : ops) = go (total + l*r) True ops

-- part2 :: [[Int]] -> Int
-- part2 = length . filter (\l -> drop1Valid isValidAscending l || drop1Valid isValidDescending l)

data Operation = Mul Int Int | Do | Dont deriving (Show)

parseInput :: Parser [Operation]
parseInput = catMaybes <$> many1' parseOperation

parseOperation :: Parser (Maybe Operation)
parseOperation = choice [Just <$> parseDo, Just <$> parseDont, Just <$> parseMul, anyChar $> Nothing]

parseDo :: Parser Operation
parseDo = string "do()" $> Do

parseDont :: Parser Operation
parseDont = string "don't()" $> Dont

parseMul :: Parser Operation
parseMul = do
  _ <- string "mul("
  l <- rangeDigitNumber 1 3
  _ <- char ','
  r <- rangeDigitNumber 1 3
  _ <- char ')'
  return $ Mul l r

rangeDigitNumber :: Int -> Int -> Parser Int
rangeDigitNumber from to = choice $ map fixedDigitNumber . reverse $ [from .. to]

fixedDigitNumber :: Int -> Parser Int
fixedDigitNumber n = foldl1' (\l r -> l * 10 + r) . map digitToInt <$> count n digit
