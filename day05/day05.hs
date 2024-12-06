{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Attoparsec.Text hiding (take)
import Data.FileEmbed (embedFileRelative)
import Data.List.Split (splitOn, splitWhen)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Rebase.Prelude hiding (left, matchM, matchS, right, takeWhile)
import Prelude ()

main :: IO ()
main = do
  putStr "Part 1 (debug, expect 143): "
  either error (print . part1) $ parseOnly parseInput debug

  putStr "part 1: "
  either error (print . part1) $ parseOnly parseInput input

  putStr "Part 2 (debug, expect 123): "
  either error (print . part2) $ parseOnly parseInput debug
  putStr "Part 2: "
  either error (print . part2) $ parseOnly parseInput input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

debug :: T.Text
debug = decodeUtf8Lenient $(embedFileRelative "./debug")

parseInput :: Parser Input
parseInput = do
  constraints <- Set.fromList <$> constraint `sepBy1'` endOfLine
  _ <- endOfLine <* endOfLine
  updates <- update `sepBy1'` endOfLine
  _ <- endOfLine
  return $ Input constraints updates

constraint :: Parser Constraint
constraint = (,) <$> (decimal <* char '|') <*> decimal

update :: Parser Update
update = decimal `sepBy1'` char ','

data Input = Input (Set Constraint) [Update] deriving (Show)
type Update = [Int]
type Constraint = (Int, Int)

part1 :: Input -> Int
part1 (Input constraints updates) = updates & (filter (checkUpdate constraints) >>> map (\u -> u !! (length u `div` 2)) >>> sum)

checkUpdate :: Set Constraint -> Update -> Bool
checkUpdate s u = all (checkSingle s u) u

-- check for violated constraints
checkSingle :: Set Constraint -> Update -> Int -> Bool
checkSingle s u toCheck = all (\b -> (toCheck, b) `Set.notMember` s) before && all (\a -> (a, toCheck) `Set.notMember` s) after
 where
  before = head $ splitOn [toCheck] u
  after = splitOn [toCheck] u !! 1

part2 :: Input -> Int
part2 (Input constraints updates) = updates & (filter (not . checkUpdate constraints) >>> map (fixSorting constraints) >>> map (\u -> u !! (length u `div` 2)) >>> sum)

fixSorting :: Set Constraint -> Update -> Update
fixSorting constraints = go []
 where
   go :: Update -> Update -> Update
   go sorted [] = sorted
   go sorted (t : toSort) =
     let
       smllr = smaller sorted t
     in
       go (smllr ++ (t : drop (length smllr) sorted)) toSort

   smaller sorted t = head $ splitWhen (\s -> (t, s) `Set.member` constraints) sorted

-- go (s:sorted) (t:toSort) | (s, t) `Set.member` constraints =

