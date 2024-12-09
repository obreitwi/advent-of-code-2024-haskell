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
import Rebase.Prelude hiding (left, matchM, matchS, right, takeWhile, rotate)
import Prelude ()

main :: IO ()
main = do
  putStr "Part 1 (debug, expect 41): "
  either error (print . part1) $ parseOnly parseInput debug

  putStr "part 1: "
  either error (print . part1) $ parseOnly parseInput input

  -- putStr "Part 2 (debug, expect 123): "
  -- either error (print . part2) $ parseOnly parseInput debug
  -- putStr "Part 2: "
  -- either error (print . part2) $ parseOnly parseInput input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

debug :: T.Text
debug = decodeUtf8Lenient $(embedFileRelative "./debug")

data Direction = DirUp | DirDown | DirLeft | DirRight deriving Show

type Position = (Int, Int)
type CharMap = Map Position Char

parseInput = parseCharSet

parseCharSet :: Parser CharMap
parseCharSet = Map.fromList . flatten . enumerate <$> many1' row
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

part1 :: CharMap -> Int
part1 m = Set.size walked
  where
    (start, updated) = extractStartingPos m
    walked = walk updated start DirUp

-- extract starting pos and adjust map
extractStartingPos :: CharMap -> (Position, CharMap)
extractStartingPos m = (startPos, updated)
  where
    startPos = Map.toList m & (filter (snd >>> (== '^')) >>> map fst >>> head)
    updated = Map.insert startPos '.' m

rotate :: Direction -> Direction
rotate DirUp = DirRight
rotate DirRight = DirDown
rotate DirDown = DirLeft
rotate DirLeft = DirUp

walk :: CharMap -> Position -> Direction -> Set Position
walk map start d = go (Set.singleton start) start d
  where
    lu :: Position -> Maybe Char
    lu = flip Map.lookup map

    go walked current d 
          | char == Nothing = walked
          | char == Just '#' = go walked current (rotate d) 
          | char == Just '.' = go (Set.insert next walked) next d
          | otherwise = undefined
      where
        next = step d current
        char = lu next

step :: Direction -> Position -> Position
step DirUp (x, y) = (x, y-1)
step DirDown (x, y) = (x, y+1)
step DirLeft (x, y) = (x-1, y)
step DirRight (x, y) = (x+1, y)
