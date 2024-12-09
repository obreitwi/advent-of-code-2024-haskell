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
import Rebase.Prelude hiding (left, matchM, matchS, right, rotate, takeWhile)
import Prelude ()

main :: IO ()
main = do
  putStr "Part 1 (debug, expect 41): "
  either error (print . part1) $ parseOnly parseInput debug

  putStr "part 1: "
  either error (print . part1) $ parseOnly parseInput input

  putStr "Part 2 (debug, expect 6): "
  either error (print . part2) $ parseOnly parseInput debug

  putStr "Part 2: "
  either error (print . part2) $ parseOnly parseInput input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

debug :: T.Text
debug = decodeUtf8Lenient $(embedFileRelative "./debug")

data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Show, Eq, Ord)

type Position = (Int, Int)
type CharMap = Map Position Char

parseInput :: Parser CharMap
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
part1 m = Set.size . fst $ walked
 where
  (start, updated) = extractStartingPos m
  walked = walk updated start DirUp

part2 :: CharMap -> Int
part2 original =
  toCheck
    & ( Set.map (\c -> walk (Map.insert c '#' grid) start DirUp)
          >>> Set.filter (snd >>> not)
          >>> Set.size
      )
 where
  (start, grid) = extractStartingPos original

  toCheck :: Set Position
  toCheck = Set.map fst . Set.filter (\(p, _) -> p /= start) . fst $ walk grid start DirUp

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

-- returns set of walked tiles and whether or not we left the grid
walk :: CharMap -> Position -> Direction -> (Set (Position, Direction), Bool)
walk grid start dir = go (Set.singleton (start, dir)) start dir
 where
  lu :: Position -> Maybe Char
  lu = flip Map.lookup grid

  go :: Set (Position, Direction) -> Position -> Direction -> (Set (Position, Direction), Bool)
  go walked currentPos d
    | isNothing nextChar = (walked, True)
    | nextChar == Just '#' = go walked currentPos (rotate d)
    | (next, d) `Set.notMember` walked = go (Set.insert (next, d) walked) next d
    | (next, d) `Set.member` walked = (walked, False)
    | otherwise = undefined
   where
    next = step d currentPos
    nextChar = lu next

step :: Direction -> Position -> Position
step DirUp (x, y) = (x, y - 1)
step DirDown (x, y) = (x, y + 1)
step DirLeft (x, y) = (x - 1, y)
step DirRight (x, y) = (x + 1, y)
