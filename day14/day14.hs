{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Attoparsec.Text hiding (take)
import Data.FileEmbed (embedFileRelative)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Rebase.Prelude hiding (IntMap, check, left, matchM, matchS, right, rotate, takeWhile)
import Prelude ()

main :: IO ()
main = do
  putStr "Part 1 (debug, expect 12): "
  either error (print . part1 (Grid{width = 11, height = 7})) $ parseOnly parseInput debug

  putStr "Part 1: "
  either error (print . part1 (Grid{height = 103, width = 101} )) $ parseOnly parseInput input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

debug :: T.Text
debug = decodeUtf8Lenient $(embedFileRelative "./debug")

parseInput :: Parser [Robot]
parseInput = many1' parseRobot

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq)

data Robot = Robot
  { pos :: Point
  , vel :: Point
  }
  deriving (Show)

data Grid = Grid {width :: Int, height :: Int}

parsePoint :: Parser Point
parsePoint = do
  x <- signed decimal
  y <- "," *> signed decimal
  return $ Point{x, y}

parseRobot :: Parser Robot
parseRobot = do
  pos <- "p=" *> parsePoint
  vel <- " v=" *> parsePoint <* endOfLine
  return $ Robot{pos, vel}

part1 :: Grid -> [Robot] -> Int
part1 grid =
  evolve grid 100 -- >>> traceWith (map (pos >>> show) >>> intercalate "\n")
    >>> quadrants grid

evolve :: Grid -> Int -> [Robot] -> [Robot]
evolve g n = map go
 where
  go r =
    r
      { pos =
          Point
            { x = ((pos >>> x) r + n * (vel >>> x) r) `posMod` width g
            , y = ((pos >>> y) r + n * (vel >>> y) r) `posMod` height g
            }
      }

posMod :: (Integral a) => a -> a -> a
posMod x m
  | x' < 0 = x' + m
  | otherwise = x'
 where
  x' = x `mod` m

quadrants :: Grid -> [Robot] -> Int
quadrants g r = [topLeft, topRight, bottomLeft, bottomRight] <*> pure r & (map length >>> traceShowId >>> foldl' (*) 1)
 where
  h2 = height g `div` 2
  w2 = width g `div` 2

  topLeft :: [Robot] -> [Robot]
  topLeft = filter (pos >>> x >>> (< w2)) >>> filter (pos >>> y >>> (< h2)) -- >>> traceQuadrant "topLeft"

  topRight :: [Robot] -> [Robot]
  topRight = filter (pos >>> x >>> (> w2)) >>> filter (pos >>> y >>> (< h2)) -- >>> traceQuadrant "topRight"

  bottomLeft :: [Robot] -> [Robot]
  bottomLeft = filter (pos >>> x >>> (< w2)) >>> filter (pos >>> y >>> (> h2)) -- >>> traceQuadrant "bottomLeft"

  bottomRight :: [Robot] -> [Robot]
  bottomRight = filter (pos >>> x >>> (> w2)) >>> filter (pos >>> y >>> (> h2)) -- >>> traceQuadrant "bottomRight"

  {-
   - traceQuadrant :: String -> [Robot] -> [Robot]
   - traceQuadrant lbl = traceWith (\x -> lbl ++ ":\n" ++ (map (pos >>> show) x & intercalate "\n"))
   -}
