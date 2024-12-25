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
  either error (print . part1 (Grid{height = 103, width = 101})) $ parseOnly parseInput input

  either error (render (Grid{width = 11, height = 7})) $ parseOnly parseInput debug

  -- Part 2
  let grid = Grid{height = 103, width = 101}
  initRobots <- either error return $ parseOnly parseInput input
  -- interactiveRender grid 1 0 initRobots
  let (period, _) = findLoopPeriod grid initRobots
  -- interactiveRender grid (-1) period robots
  
  let steps =  initRobots & (iterate (evolve grid 1) >>> take period >>> enumerate >>> map (dup >>> first (snd >>> quadrants grid)) >>> foldl1' minByQuadrant)

  print . fst. snd $ steps

  -- render grid (snd . snd $ steps)

  -- either error (renderUntil grid (middleLineOccupied grid)) $ parseOnly parseInput input
  -- either error (renderWhen grid (isSymmetric grid)) $ parseOnly parseInput input
  -- either error (renderWhen grid (onlyOneQuadrant grid)) $ parseOnly parseInput input

-- either error (interactiveRender grid) $ parseOnly parseInput input

minByQuadrant :: (Int, (Int, [Robot])) -> (Int, (Int, [Robot])) -> (Int, (Int, [Robot]))
minByQuadrant l r | fst l <= fst r = l
                  | otherwise = r

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

debug :: T.Text
debug = decodeUtf8Lenient $(embedFileRelative "./debug")

parseInput :: Parser [Robot]
parseInput = many1' parseRobot

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq)
instance Hashable Point where
  hashWithSalt salt Point{x, y} = salt `hashWithSalt` x `hashWithSalt` y

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
quadrants g r = [topLeft, topRight, bottomLeft, bottomRight] <*> pure g <*> pure r & (map length >>> foldl' (*) 1)

onlyOneQuadrant :: Grid -> [Robot] -> Bool
onlyOneQuadrant g r = [topLeft, topRight, bottomLeft, bottomRight] <*> pure g <*> pure r & (map length >>> filter (== 0) >>> length >>> (>= 1))

topLeft :: Grid -> [Robot] -> [Robot]
topLeft g = filter (pos >>> x >>> (< width g `div` 2)) >>> filter (pos >>> y >>> (< height g `div` 2)) -- >>> traceQuadrant "topLeft"

topRight :: Grid -> [Robot] -> [Robot]
topRight g = filter (pos >>> x >>> (> width g `div` 2)) >>> filter (pos >>> y >>> (< height g `div` 2)) -- >>> traceQuadrant "topRight"

bottomLeft :: Grid -> [Robot] -> [Robot]
bottomLeft g = filter (pos >>> x >>> (< width g `div` 2)) >>> filter (pos >>> y >>> (> height g `div` 2)) -- >>> traceQuadrant "bottomLeft"

bottomRight :: Grid -> [Robot] -> [Robot]
bottomRight g = filter (pos >>> x >>> (> width g `div` 2)) >>> filter (pos >>> y >>> (> height g `div` 2)) -- >>> traceQuadrant "bottomRight"

render :: Grid -> [Robot] -> IO ()
render g r = [0 .. height g - 1] & (concatMap (\y -> [0 .. width g - 1] & (map (`renderSingle` y) >>> (++ "\n"))) >>> putStr)
 where
  present :: HashSet Point
  -- present = r & (map pos >>> filter (x >>> (/= width g `div` 2)) >>> filter (y >>> (/= height g `div` 2)) >>> Set.fromList)
  present = r & (map pos >>> Set.fromList)

  renderSingle :: Int -> Int -> Char
  renderSingle x y
    -- | x == width g `div` 2 = 'O'
    -- | y == height g `div` 2 = 'O'
    | Point{x, y} `Set.member` present = 'X'
    | otherwise = ' '

findLoopPeriod :: Grid -> [Robot] -> (Int, [Robot])
findLoopPeriod grid = go Set.empty 0
 where
  go :: HashSet [Point] -> Int -> [Robot] -> (Int, [Robot])
  go s n r
    | toPos r `Set.member` s = (n, r)
    | otherwise = go (Set.insert (toPos r) s) (n + 1) (evolve grid 1 r)

  toPos = map pos

renderWhen :: Grid -> ([Robot] -> Bool) -> [Robot] -> IO ()
renderWhen grid check = go 1
 where
  go :: Int -> [Robot] -> IO ()
  go n r = do
    let next = evolve grid 1 r
    if check next
      then do
        putStrLn $ "n = " ++ show n
        render grid next
        return ()
      else go (n + 1) next

middleLineOccupied :: Grid -> [Robot] -> Bool
middleLineOccupied grid = filter (pos >>> x >>> (== (width grid `div` 2))) >>> length >>> (> (height grid `div` 2))

interactiveRender :: Grid -> Int -> Int -> [Robot] -> IO ()
interactiveRender grid step = go
 where
  go :: Int -> [Robot] -> IO ()
  go n r = do
    putStrLn $ "n = " ++ show n
    render grid r
    localStep <- getLine >>= (readMaybe >>> return)
    case localStep of
      Just ls -> go (n + ls) (evolve grid ls r)
      Nothing -> go (n + step) (evolve grid step r)

isSymmetric :: Grid -> [Robot] -> Bool
isSymmetric grid robots = (map pos >>> filter hasSymmetricPartner >>> length) robots > 300
 where
  present :: HashSet Point
  present = robots & (map pos >>> Set.fromList)

  w2 :: Int
  w2 = width grid `div` 2 

  hasSymmetricPartner :: Point -> Bool
  hasSymmetricPartner Point{x, y} = x == w2 || Point { x = (x + w2) `mod` width grid, y } `Set.member` present

dup :: a -> (a, a)
dup a = (a, a)

enumerate :: [a] -> [(Int, a)]
enumerate = go 0
  where
  go _ [] = []
  go i (l : ll) = (i, l) : go (i + 1) ll
