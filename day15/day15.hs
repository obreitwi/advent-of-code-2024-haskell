{-# LANGUAGE DeriveGeneric #-}
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
  putStr "Part 1 (debug, expect 2028): "
  either error part1 (parseOnly parseInput debug2) >>= print

  putStr "Part 1 (debug, expect 10092): "
  either error part1 (parseOnly parseInput debug) >>= print

  putStr "Part 1: (expect 1318523)"
  either error part1 (parseOnly parseInput input) >>= print

  putStr "Part 2 (debug, expect 9021): "
  either error part1 (transformPart2 >>> parseOnly parseInput $ debug) >>= print

  putStr "Part 2: "
  either error part1 (transformPart2 >>> parseOnly parseInput $ input) >>= print

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

debug :: T.Text
debug = decodeUtf8Lenient $(embedFileRelative "./debug")

debug2 :: T.Text
debug2 = decodeUtf8Lenient $(embedFileRelative "./debug2")

transformPart2 :: Text -> Text
transformPart2 = T.replace "#" "##" >>> T.replace "O" "[]" >>> T.replace "." ".." >>> T.replace "@" "@."

parseInput :: Parser Input
parseInput = parseGrid

part1 :: Input -> IO Int
part1 Input{inputEntities, instructions} =
  let finalGrid = execState (mapM_ handleInstruction instructions) (Grid{entities = inputEntities, current = inputEntities & robotPosition})
   in do
        -- [1 .. (length instructions)] & mapM_ (\n -> execState (mapM_ handleInstruction $ take n instructions) (Grid{entities = inputEntities, current = inputEntities & robotPosition}) & (\s -> printf "\nn = %d (%s):" n (show $ instructions !! (n - 1)) >> (entities s & printEntities)))
        printEntities $ entities finalGrid
        entities finalGrid & (score >>> return)

data Input = Input
  { inputEntities :: Entities
  , instructions :: [Instruction]
  }
  deriving (Show)

type Position = (Int, Int)

type Entities = HashMap Position Kind

data Instruction = GoUp | GoDown | GoLeft | GoRight deriving (Show, Eq)

parseInstruction :: Parser Instruction
parseInstruction = (char '^' $> GoUp) <|> (char 'v' $> GoDown) <|> (char '>' $> GoRight) <|> (char '<' $> GoLeft)

data Kind = Wall | Box | BoxLeft | BoxRight | Robot | Free deriving (Show, Eq, Generic)
instance Hashable Kind

parseGrid :: Parser Input
parseGrid = do
  rows <- many1' $ many1' parseKind <* endOfLine
  let inputEntities = enumerate2 rows & Map.fromList
  _ <- many1' endOfLine
  instructions <- many1' (parseInstruction <* optional endOfLine)
  return $ Input{inputEntities, instructions}

parseKind :: Parser Kind
parseKind = (char '#' $> Wall) <|> (char 'O' $> Box) <|> (char '@' $> Robot) <|> (char '.' $> Free) <|> (char '[' $> BoxLeft) <|> (char ']' $> BoxRight)

enumerate :: [a] -> [(Int, a)]
enumerate = go 0
 where
  go _ [] = []
  go i (l : ll) = (i, l) : go (i + 1) ll

enumerate2 :: [[a]] -> [(Position, a)]
enumerate2 = map enumerate >>> enumerate >>> concatMap go
 where
  go :: (Int, [(Int, a)]) -> [(Position, a)]
  go (j, l) = map (go' j) l
  go' j (i, e) = ((i, j), e)

move :: Instruction -> Position -> Position
move GoUp (x, y) = (x, y - 1)
move GoDown (x, y) = (x, y + 1)
move GoLeft (x, y) = (x - 1, y)
move GoRight (x, y) = (x + 1, y)

turnAround :: Instruction -> Instruction
turnAround GoUp = GoDown
turnAround GoDown = GoUp
turnAround GoLeft = GoRight
turnAround GoRight = GoLeft

data Grid = Grid
  { entities :: Entities
  , current :: Position
  }
  deriving (Show)

handleInstruction :: Instruction -> State Grid ()
handleInstruction i = checkPathFree i >>= go
 where
  go :: (HashSet (Position, Kind), Bool) -> State Grid ()
  go (_, False) = return ()
  go (targets, True) = shiftBoxes targets i

modifyEntities :: (Entities -> Entities) -> State Grid ()
modifyEntities f = modify $ \s -> s{entities = s & (entities >>> f)}

-- move all boxes along the path from current position
-- updates current position of robot
shiftBoxes :: HashSet (Position, Kind) -> Instruction -> State Grid ()
shiftBoxes targets i = do
  targets & (Set.map fst >>> Set.toList >>> mapM_ (\p -> modifyEntities $ Map.insert p Free))
  targets & (Set.map (first $ move i) >>> Set.toList >>> mapM_ (\(p, k) -> modifyEntities $ Map.insert p k))
  modify $ \s -> s{current = move i (current s)}

-- check if path in given direction is free (True) or blocked (False)
checkPathFree :: Instruction -> State Grid (HashSet (Position, Kind), Bool)
checkPathFree i = gets current >>= checkPath i

checkPath :: Instruction -> Position -> State Grid (HashSet (Position, Kind), Bool)
checkPath i p = do
  k <- gets $ entities >>> Map.lookup p
  case k of
    Just Box -> checkPath i (move i p) >>= appendIfFree (p, Box)
    Just Wall -> return (Set.empty, False)
    Just Free -> return (Set.empty, True)
    Just Robot -> checkPath i (move i p) >>= appendIfFree (p, Robot)
    Just BoxLeft ->
      if i `elem` [GoLeft, GoRight]
        then checkPath i (move i p) >>= appendIfFree (p, BoxLeft)
        else (checkPath i (move i p) `checkBoth` checkPath i (move i (move GoRight p))) >>= appendIfFree (p, BoxLeft) >>= appendIfFree (move GoRight p, BoxRight)
    Just BoxRight ->
      if i `elem` [GoLeft, GoRight] -- only check boht if going up/down
        then checkPath i (move i p) >>= appendIfFree (p, BoxRight)
        else (checkPath i (move i p) `checkBoth` checkPath i (move i (move GoLeft p))) >>= appendIfFree (move GoLeft p, BoxLeft) >>= appendIfFree (p, BoxRight)
    Nothing -> error "ran out of map"
 where
  appendIfFree :: (Position, Kind) -> (HashSet (Position, Kind), Bool) -> State Grid (HashSet (Position, Kind), Bool)
  appendIfFree _ (_, False) = return (Set.empty, False)
  appendIfFree c (rest, True) = return (Set.insert c rest, True)

  checkBoth :: State Grid (HashSet (Position, Kind), Bool) -> State Grid (HashSet (Position, Kind), Bool) -> State Grid (HashSet (Position, Kind), Bool)
  checkBoth = liftM2 go
   where
    go :: (HashSet (Position, Kind), Bool) -> (HashSet (Position, Kind), Bool) -> (HashSet (Position, Kind), Bool)
    go (_, False) _ = (Set.empty, False)
    go _ (_, False) = (Set.empty, False)
    go (l, True) (r, True) = (l `Set.union` r, True)

robotPosition :: Entities -> Position
robotPosition = Map.toList >>> filter (snd >>> (== Robot)) >>> head >>> fst

printEntities :: Entities -> IO ()
printEntities e =
  let
    maxX = e & (Map.keys >>> map fst >>> foldl1' max)
    maxY = e & (Map.keys >>> map snd >>> foldl1' max)
   in
    putStrLn "" >> ([0 .. maxY] & mapM_ (printRow maxX))
 where
  printRow maxX y = mapM_ (\x -> Map.lookup (x, y) e & (putKind >>> putChar)) [0 .. maxX] >> putStrLn ""

  putKind :: Maybe Kind -> Char
  putKind (Just Free) = '.'
  putKind (Just Box) = 'O'
  putKind (Just Robot) = '@'
  putKind (Just Wall) = '#'
  putKind (Just BoxLeft) = '['
  putKind (Just BoxRight) = ']'
  putKind Nothing = 'X'

score :: Entities -> Int
score = Map.filter ((== Box) &&& (== BoxLeft) >>> uncurry (||)) >>> Map.keys >>> map (\(x, y) -> x + 100 * y) >>> sum
