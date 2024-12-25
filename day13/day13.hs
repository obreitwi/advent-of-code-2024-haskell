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
import Numeric.LinearAlgebra qualified as LA
import Numeric.LinearAlgebra.Data qualified as LAD
import Numeric.LinearAlgebra.Data ((><))

main :: IO ()
main = do
  putStrLn "Part 1 (debug, expect 480): "
  either error (print . part1) $ parseOnly parseInput debug
  either error (print . part1) $ parseOnly parseInput input

  putStrLn "Part 2 (debug, expect ???): "
  either error (mapM_ print) $ parseOnly parseInput debug
  either error (print . part2) $ parseOnly parseInput debug
  either error (print . part2) $ parseOnly parseInput input

input :: T.Text
input = decodeUtf8Lenient $(embedFileRelative "./input")

debug :: T.Text
debug = decodeUtf8Lenient $(embedFileRelative "./debug")

parseInput :: Parser [Claw]
parseInput = parseClaw `sepBy1'` many1' endOfLine

part1 :: [Claw] -> Int
-- part1 = mapMaybe bruteForce >>> foldl' (+) 0
part1 = mapMaybe (dup >>> second solveClaw >>> uncurry checkSolution) >>> foldl' (+) 0

part2 :: [Claw] -> Int
part2 = mapMaybe (adaptPart2 >>> dup >>> second solveClaw >>> uncurry checkSolution) >>> foldl' (+) 0

data Claw = Claw
  { buttonA :: Button
  , buttonB :: Button
  , prize :: Prize
  }
  deriving (Show)

data Button = Button {x :: Int, y :: Int}
  deriving (Show, Eq)

type Prize = Button

shiftPart2 :: Int
shiftPart2 = 10000000000000

adaptPart2 :: Claw -> Claw
adaptPart2 c =
  c
    { prize = Button{x = (prize >>> x) c + shiftPart2, y = (prize >>> y) c + shiftPart2}
    }

parseClaw :: Parser Claw
parseClaw = do
  buttonA <- parseButton 'A'
  buttonB <- parseButton 'B'
  prize <- parsePrize
  return $ Claw{buttonA, buttonB, prize}

parseButton :: Char -> Parser Button
parseButton c = do
  x <- "Button " *> char c *> ": X+" *> decimal
  y <- ", Y+" *> decimal <* endOfLine
  return $ Button x y

parsePrize :: Parser Button
parsePrize = do
  x <- "Prize: X=" *> decimal
  y <- ", Y=" *> decimal
  return $ Button{x, y}

bruteForce :: Claw -> Maybe Int
bruteForce c = foldl' maybeMin Nothing $ mapMaybe (checkSolution c) [0 .. maxTries]
 where
  maybeMin :: Maybe Int -> Int -> Maybe Int
  maybeMin Nothing r = Just r
  maybeMin (Just l) r = Just $ l `min` r

maxTries :: Int
maxTries = 100

-- given two buttons and a number of application of the first button, check whether prize is attainable and return total costs
checkSolution :: Claw -> Int -> Maybe Int
checkSolution c numApplyA = go $ ((prize >>> x) c - (buttonA >>> x) c * numApplyA) `div` (buttonB >>> x) c
 where
  go :: Int -> Maybe Int
  go neededApplyB
    | -- trace (printf "A x %d B x %d -> %s" numApplyA neededApplyB (show . applyB $ neededApplyB)) $
      applyB neededApplyB == prize c =
        Just $ 3 * numApplyA + neededApplyB
    | otherwise = Nothing

  applyB :: Int -> Button
  applyB numApplyB =
    Button
      { x = (buttonA >>> x) c * numApplyA + (buttonB >>> x) c * numApplyB
      , y = (buttonA >>> y) c * numApplyA + (buttonB >>> y) c * numApplyB
      }

dup :: a -> (a, a)
dup a = (a, a)

solveClaw :: Claw -> Int
solveClaw c = round $ solution `LA.atIndex` 0
  where
    solution = LA.cgSolve False (
      LA.mkDense $ (2><2) (sequenceA [
        (buttonA >>> x >>> fromIntegral), (buttonB >>> x >>> fromIntegral),
        (buttonA >>> y >>> fromIntegral), (buttonB >>> y >>> fromIntegral)
      ] c)) (fromList $ sequenceA [prize >>> x >>> fromIntegral, prize >>> y >>> fromIntegral] c)

