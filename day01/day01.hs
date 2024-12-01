{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFileRelative)

input :: BS.ByteString
input = $(embedFileRelative "./input")

main :: IO ()
main = do
   TIO.putStrLn "Hello, Advent of Code!"
   let numBytes = BS.length input
   putStrLn $ "First input is " ++ show numBytes ++ " bytes long."

