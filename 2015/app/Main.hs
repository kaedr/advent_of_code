module Main (main) where

import Data.List.NonEmpty qualified as NE
import Day01 (day01main)
import Day02 (day02main)
import Day03 (day03main)
import Day04 (day04main)
import Day05 (day05main)
import Day06 (day06main)
import Day06Arrays (day06Amain)
import Day07 (day07main)
import Day08 (day08main)
import Day09 (day09main)
import Day10 (day10main)
import Day11 (day11main)
import Day12 (day12main)
import System.Environment (getArgs)

dispatch :: String -> IO ()
dispatch "1" = day01main
dispatch "2" = day02main
dispatch "3" = day03main
dispatch "4" = day04main
dispatch "5" = day05main
dispatch "6" = day06main
dispatch "66" = day06Amain
dispatch "7" = day07main
dispatch "8" = day08main
dispatch "9" = day09main
dispatch "10" = day10main
dispatch "11" = day11main
dispatch "12" = day12main
dispatch input = putStr $ "No function for day: " ++ input

main :: IO ()
main = do
  args <- getArgs
  case NE.nonEmpty args of
    Nothing -> do
      x <- getLine
      dispatch x
    Just argsHere -> do
      putStrLn ""
      putStrLn $ "Running Day " ++ NE.head argsHere
      dispatch (NE.head argsHere)
      putStrLn ""

  putStrLn "Done!"
