module Main (main) where

import Day01 (day01main)
import Day02 (day02main)
import Day03 (day03main)
import Day04 (day04main)
import Day05 (day05main)
import Day06 (day06main)

dispatch :: String -> IO ()
dispatch "1" = day01main
dispatch "2" = day02main
dispatch "3" = day03main
dispatch "4" = day04main
dispatch "5" = day05main
dispatch "6" = day06main
dispatch input = putStr $ "No function for day: " ++ input

main :: IO ()
main = do
    putStr $ "Enter a Day # to run: "
    dayNum <- getLine
    putStrLn $ ""
    putStrLn $ "Running Day " ++ dayNum
    dispatch dayNum
    putStrLn ""
