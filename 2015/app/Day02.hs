module Day02 (day02main) where

import Data.List (sort)
import Tools (splitBy, getFileLines)

parseDimensions :: String -> (Int, Int, Int)
parseDimensions input = output
    where
        stuff = splitBy 'x' input
        as_ints :: [Int]
        as_ints = map read stuff
        (x : y : z : _) = sort as_ints
        output = (x, y, z)


paperArea :: (Int, Int, Int) -> Int
paperArea (x, y, z) = 3 * x * y + 2 * y * z + 2 * z * x

parsePaperArea :: String -> Int
parsePaperArea input = (paperArea . parseDimensions) input

ribbonLength :: (Int, Int, Int) -> Int
ribbonLength (x, y, z) = (x * 2 + y * 2) + (x * y * z)

parseRibbonLength :: String -> Int
parseRibbonLength input = (ribbonLength . parseDimensions) input

totalArea :: [String] -> Int
totalArea inputLines = sum (map parsePaperArea inputLines)

totalRibbon :: [String] -> Int
totalRibbon inputLines = sum (map parseRibbonLength inputLines)

day02main :: IO ()
day02main = do
    stuff <- getFileLines "inputs/day_02_input"
    putStrLn $ "Total paper area: " ++ show (totalArea stuff)
    putStrLn $ "Total ribbon length: " ++ show (totalRibbon stuff)
