module Day03 (day03main) where

import Data.List.NonEmpty qualified as NE
import Data.Set (fromList, size)
import Tools (Point (..), getFileLines, nonEmptyOrError, originPoint)

mapDirection :: Char -> Point Int
mapDirection '<' = Pt (-1) 0
mapDirection '>' = Pt 1 0
mapDirection '^' = Pt 0 1
mapDirection 'v' = Pt 0 (-1)
mapDirection ch = error ("Bad directional character" ++ [ch])

mapLocations :: String -> Point Int -> [Point Int]
mapLocations "" current = [current]
mapLocations (ch : rest) current =
  let change = mapDirection ch
   in current : mapLocations rest (current + change)

countLocations :: [Point Int] -> Int
countLocations locations = size (fromList locations)

mapCountLocations :: String -> Int
mapCountLocations input = countLocations (mapLocations input originPoint)

data Tracker = Tracker
  { santa :: Point Int,
    robo :: Point Int
  }
  deriving (Show)

originTracker :: Tracker
originTracker = Tracker originPoint originPoint

updateTracker :: Tracker -> Point Int -> Point Int -> Tracker
updateTracker current newS newR = Tracker (santa current + newS) (robo current + newR)

roboMapper :: String -> Tracker -> [Point Int]
roboMapper "" current = [santa current, robo current]
roboMapper (sCh : "") current = output
  where
    santaChange = mapDirection sCh
    updated = updateTracker current santaChange originPoint
    output = [santa updated]
roboMapper (sCh : rCh : rest) current = output
  where
    santaChange = mapDirection sCh
    roboChange = mapDirection rCh
    updated = updateTracker current santaChange roboChange
    output = santa current : robo current : roboMapper rest updated

roboCounter :: String -> Int
roboCounter input = countLocations (roboMapper input originTracker)

day03main :: IO ()
day03main = do
  stuff <- getFileLines "inputs/day_03_input"
  let stuffHere = nonEmptyOrError stuff
  let totalHouses = mapCountLocations (NE.head stuffHere)
  putStrLn $ "Total Houses: " ++ show totalHouses
  let roboTotal = roboCounter (NE.head stuffHere)
  putStrLn $ "Total Robo Houses: " ++ show roboTotal
