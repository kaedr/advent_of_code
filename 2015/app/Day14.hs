module Day14 (day14main) where

import Data.Function (on)
import Data.List (group, groupBy, sort, sortOn)
import Tools (getFileLines, splitBy)

performanceFrom :: String -> (String, Int, Int, Int)
performanceFrom input = case splitBy ' ' input of
  [name, "can", "fly", speed, "km/s", "for", duration, "seconds,", "but", "then", "must", "rest", "for", recoveryTime, "seconds."] -> (name, read speed, read duration, read recoveryTime)
  _ -> error ("Bad Input Data: " ++ input)

distanceIn :: Int -> (String, Int, Int, Int) -> Int
distanceIn time (name, speed, duration, recoveryTime)
  | time > duration + recoveryTime = speed * duration + distanceIn (time - duration - recoveryTime) (name, speed, duration, recoveryTime)
  | time > duration = speed * duration
  | otherwise = speed * time

leadersAt :: Int -> [(String, Int, Int, Int)] -> [String]
leadersAt time stats =
  let distances = map (distanceIn time) stats
      scores = sortOn snd (zip (map (\(name, _, _, _) -> name) stats) distances)
      groupings = groupBy ((==) `on` snd) scores
   in map fst (last groupings)

leadersList :: Int -> [(String, Int, Int, Int)] -> [String]
leadersList 0 _ = []
leadersList time stats = leadersAt time stats ++ leadersList (time - 1) stats

day14main :: IO ()
day14main = do
  input <- getFileLines "inputs/day_14_input"
  let stats = map performanceFrom input
  let outcomes = map (distanceIn 2503) stats
  putStrLn $ "Outcomes: " ++ show outcomes
  putStrLn $ "Best: " ++ show (maximum outcomes)

  let newScoring = map length (group (sort (leadersList 2503 stats)))
  putStrLn $ "New Outcomes: " ++ show newScoring
  putStrLn $ "New Score: " ++ show (maximum newScoring)
