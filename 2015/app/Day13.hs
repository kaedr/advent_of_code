module Day13 (day13main) where

import Data.List (nub, permutations)
import Data.Map qualified as M (Map, fromList, lookup)
import Tools (getFileLines, splitBy)

happinessFrom :: String -> ((String, String), Int)
happinessFrom input = case splitBy ' ' input of
  [left, "would", "gain", val, "happiness", "units", "by", "sitting", "next", "to", right] -> ((left, filter (/= '.') right), read val)
  [left, "would", "lose", val, "happiness", "units", "by", "sitting", "next", "to", right] -> ((left, filter (/= '.') right), -(read val))
  _ -> error ("Bad Input Data: " ++ input)

feelsLookup :: (String, String) -> M.Map (String, String) Int -> Int
feelsLookup (l, r) modMap = case (M.lookup (l, r) modMap, M.lookup (r, l) modMap) of
  (Just a, Just b) -> a + b
  (_, _) -> error ("Failed on: " ++ l ++ " & " ++ r)

sumOfAllFeels :: M.Map (String, String) Int -> String -> [String] -> Int
sumOfAllFeels modMap first (l : r : others) = total
  where
    root = if first == "" then l else first
    subTotal = sumOfAllFeels modMap root (r : others)
    total = feelsLookup (l, r) modMap + subTotal
sumOfAllFeels modMap first [l] = feelsLookup (l, first) modMap
sumOfAllFeels _ _ [] = error "Shouldn't have come here..."

day13main :: IO ()
day13main = do
  input <- getFileLines "inputs/day_13_input"
  let modifiers = map happinessFrom input
  let guestList = nub (map (fst . fst) modifiers)
  let modMap = M.fromList modifiers
  let allSeatings = permutations guestList
  let totals = map (sumOfAllFeels modMap "") allSeatings

  putStrLn $ "Max happiness: " ++ show (maximum totals)

  let part2mods = modifiers ++ concatMap (\guest -> [(("Me", guest), 0), ((guest, "Me"), 0)]) guestList
  let part2guests = "Me" : guestList
  let part2modMap = M.fromList part2mods
  let part2Seatings = permutations part2guests
  let part2Totals = map (sumOfAllFeels part2modMap "") part2Seatings

  putStrLn $ "Part 2 Max happiness: " ++ show (maximum part2Totals)
