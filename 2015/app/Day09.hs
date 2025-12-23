module Day09 (day09main) where

import Data.List (permutations)
import Data.Map qualified as M (Map, fromList, lookup)
import Data.Set qualified as DS
import Debug.Trace (traceShowId)
import Tools (getFileLines, splitBy)

edgeFrom :: String -> ((String, String), Int)
edgeFrom input = case splitBy ' ' input of
  [left, "to", right, "=", dist] -> ((left, right), read dist)
  _ -> error ("Bad Edge Data: " ++ input)

smartLookup :: (String, String) -> M.Map (String, String) Int -> Int
smartLookup (a, b) distances = case (M.lookup (a, b) distances, M.lookup (b, a) distances) of
  (Just dist, Nothing) -> dist
  (Nothing, Just dist) -> dist
  (left, right) -> error ("Wat?! (" ++ show left ++ ") (" ++ show right ++ ")")

travelList :: M.Map (String, String) Int -> [String] -> Int
travelList distances (a : b : rest) = smartLookup (a, b) distances + travelList distances (b : rest)
travelList _ _ = 0

day09main :: IO ()
day09main = do
  stuff <- getFileLines "inputs/day_09_input"
  let edgeList = map edgeFrom stuff
  let (left, right) = unzip (map fst edgeList)
  let cityList = DS.toList (DS.fromList left `DS.union` DS.fromList right)
  -- putStrLn $ "Cities: " ++ show cityList
  let cityPermutations = permutations cityList
  let edgeMap = M.fromList edgeList
  -- putStrLn $ "Edges: " ++ show edgeMap
  let travelWithMap = travelList edgeMap
  let allDistances = map (traceShowId . travelWithMap) cityPermutations

  putStrLn $ "Min Distance: " ++ show (minimum allDistances) ++ " & Max Distance: " ++ show (maximum allDistances)
