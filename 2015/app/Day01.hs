module Day01 (day01main) where

import Data.List.NonEmpty qualified as NE
import Tools (getFileLines, nonEmptyOrError)

munch :: String -> Int
munch "" = 0
munch ('(' : t) = 1 + munch t
munch (')' : t) = -1 + munch t
munch (_ : t) = munch t

munchier :: String -> (Int, Int) -> (Int, Int)
munchier "" (pos, val) = (pos, val)
munchier ('(' : t) (pos, val) = munchier t (pos + 1, val + 1)
munchier (')' : t) (pos, val)
  | val < 0 = (pos, val)
  | otherwise = munchier t (pos + 1, val - 1)
munchier w (t, f) = error ("Badmunchier " ++ w ++ " (" ++ show t ++ "," ++ show f ++ ")")

day01main :: IO ()
day01main = do
  stuff <- getFileLines "inputs/day_01_input"
  let stuffHere = nonEmptyOrError stuff
  putStrLn $ "Final Floor: " ++ show (munch (NE.head stuffHere))
  putStrLn $ "Position of basement: " ++ show (munchier (NE.head stuffHere) (0, 0))
