module Day11 (day11main) where

hasStraight :: String -> Bool
hasStraight (a : b : c : rest) = (succ a == b && succ b == c) || hasStraight (b : c : rest)
hasStraight _ = False

hasPairs :: String -> Bool -> Bool
hasPairs (a : b : rest) previous
  | a == b && previous = True
  | a == b = hasPairs rest True
  | otherwise = hasPairs (b : rest) previous
hasPairs _ _ = False

hasBad :: String -> Bool
hasBad = foldr (\a -> (||) (a `elem` ['i', 'o', 'l'])) False

isValid :: String -> Bool
isValid input = hasStraight input && hasPairs input False && not (hasBad input)

stringCrement :: String -> String
stringCrement input = case reverse input of
  [] -> []
  "z" -> "a"
  ('z' : rest) -> stringCrement (reverse rest) ++ "a"
  (ch : rest) -> reverse (succ ch : rest)

nextValid :: String -> String
nextValid input =
  let nextPW = stringCrement input
   in if isValid nextPW then nextPW else nextValid nextPW

day11main :: IO ()
day11main = do
  let first = nextValid "vzbxkghb"
  putStrLn $ "Next PW: " ++ first
  let seocnd = nextValid first
  putStrLn $ "Next PW: " ++ seocnd
