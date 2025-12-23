module Day10 (day10main) where

takeAndCount :: String -> [(Int, Char)]
takeAndCount (ch : rest) = (count, ch) : takeAndCount whatsLeft
  where
    count = length (takeWhile (== ch) (ch : rest))
    whatsLeft = dropWhile (== ch) (ch : rest)
takeAndCount [] = []

rebuildStr :: [(Int, Char)] -> String
rebuildStr ((reps, ch) : rest) = show reps ++ [ch] ++ rebuildStr rest
rebuildStr [] = ""

doReps :: (Int, String) -> (Int, String)
doReps (0, input) = (0, input)
doReps (reps, input) = doReps (reps - 1, rebuildStr (takeAndCount input))

day10main :: IO ()
day10main = do
  let (_, partOne) = doReps (40, "1113222113")
  putStrLn $ "Part 1: " ++ show (length partOne)
  let (_, partTwo) = doReps (50, "1113222113")
  putStrLn $ "Part 1: " ++ show (length partTwo)
