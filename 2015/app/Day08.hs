module Day08 (day08main) where

import Tools (getFileLines)

type StringCounts = (String, Int, Int, Int)

startNom :: String -> (Int, Int, Int)
startNom input =
  let (_, enc, chars, mem) = omNom (input, 0, 0, 0)
   in (enc, chars, mem)

omNom :: StringCounts -> StringCounts
omNom ("", enc, chars, mem) = ("", enc, chars, mem)
-- Closing Quote Branch
omNom ('"' : "", enc, chars, mem) = ("", enc + 3, chars + 1, mem)
-- Single Escapes
omNom ('\\' : '\\' : rest, enc, chars, mem) = omNom (rest, enc + 4, chars + 2, mem + 1)
omNom ('\\' : '"' : rest, enc, chars, mem) = omNom (rest, enc + 4, chars + 2, mem + 1)
-- Hex Escape
omNom ('\\' : 'x' : _ : _ : rest, enc, chars, mem) = omNom (rest, enc + 5, chars + 4, mem + 1)
-- Opening Quote Branch
omNom ('"' : rest, enc, chars, mem) = omNom (rest, enc + 3, chars + 1, mem)
-- Business as usual
omNom (_ : rest, enc, chars, mem) = omNom (rest, enc + 1, chars + 1, mem + 1)

sumTuples :: (Num a) => [(a, a, a)] -> (a, a, a)
sumTuples = foldl addTuples (0, 0, 0)
  where
    addTuples (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

day08main :: IO ()
day08main = do
  stuff <- getFileLines "inputs/day_08_input"
  let (enc, chars, mem) = sumTuples (map startNom stuff)
  putStrLn $ "Chars: " ++ show chars ++ " Mem: " ++ show mem ++ " Diff: " ++ show (chars - mem)
  putStrLn $ ": " ++ show enc ++ " Chars: " ++ show chars ++ " Diff: " ++ show (enc - chars)
