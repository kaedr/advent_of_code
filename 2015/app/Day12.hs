module Day12 (day12main) where

import Data.Aeson (Value (Array, Number, Object, String), decode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL
import Data.Scientific (Scientific)
import Data.Text qualified as T
import Text.Regex.Posix
import Tools (safeHead)

intRegex :: String
intRegex = "-?[0-9]+"

sumWith :: (Foldable t, Num c, Functor t) => (a -> c) -> t a -> c
sumWith f = sum . fmap f

jSum :: Value -> Scientific
jSum (Number n) = n
jSum (Array arr) = sumWith jSum arr
jSum (Object obj) = if objHasRed obj then sumWith jSum $ KM.elems obj else 0
jSum _ = 0

objHasRed :: KM.KeyMap Value -> Bool
objHasRed = not . any (\(k, v) -> k == Key.fromString "red" || valAsString v == T.pack "red") . KM.toList

valAsString :: Value -> T.Text
valAsString (String s) = s
valAsString other = T.pack (show other)

day12main :: IO ()
day12main =
  do
    fullMonty <- readFile "inputs/day_12_input"
    let allTheNumstrs = fullMonty =~ intRegex :: [[String]]
    let allTheNums = map (read . safeHead) allTheNumstrs :: [Int]
    putStrLn $ "Sum: " ++ show (sum allTheNums)
    jsonData <- decode <$> BL.readFile "inputs/day_12_input"
    case jsonData of
      Just jDat -> putStrLn $ "Total: " ++ show (jSum jDat)
      Nothing -> error "BAD JSON!"
