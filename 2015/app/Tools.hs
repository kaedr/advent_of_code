module Tools (splitBy, nonEmptyOrError, getFileLines, Point (..), originPoint, safeHead, safeTail) where

import Data.List.NonEmpty qualified as NE

splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy delimiter (c : cs)
  | c == delimiter = "" : rest
  | otherwise = (c : safeHead rest) : safeTail rest
  where
    rest = splitBy delimiter cs

nonEmptyOrError :: [a] -> NE.NonEmpty a
nonEmptyOrError stuff =
  case NE.nonEmpty stuff of
    Nothing -> error "Empty Input!"
    Just stuffHere -> stuffHere

safeHead :: [c] -> c
safeHead = NE.head . nonEmptyOrError

safeTail :: [a] -> [a]
safeTail = NE.tail . nonEmptyOrError

getFileLines :: FilePath -> IO [String]
getFileLines fileName = do
  contents <- readFile fileName
  return (lines contents)

data Point a = Pt a a deriving (Eq, Ord, Show)

originPoint :: Point Int
originPoint = Pt 0 0

instance (Num a) => Num (Point a) where
  -- Implement +
  (Pt x1 y1) + (Pt x2 y2) = Pt (x1 + x2) (y1 + y2)

  -- Implement -
  (Pt x1 y1) - (Pt x2 y2) = Pt (x1 - x2) (y1 - y2)

  -- Implement *
  (Pt x1 y1) * (Pt x2 y2) = Pt (x1 * x2) (y1 * y2)

  -- Implement absolute value
  abs (Pt x y) = Pt (abs x) (abs y)

  -- Implement sign function
  signum (Pt x y) = Pt (signum x) (signum y)

  -- Implement fromInteger to convert literals
  fromInteger n = Pt (fromInteger n) (fromInteger n)
