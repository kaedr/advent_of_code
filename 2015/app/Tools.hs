module Tools (splitBy, getFileLines, Point(..), originPoint) where

splitBy :: Char -> String -> [String]
splitBy _ [] = [""]
splitBy delimiter (c:cs) | c == delimiter = "" : rest
                        | otherwise = (c : head rest) : tail rest
            where rest = splitBy delimiter cs


getFileLines :: FilePath -> IO [String]
getFileLines fileName = do
    contents <- readFile fileName
    return (lines contents)


data Point a = Pt a a deriving (Eq, Ord, Show)

originPoint :: Point Int
originPoint = Pt 0 0

instance Num a => Num (Point a) where
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
