module Day05 (day05main) where

import Tools (getFileLines)

data NiceNess = NN Int Bool deriving (Show)

baseBehavior :: NiceNess
baseBehavior = NN 0 False

isNice :: NiceNess -> Bool
isNice (NN vowels hasDouble) = vowels >= 3 && hasDouble

addVowel :: NiceNess -> NiceNess
addVowel (NN vowels hasDouble) = NN (vowels + 1) hasDouble

flagDouble :: NiceNess -> NiceNess
flagDouble (NN vowels _) = NN vowels True

vowelList :: [Char]
vowelList = ['a', 'e', 'i', 'o', 'u']

isVowel :: Char -> Bool
isVowel item = item `elem` vowelList

naughtyList :: [String]
naughtyList = ["ab", "cd", "pq", "xy"]

onList :: String -> Bool
onList item = item `elem` naughtyList

checkTwice :: String -> NiceNess -> Bool
checkTwice [] behavior = isNice behavior
checkTwice (one : "") behavior
  | isVowel one = isNice (addVowel behavior)
  | otherwise = isNice behavior
checkTwice (one : two : rest) behavior
  | onList [one, two] = False
  | isVowel one && one == two = checkTwice (two : rest) (addVowel (flagDouble behavior))
  | isVowel one = checkTwice (two : rest) (addVowel behavior)
  | one == two = checkTwice (two : rest) (flagDouble behavior)
  | otherwise = checkTwice (two : rest) behavior

startChecking :: String -> Bool
startChecking input = checkTwice input baseBehavior

totalNice :: [String] -> Int
totalNice input = length (filter startChecking input)

firstCheck :: String -> Bool
firstCheck (a : b : r) = bookEnded || firstCheck (b : r)
  where
    bookEnded = findBookend (a : [b]) (reverse r)
firstCheck (_ : _) = False
firstCheck [] = False

findBookend :: String -> String -> Bool
findBookend (a : b) (x : y : r) = (a : b) == (y : [x]) || findBookend (a : b) (y : r)
findBookend _ (_ : _) = False
findBookend _ [] = False

secondCheck :: String -> Bool
secondCheck (x : y : z : r)
  | x == z = True
  | r /= "" = secondCheck (y : z : r)
  | otherwise = False
secondCheck _ = False

totalNice2 :: [String] -> Int
totalNice2 input = length (filter (\x -> secondCheck x && firstCheck x) input)

day05main :: IO ()
day05main = do
  d_stuff <- getFileLines "inputs/day_05_debug"
  putStrLn $ "Total Nice: " ++ show (totalNice d_stuff)
  stuff <- getFileLines "inputs/day_05_input"
  putStrLn $ "Total Nice: " ++ show (totalNice stuff)
  putStrLn $ "Total Nice2: " ++ show (totalNice2 stuff)
