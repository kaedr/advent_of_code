module Day15 (day15main) where

import Data.List (nub, permutations, unzip5)
import Data.Set as DS (Set, empty, insert, notMember)
import Text.Regex.Posix
import Tools (safeTail)

data Ingredient = Ingredient
  { name :: String,
    capacity :: Int,
    durability :: Int,
    flavor :: Int,
    texture :: Int,
    calories :: Int
  }
  deriving (Show)

ingredientRegex :: String
ingredientRegex = "([A-Za-z]+): capacity (-?[0-9]+), durability (-?[0-9]+), flavor (-?[0-9]+), texture (-?[0-9]+), calories (-?[0-9]+)"

parseIngredient :: [String] -> Ingredient
parseIngredient (name : capacity : durability : flavor : texture : calories : _) =
  Ingredient name (read capacity) (read durability) (read flavor) (read texture) (read calories)
parseIngredient _ = error "Bad Ingredient Data!"

parseIngredients :: String -> [Ingredient]
parseIngredients input =
  let all_submatches = map safeTail (input =~ ingredientRegex :: [[String]])
   in map parseIngredient all_submatches

ingredientSubtotals :: (Int, Ingredient) -> (Int, Int, Int, Int, Int)
ingredientSubtotals (qty, item) = (qty * capacity item, qty * durability item, qty * flavor item, qty * texture item, qty * calories item)

flooredSum :: [Int] -> Int
flooredSum = max 0 . sum

calculateScore :: [Int] -> [Ingredient] -> Int
calculateScore qtys ingredients =
  let subTotals = zipWith (curry ingredientSubtotals) qtys ingredients
      (cap, dur, flav, tex, cal) = unzip5 subTotals
   in flooredSum cap * flooredSum dur * flooredSum flav * flooredSum tex * if sum cal > 500 then 0 else 1

masks :: Int -> [[Int]]
masks n = nub (permutations (1 : replicate (n - 1) 0))

findCombos :: Set [Int] -> [Ingredient] -> Int -> [Int] -> (Set [Int], [Int])
findCombos visited ingredients currentScore qtys
  | sum qtys < 100 =
      let nextQtys = map (zipWith (+) qtys) (masks (length qtys))
       in checkPursuits visited ingredients currentScore nextQtys
  | otherwise = (visited, [calculateScore qtys ingredients])

checkPursuits :: Set [Int] -> [Ingredient] -> Int -> [[Int]] -> (Set [Int], [Int])
checkPursuits visited ingredients currentScore (qtys : rest) = (nowVisited, myResults ++ nextResults)
  where
    (myVisited, myResults) = pursueIf visited ingredients currentScore qtys
    (nowVisited, nextResults) = checkPursuits myVisited ingredients currentScore rest
checkPursuits visited _ _ [] = (visited, [])

pursueIf :: Set [Int] -> [Ingredient] -> Int -> [Int] -> (Set [Int], [Int])
pursueIf visited ingredients currentScore qtys =
  let newScore = calculateScore qtys ingredients
   in if newScore >= currentScore && DS.notMember qtys visited
        then findCombos (DS.insert qtys visited) ingredients newScore qtys
        else (DS.insert qtys visited, [])

day15main :: IO ()
day15main = do
  input <- readFile "inputs/day_15_input"
  let cabinet = parseIngredients input
  putStrLn $ "Ingredients: " ++ show cabinet
  let (_, scores) = findCombos DS.empty cabinet 0 (replicate (length cabinet) 0)
  -- putStrLn $ "Scores: " ++ show scores
  putStrLn $ "Best: " ++ show (maximum scores)
