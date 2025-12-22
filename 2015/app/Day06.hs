module Day06 (day06main) where

import Debug.Trace (traceShow)
import Text.Regex.Posix
import Tools (safeTail)

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

innerNumerate :: Int -> [a] -> [(Int, Int, a)]
innerNumerate outer = zipWith (curry (\(inner, item) -> (outer, inner, item))) [0 ..]

-- miniGrid :: [[Bool]]
-- miniGrid = [[False | _ <- [0 .. 2]] | _ <- [0 .. 2]]

startingGrid :: [[Bool]]
startingGrid = [[False | _ <- [0 :: Int .. 999]] | _ <- [0 :: Int .. 999]]

startingGrid2 :: [[Int]]
startingGrid2 = [[0 | _ <- [0 :: Int .. 999]] | _ <- [0 :: Int .. 999]]

data Activity = On | Off | Toggle deriving (Show)

parseActivity :: String -> Activity
parseActivity "turn on" = On
parseActivity "turn off" = Off
parseActivity "toggle" = Toggle
parseActivity _ = error "Bad Activity!"

data Instruction = Instr Activity (Int, Int) (Int, Int) deriving (Show)

instructionRegex :: String
instructionRegex = "(toggle|turn on|turn off) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)"

parseInstruction :: [String] -> Instruction
parseInstruction (ins : x1 : y1 : x2 : y2 : _) = Instr (parseActivity ins) (read x1, read y1) (read x2, read y2)
parseInstruction _ = error "Bad Instruction Data!"

deriveInstructions :: String -> [Instruction]
deriveInstructions input =
  let all_submatches = map safeTail (input =~ instructionRegex :: [[String]])
   in map parseInstruction all_submatches

applyInstruction :: [[Bool]] -> Instruction -> [[Bool]]
applyInstruction grid (Instr ins (x1, y1) (x2, y2)) = mapGrid
  where
    xRange = [x1 .. x2]
    yRange = [y1 .. y2]
    doInstruction :: (Int, Int, Bool) -> Bool
    doInstruction (y, x, val) =
      if (y `elem` yRange) && (x `elem` xRange)
        then case ins of
          On -> True
          Off -> False
          Toggle -> not val
        else val

    mapRow :: (Int, [Bool]) -> [Bool]
    mapRow (y, row) = map doInstruction (innerNumerate y row)
    mapGrid = map mapRow (enumerate grid)

applyInstructions :: [[Bool]] -> [Instruction] -> [[Bool]]
applyInstructions grid (order : remainingOrders) = applyInstructions newGrid remainingOrders
  where
    _ = traceShow ("Remaining: ", length remainingOrders)
    newGrid = applyInstruction grid order
applyInstructions grid [] = grid

applyBrightness :: [[Int]] -> Instruction -> [[Int]]
applyBrightness grid (Instr ins (x1, y1) (x2, y2)) = mapGrid
  where
    xRange = [x1 .. x2]
    yRange = [y1 .. y2]
    doInstruction :: (Int, Int, Int) -> Int
    doInstruction (y, x, val) =
      if (y `elem` yRange) && (x `elem` xRange)
        then case ins of
          On -> val + 1
          Off -> if val > 0 then val - 1 else 0
          Toggle -> val + 2
        else val

    mapRow :: (Int, [Int]) -> [Int]
    mapRow (y, row) = map doInstruction (innerNumerate y row)
    mapGrid = map mapRow (enumerate grid)

applyBrightnesses :: [[Int]] -> [Instruction] -> [[Int]]
applyBrightnesses grid (order : remainingOrders) = applyBrightnesses newGrid remainingOrders
  where
    _ = traceShow ("Remaining: ", length remainingOrders)
    newGrid = applyBrightness grid order
applyBrightnesses grid [] = grid

flatCount :: [[Bool]] -> Int
flatCount items = length (concatMap (filter id) items)

flatSum :: [[Int]] -> Int
flatSum items = sum (concat items)

day06main :: IO ()
day06main = do
  stuff <- readFile "inputs/day_06_input"
  let orders = deriveInstructions stuff
      resultGrid = applyInstructions startingGrid orders
      brightGrid = applyBrightnesses startingGrid2 orders

  putStrLn $ "Total Lights on: " ++ show (flatCount resultGrid)
  putStrLn $ "Total Brightness: " ++ show (flatSum brightGrid)
