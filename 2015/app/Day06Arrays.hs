module Day06Arrays (day06Amain) where

import Data.Array
import Debug.Trace (traceShow)
import Text.Regex.Posix
import Tools (safeTail)

-- enumerate :: [a] -> [(Int, a)]
-- enumerate = zip [0 ..]

-- innerNumerate :: Int -> [a] -> [(Int, Int, a)]
-- innerNumerate outer items = map (\(inner, item) -> (outer, inner, item)) (zip [0 ..] items)

-- miniGrid :: [[Bool]]
-- miniGrid = [[False | _ <- [0 .. 2]] | _ <- [0 .. 2]]

matrixBounds :: ((Int, Int), (Int, Int))
matrixBounds = ((0, 0), (999, 999))

startingMatrix :: Array (Int, Int) Bool
startingMatrix = array matrixBounds [((y, x), False) | y <- [0 .. 999], x <- [0 .. 999]]

startingMatrix2 :: Array (Int, Int) Int
startingMatrix2 = array matrixBounds [((y, x), 0) | y <- [0 .. 999], x <- [0 .. 999]]

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
parseInstruction (ins : x1 : y1 : x2 : y2 : _) = Instr (parseActivity ins) ((read x1), (read y1)) ((read x2), (read y2))
parseInstruction _ = error "Bad Instruction Data!"

deriveInstructions :: String -> [Instruction]
deriveInstructions input =
  let all_submatches = map safeTail (input =~ instructionRegex :: [[String]])
   in map parseInstruction all_submatches

applyInstruction :: Array (Int, Int) Bool -> Instruction -> Array (Int, Int) Bool
applyInstruction grid (Instr ins (x1, y1) (x2, y2)) = mapGrid
  where
    xRange = [x1 .. x2]
    yRange = [y1 .. y2]
    doInstruction :: ((Int, Int), Bool) -> ((Int, Int), Bool)
    doInstruction ((y, x), val) =
      if (y `elem` yRange) && (x `elem` xRange)
        then case ins of
          On -> ((y, x), True)
          Off -> ((y, x), False)
          Toggle -> ((y, x), not val)
        else ((y, x), val)

    mapGrid = array matrixBounds (fmap doInstruction (assocs grid))

applyInstructions :: Array (Int, Int) Bool -> [Instruction] -> Array (Int, Int) Bool
applyInstructions grid (order : remainingOrders) = applyInstructions newGrid remainingOrders
  where
    _ = traceShow ("Remaining: ", length remainingOrders)
    newGrid = applyInstruction grid order
applyInstructions grid [] = grid

applyBrightness :: Array (Int, Int) Int -> Instruction -> Array (Int, Int) Int
applyBrightness grid (Instr ins (x1, y1) (x2, y2)) = mapGrid
  where
    xRange = [x1 .. x2]
    yRange = [y1 .. y2]
    doInstruction :: ((Int, Int), Int) -> ((Int, Int), Int)
    doInstruction ((y, x), val) =
      if (y `elem` yRange) && (x `elem` xRange)
        then case ins of
          On -> ((y, x), val + 1)
          Off -> ((y, x), if val > 0 then val - 1 else 0)
          Toggle -> ((y, x), val + 2)
        else ((y, x), val)

    mapGrid = array matrixBounds (fmap doInstruction (assocs grid))

applyBrightnesses :: Array (Int, Int) Int -> [Instruction] -> Array (Int, Int) Int
applyBrightnesses grid (order : remainingOrders) = applyBrightnesses newGrid remainingOrders
  where
    _ = traceShow ("Remaining: ", length remainingOrders)
    newGrid = applyBrightness grid order
applyBrightnesses grid [] = grid

flatCount :: Array (Int, Int) Bool -> Int
flatCount items = length (filter id (elems items))

day06Amain :: IO ()
day06Amain = do
  stuff <- readFile "inputs/day_06_input"
  let orders = deriveInstructions stuff
      resultGrid = applyInstructions startingMatrix orders
      brightGrid = applyBrightnesses startingMatrix2 orders
  putStrLn $ "Total Brightness: " ++ show (sum brightGrid)
  putStrLn $ "Total Lights on: " ++ show (flatCount resultGrid)
  putStrLn "Done."
