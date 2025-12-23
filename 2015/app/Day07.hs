module Day07 (day07main) where

import Data.Bits
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as DM
import Data.Maybe (fromJust)
import Data.Word
import Text.Read (readMaybe)
import Tools (getFileLines, splitBy)

data WireTreeNode
  = Signal {val :: Word16, output :: String}
  | AndNode {left :: String, right :: String, output :: String}
  | And1Node {right :: String, output :: String}
  | OrNode {left :: String, right :: String, output :: String}
  | NotNode {input :: String, output :: String}
  | LShiftNode {input :: String, val :: Word16, output :: String}
  | RShiftNode {input :: String, val :: Word16, output :: String}
  | PassThrough {input :: String, output :: String}
  deriving (Show)

type MemoTree = ([WireTreeNode], Map String Word16)

calculateNodeVal :: MemoTree -> WireTreeNode -> (MemoTree, Word16)
calculateNodeVal (nodes, memo) (Signal val output) = ((nodes, DM.insert output val memo), val)
calculateNodeVal (nodes, memo) (AndNode left right output) = ((nodes, DM.insert output val newMemo), val)
  where
    ((_, midMemo), leftVal) = getNodeVal (nodes, memo) left
    ((_, newMemo), rightVal) = getNodeVal (nodes, midMemo) right
    val = leftVal .&. rightVal
calculateNodeVal (nodes, memo) (And1Node right output) = ((nodes, DM.insert output val newMemo), val)
  where
    ((_, newMemo), rightVal) = getNodeVal (nodes, memo) right
    val = 1 .&. rightVal
calculateNodeVal (nodes, memo) (OrNode left right output) = ((nodes, DM.insert output val newMemo), val)
  where
    ((_, midMemo), leftVal) = getNodeVal (nodes, memo) left
    ((_, newMemo), rightVal) = getNodeVal (nodes, midMemo) right
    val = leftVal .|. rightVal
calculateNodeVal (nodes, memo) (NotNode input output) = ((nodes, DM.insert output val newMemo), val)
  where
    ((_, newMemo), inVal) = getNodeVal (nodes, memo) input
    val = complement inVal
calculateNodeVal (nodes, memo) (LShiftNode input shiftVal output) = ((nodes, DM.insert output outVal newMemo), outVal)
  where
    ((_, newMemo), inVal) = getNodeVal (nodes, memo) input
    outVal = shiftL inVal (fromIntegral shiftVal)
calculateNodeVal (nodes, memo) (RShiftNode input shiftVal output) = ((nodes, DM.insert output outVal newMemo), outVal)
  where
    ((_, newMemo), inVal) = getNodeVal (nodes, memo) input
    outVal = shiftR inVal (fromIntegral shiftVal)
calculateNodeVal (nodes, memo) (PassThrough input output) =
  let ((_, newMemo), val) = getNodeVal (nodes, memo) input
   in ((nodes, DM.insert output val newMemo), val)

getNodeVal :: MemoTree -> String -> (MemoTree, Word16)
getNodeVal (nodes, memo) nodeId =
  case DM.lookup nodeId memo of
    Just val -> ((nodes, memo), val)
    Nothing ->
      let nextNode = find (\node -> output node == nodeId) nodes
       in calculateNodeVal (nodes, memo) (fromJust nextNode)

nodeFrom :: [String] -> WireTreeNode
nodeFrom [input, "LSHIFT", val, "->", output] = LShiftNode input (read val) output
nodeFrom [input, "RSHIFT", val, "->", output] = RShiftNode input (read val) output
nodeFrom [left, "OR", right, "->", output] = OrNode left right output
nodeFrom ["1", "AND", right, "->", output] = And1Node right output
nodeFrom [left, "AND", right, "->", output] = AndNode left right output
nodeFrom ["NOT", input, "->", output] = NotNode input output
nodeFrom [val, "->", output] = case readMaybe val of
  Just numVal -> Signal numVal output
  Nothing -> PassThrough val output
nodeFrom other = error ("Unable to create node from: " ++ show other)

parseNode :: String -> WireTreeNode
parseNode = nodeFrom . splitBy ' '

day07main :: IO ()
day07main = do
  stuff <- getFileLines "inputs/day_07_input"
  let nodes = map parseNode stuff
  let (_, result) = getNodeVal (nodes, DM.empty) "a"
  putStrLn $ "Output Values: " ++ show result
