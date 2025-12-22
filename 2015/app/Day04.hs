module Day04 (day04main) where

import Crypto.Hash
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.List as L

hashStartsWith :: Int -> Bool
hashStartsWith number = L.isPrefixOf "000000" (show (hashWith MD5 (UTF8.fromString ("iwrupvqb" ++ show number) :: UTF8.ByteString)))

rabbitHole :: Int -> Int
rabbitHole layers = if hashStartsWith layers then layers else rabbitHole $! layers + 1

day04main :: IO ()
day04main = do
    print $ show (rabbitHole 0)
