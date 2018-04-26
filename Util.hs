module Util where

import Data.Word (Word32)
import Text.Printf (printf)

(|>) :: a -> (a -> b) -> b 
x |> f = f x

wordHex :: Word32 -> String
wordHex = printf "%08x" 

partialSum :: Num a => [a] -> [a]
partialSum = scanl (+) 0

immSext :: Int -> Integer -> Integer 
immSext w imm = 
    if imm >= bound
        then imm - bound * 2 
        else imm 
    where bound = 2^(w-1)