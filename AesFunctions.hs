module AESFunctions where

import Data.Bits
import Data.List
import Data.Char

import Aux

shiftLeft :: [a] -> Int -> [a]
-- dat: data
-- off: offset
shiftLeft dat off
  | length dat == 4 = [dat !! i | i <- [off..(length dat - 1)]] ++ [dat !! i | i <- [0..(off-1)]]
  | otherwise = dat

shiftRight :: [a] -> Int -> [a]
shiftRight dat off
  | length dat == 4 = [dat !! i | i <- [(length dat - off)..(length dat - 1)]] ++ [dat !! i | i <- [0..((length dat) - off - 1)]]
  | otherwise = dat

shiftRow :: [a] -> [a]
shiftRow dat
  | length dat == 16 = concat $ transpose $ operateOnRow $ transpose [[dat !! (k*4+j) | j <- [0..3]] | k <- [0..3]]
  | otherwise = dat
      where operateOnRow :: [[a]] -> [[a]]
            operateOnRow matrix = [shiftLeft (matrix !! i) (i+1) | i <- [0..3]]
            
mapHex :: Int -> Char
mapHex = head . toHex

ltable :: Char -> Char
ltable = undefined
etable :: Int -> Char
etable = undefined
gmul :: Char -> Char -> Char
gmul = undefined


mixColumn :: [a] -> [a]
mixColumn dat
  | length dat == 16 = undefined
  | otherwise = dat
