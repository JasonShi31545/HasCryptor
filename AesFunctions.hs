module AESFunctions where

import Data.Bits
import Data.List
import Data.Char
import Data.Int ()
import Data.ByteString hiding (length, concat, transpose, head, tail)

-- TODO: Convert Char to Word8, it's more generic that way

import Aux
import RawData

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
            

hexToInt :: Char -> Int
hexToInt c
  | (ord c <= ord '9') && (ord c >= ord '0') = ord c - ord '0'
  | (ord c <= ord 'F') && (ord c >= ord 'A') = 10 + (ord c - ord 'A')
  | (ord c <= ord 'f') && (ord c >= ord 'f') = 10 + (ord c - ord 'a')
  | otherwise = -1

  
ltableLookup :: Char -> Char
ltableLookup input = chr ((ltable !! a) !! b)
  where a :: Int
        a = hexToInt $ head $ toHex $ ord input
        b :: Int
        b = hexToInt $ head $ tail $ toHex $ ord input

etableLookup :: Int -> Char
etableLookup input = chr $ ((etable !! a) !! b)
  where a :: Int
        a = hexToInt $ head $ toHex input
        b :: Int
        b = hexToInt $ head $ tail $ toHex input
        
gmul :: Char -> Char -> Char
gmul a b
  | ord a == 0 || ord b == 0 = chr 0
  


mixColumn :: [a] -> [a]
mixColumn dat
  | length dat == 16 = undefined
  | otherwise = dat
