-- | Generator for Prime Numbers

module HasCryptor.Prime where

import Data.Bits
import Data.Word

sieve_n :: Int
sieve_n = 4

sieveList :: [Int]
sieveList = [2,3..(10^sieve_n)]

-- pmod is the modular power of (x^y) % g.
pmod :: Int -> Int -> Int -> Int
pmod x y g = pmod' x y g 1
  where pmod' :: Int -> Int -> Int -> Int -> Int
        pmod' x' y' g' res
          | y' <= 0 = res
          | (mod y' 2) == 1 = pmod' (mod ((mod (mod x' g') g')^2) g') (shiftR y' 1) g' (mod ((mod res g') * (mod (mod x' g') g')) g')
          | otherwise = pmod' (mod ((mod (mod x' g') g')^2) g') (shiftR y' 1) g' res

miller :: Int -> Int -> Int -> Bool
miller d n r = let x = pmod (processR r) d n
               in if (x == 1 || x == (n-1)) then True else millerLoop d n x
                  where processR :: Int -> Int
                        processR r' = 2 + (mod r' (n - 4))
                        millerLoop :: Int -> Int -> Int -> Bool
                        millerLoop d' n' x' = if (d' == (n-1) || x' == 1) then False else if (x' == (n'-1)) then True else millerLoop (d'*2) n' (mod ((mod x' n') * (mod x' n')) n')




isPrime :: Int -> Int -> [Int] -> Bool
isPrime n k rs
  | (n <= 1 || n == 4) = False
  | (n <= 3) = True
  | otherwise = let d = continuousShift (n-1)
                in foldl (&&) True $ [miller d n (rs !! i) | i <- [0..(k-1)]]
                   where continuousShift :: Int -> Int
                         continuousShift d'
                           | mod d' 2 /= 0 = d'
                           | otherwise = continuousShift (div d' 2)

getSieve :: Int -> [Int]
getSieve n
  | n >= 3 = pass [0..200] 0
  | otherwise = []
  where pass :: [Int] -> Int -> [Int]
        pass list k
          | k >= (length list) = list
          | length list == 0 = list
          | (list !! k) /= 0 && (list !! k) /= 1 = let newList = [list !! k] ++ filter (\x -> (mod x (list !! k)) /= 0) list
                               in pass newList (k+1)
          | otherwise = list
