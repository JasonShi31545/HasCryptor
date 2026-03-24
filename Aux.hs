-- | Some data converters

module Aux where

import Data.Char

intToBin :: Int -> String
intToBin 0 = "0"
intToBin n = convert n
  where convert :: Int -> String
        convert k = if (k > 0) then convert (div k 2) ++ (show $ (mod k 2)) else ""

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

binToInt :: String -> Int
binToInt "0" = 0
binToInt "1" = 1
binToInt s = raise s 0
  where raise :: String -> Int -> Int
        raise "" _ = 0
        raise str n = if (last str == '1') then 2^n + raise (safeInit str) (n+1)
                      else 0 + raise (safeInit str) (n+1)

removeLeadingZeros :: String -> String
removeLeadingZeros str = if (head str == '0') then removeLeadingZeros (tail str) else str

toHex :: Int -> String
toHex n = let a = intToBin n
          in getHex a
            where getHex :: String -> String
                  getHex s = foldl (++) "" $ map substitute $ getSeparated $ padded s
                    where getSeparated :: String -> [String]
                          getSeparated str = [substring str (k*4) 4 | k <- [0..(div (length str) 4)-1]]
                            where substring :: String -> Int -> Int -> String
                                  substring s' startIndex len = [s' !! (startIndex + i) | i <- [0..(len-1)]]
                          substitute :: String -> String
                          substitute __s = sub $ binToInt __s
                            where sub :: Int -> String
                                  sub m
                                    | m < 10 = show m
                                    | otherwise = [chr (ord 'A' + (m - 10))]
                          padded :: String -> String
                          padded string = let l = length string
                                          in pad string $ if (mod l 4 == 0) then 0 else 4-(mod l 4)
                                            where pad :: String -> Int -> String
                                                  pad _s num = if (num > 0) then pad ("0" ++ _s) (num-1) else _s


fromHex :: String -> Int
fromHex hexString = foldl (+) 0 $ numList hexString
  where numList :: String -> [Int]
        numList hexString' = [(getCoef ((reverse hexString') !! index)) * 16^index | index <- [0..((length hexString') - 1)]]
          where getCoef :: Char -> Int
                getCoef ch = let upperlimit = ord '9'
                                 lowerlimit = ord '0'
                                 order = ord ch
                             in if (order >= lowerlimit && order <= upperlimit) then order-lowerlimit else (10 + (order - (ord 'A')))

