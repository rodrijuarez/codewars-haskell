module WeightSort where

import Data.Char
import Data.List

orderWeight :: [Char] -> [Char]
orderWeight = unwords . sortOn calcWeight . sort . words

calcWeight :: String -> Int
calcWeight = sum . map digitToInt
