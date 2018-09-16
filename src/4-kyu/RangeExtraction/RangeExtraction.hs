module RangeExtraction where

import Data.List (intercalate)

solution :: [Int] -> String
solution = showConsecutives . groupConsecutives

showConsecutive :: [Int] -> String
showConsecutive xs
  | length xs < 3 = intercalate "," $ map show xs
  | otherwise = (show $ head xs) ++ "-" ++ (show $ last xs)

showConsecutives [] = ""
showConsecutives xs = intercalate "," $ map showConsecutive xs

groupConsecutives :: [Int] -> [[Int]]
groupConsecutives xs =
  foldr
    (\x acc ->
       case acc of
         [] -> [[x]]
         ((y:group):rest) ->
           if x + 1 == y
             then [[x, y] ++ group] ++ rest
             else [[x], y : group] ++ rest)
    []
    xs
