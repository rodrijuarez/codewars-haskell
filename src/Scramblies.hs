module Scramblies where

import Data.List (delete)

scramble :: [Char] -> [Char] -> Bool
scramble _ "" = True
scramble s1 (x:xs) =
  if (x `elem` s1)
    then scramble (delete x s1) xs
    else False
