module WeightSort where

import Data.Digits (digits)
import Data.List (sort)

orderWeight :: [Char] -> [Char]
orderWeight = unwords . map show . sort . map (FatNumber) . words

data FatNumber =
  FatNumber (String)

weight :: FatNumber -> Int
weight (FatNumber x) = sum $ digits 10 $ read x :: Int

instance Show FatNumber where
  show (FatNumber x) = x

instance Eq FatNumber where
  (FatNumber x) == (FatNumber y) = x == y

instance Ord FatNumber where
  compare a b =
    if (weight a == weight b)
      then compare (show a) (show b)
      else compare (weight a) (weight b)
