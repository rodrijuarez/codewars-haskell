module NarcissisticNumber where

import Data.Digits (digits)

narcissistic :: Integral n => n -> Bool
narcissistic n = (==) n . sum $ map (^ (length nDigits)) nDigits
  where
    nDigits = digits 10 n
