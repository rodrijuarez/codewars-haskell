module BreakCamelCase where

import Data.Char

solution :: String -> String
solution (x:xs) = foldl f [x] xs
  where
    f acc c =
      case generalCategory c of
        UppercaseLetter -> acc ++ ' ' : [c]
        _ -> acc ++ [c]
