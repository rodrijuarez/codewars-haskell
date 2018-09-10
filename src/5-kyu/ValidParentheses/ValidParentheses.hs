module Parentheses where

import Data.Either

-- Kata link: https://www.codewars.com/kata/valid-parentheses/train/haskell
validParentheses :: String -> Bool
validParentheses xs =
  case result of
    (Right 0) -> True
    _ -> False
  where
    result = foldl (\x y -> solveParenthesis y x) (Right 0) xs

solveParenthesis :: Char -> Either Bool Int -> Either Bool Int
solveParenthesis _ (Left _) = Left False
solveParenthesis ')' (Right 0) = Left False
solveParenthesis ')' (Right n) = Right (n - 1)
solveParenthesis '(' (Right n) = Right (n + 1)
