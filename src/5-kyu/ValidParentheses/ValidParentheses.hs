module Parentheses where

import Data.Either

-- Kata link: https://www.codewars.com/kata/valid-parentheses/train/haskell
validParentheses :: String -> Bool
validParentheses xs =
  case result of
    (Right 0) -> True
    _ -> False
  where
    result = foldr (\x y -> (solveParenthesis x) =<< y) (Right 0) xs

solveParenthesis :: Char -> Int -> Either Bool Int
solveParenthesis '(' 0 = Left False
solveParenthesis '(' n = Right (n - 1)
solveParenthesis ')' n = Right (n + 1)
