module Parentheses where

import Control.Monad
import Data.Either

-- Kata link: https://www.codewars.com/kata/valid-parentheses/train/haskell
validParentheses :: String -> Bool
validParentheses xs = either (const False) (== 0) result
  where
    result = foldM solveParenthesis 0 xs

solveParenthesis :: Int -> Char -> Either Bool Int
solveParenthesis 0 ')' = Left False
solveParenthesis n ')' = Right (n - 1)
solveParenthesis n '(' = Right (n + 1)
