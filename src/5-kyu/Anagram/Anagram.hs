module Anagrams where

import Data.List

--Kata Link:
--  https://www.codewars.com/kata/where-my-anagrams-at/train/haskell
anagrams :: String -> [String] -> [String]
anagrams w = filter (null . (\\ w))
