module HashtagGenerator where

import Control.Lens
import Data.Char (toUpper)
import Data.List (intercalate)

--Kata Link:
--  https://www.codewars.com/kata/the-hashtag-generator/train/haskell
generateHashtag :: String -> Maybe String
generateHashtag word = do
  let word' = f (word)
  word'' <-
    if length word' < 140 && length word' > 1
      then Just word'
      else Nothing
  return word''
  where
    f = (:) '#' . intercalate "" . map (over _head toUpper) . words
