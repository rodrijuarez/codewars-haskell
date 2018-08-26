module Rot13 where

import Data.Char

rot13 :: String -> String
rot13 word = map f word
  where
    f c =
      if not (c `elem` (['a' .. 'z'] ++ ['A' .. 'Z']))
        then c
        else case generalCategory c of
               UppercaseLetter -> encryptUppercaseLetter c
               LowercaseLetter -> encryptLowercaseLetter c
               _ -> c

encryptUppercaseLetter = chr . placeBetween 65 90 . (+) 13 . ord

encryptLowercaseLetter = chr . placeBetween 97 122 . (+) 13 . ord

placeBetween start end num =
  if num > end
    then num - end + start - 1
    else num
