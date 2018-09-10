module HumanTime where

humanReadable :: Int -> String
humanReadable x = hours ++ ":" ++ minutes ++ ":" ++ seconds
  where
    seconds = showWithTwoDigits $ x `mod` 60
    minutes = showWithTwoDigits $ (x `div` 60) `mod` 60
    hours = showWithTwoDigits $ ((x `div` 60) `div` 60)

showWithTwoDigits :: Int -> String
showWithTwoDigits n
  | length (show n) == 1 = "0" ++ (show n)
  | otherwise = show n
