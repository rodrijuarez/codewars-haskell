module CreatePhoneNumber where

import Data.List (intercalate)

createPhoneNumber :: [Int] -> String
createPhoneNumber phone = "(" ++ first ++ ") " ++ second ++ "-" ++ third
  where
    f = intercalate "" . map show
    first = f $ take 3 phone
    second = f $ take 3 $ drop 3 phone
    third = f $ take 4 $ drop 6 phone
