module JadenCased where

import Control.Lens
import Data.Char (toUpper)

toJadenCase :: String -> String
toJadenCase = unwords . map (over _head toUpper) . words
