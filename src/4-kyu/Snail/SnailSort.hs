module Snail where

twoTwo :: [[Int]]
twoTwo = [[1, 2], [4, 5]]

threeThree :: [[Int]]
threeThree = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

fiveFive :: [[Int]]
fiveFive =
  [ [1, 2, 3, 4, 5]
  , [6, 7, 8, 9, 10]
  , [11, 12, 13, 14, 15]
  , [16, 17, 18, 19, 20]
  , [21, 22, 23, 24, 25]
  ]

sixSix :: [[Int]]
sixSix =
  [ [41, 41, 41, 41, 41, 26]
  , [41, 41, 41, 41, 10, 27]
  , [11, 12, 13, 14, 15, 28]
  , [16, 17, 18, 19, 20, 29]
  , [21, 22, 23, 24, 25, 30]
  , [31, 32, 33, 34, 35, 36]
  ]

snail :: [[Int]] -> [Int]
snail [] = []
snail [[]] = []
snail [a] = a
snail (top:rest) = top ++ right ++ bottom ++ left ++ content
  where
    middle = init rest
    right = map last middle
    bottom = reverse $ last rest
    left = reverse $ map head middle
    content = snail $ map (drop 1 . init) middle
