module Lib
    ( 
      numberOfStonesAfter,
      blinkAtList
    ) where

blinkAtStone :: Int -> [Int]
blinkAtStone 0 = [1]
blinkAtStone stone
  | mod lenStoneStr 2 == 0 = [read (take (div lenStoneStr 2) stoneStr), read (drop (div lenStoneStr 2) stoneStr)]
  where stoneStr = show stone
        lenStoneStr = length(stoneStr)
  

blinkAtList :: [Int] -> [Int]
blinkAtList stones = stones >>= blinkAtStone

numberOfStonesAfter :: [Int] -> Int -> Int
numberOfStonesAfter stones numOfBlinks = length(blinkAtList stones)