module Lib
    ( 
      numberOfStonesAfter,
      blinkAtList
    ) where

import Data.MemoTrie

blinkAtStone :: Int -> [Int]
blinkAtStone 0 = [1]
blinkAtStone stone
  | mod lenStoneStr 2 == 0 = [read (take halfLen stoneStr), read (drop halfLen stoneStr)]
  | otherwise = [stone * 2024]
  where stoneStr = show stone
        lenStoneStr = length(stoneStr)
        halfLen = div lenStoneStr 2

blinkAtStone' :: Int -> [Int]
blinkAtStone' = memo blinkAtStone

blinkAtList :: [Int] -> [Int]
blinkAtList stones = stones >>= blinkAtStone'


numOfStones :: Int -> Int -> Int
numOfStones 0 _ = 1
numOfStones 3 stone | stone >= 1 && stone <= 4 = 4
numOfStones 2 stone | stone >= 1 && stone <= 4 = 2
numOfStones 1 stone | stone >= 1 && stone <= 4 = 1

numOfStones 5 stone | stone >= 5 && stone <= 9 && stone /= 8 = 8
numOfStones 5 8 = 7
numOfStones 4 stone | stone == 0 || (stone >= 5 && stone <= 9) = 4
numOfStones 3 stone | stone == 0 || (stone >= 5 && stone <= 9) = 2
numOfStones 2 stone | stone == 0 || (stone >= 5 && stone <= 9) = 1
numOfStones 1 stone | stone == 0 || (stone >= 5 && stone <= 9) = 1

numOfStones numOfBlinks 0
  | numOfBlinks >= 4 = numberOfStonesAfter [2, 0, 2, 4] (numOfBlinks-4)

numOfStones numOfBlinks 1
  | numOfBlinks >= 3 = numberOfStonesAfter [2, 0, 2, 4] (numOfBlinks-3)

numOfStones numOfBlinks 2
  | numOfBlinks >= 3 = numberOfStonesAfter [4, 0, 4, 8] (numOfBlinks-3)

numOfStones numOfBlinks 3
  | numOfBlinks >= 3 = numberOfStonesAfter [6, 0, 7, 2] (numOfBlinks-3)

numOfStones numOfBlinks 4
  | numOfBlinks >= 3 = numberOfStonesAfter [8, 0, 9, 6] (numOfBlinks-3)

numOfStones numOfBlinks 5
  | numOfBlinks >= 5 = numberOfStonesAfter [2, 0, 4, 8, 2, 8, 8, 0] (numOfBlinks-5)

numOfStones numOfBlinks 6
  | numOfBlinks >= 5 = numberOfStonesAfter [2, 4, 5, 7, 9, 4, 5, 6] (numOfBlinks-5)

numOfStones numOfBlinks 7
  | numOfBlinks >= 5 = numberOfStonesAfter [2, 8, 6, 7, 6, 0, 3, 2] (numOfBlinks-5)

numOfStones numOfBlinks 8
  | numOfBlinks >= 5 = (numOfStones' (numOfBlinks-4) 8) + (numberOfStonesAfter [3, 2, 7, 7, 2, 6] (numOfBlinks-5))

numOfStones numOfBlinks 9
  | numOfBlinks >= 5 = numberOfStonesAfter [3, 6, 8, 6, 9, 1, 8, 4] (numOfBlinks-5)

numOfStones numOfBlinks stone = numberOfStonesAfter (blinkAtStone' stone) (numOfBlinks-1)

numOfStones' :: Int -> Int -> Int
numOfStones' = memo numOfStones

numberOfStonesAfter :: [Int] -> Int -> Int
numberOfStonesAfter stones 0 = length(stones)
numberOfStonesAfter stones numOfBlinks = sum(map (numOfStones' numOfBlinks) stones)
--numberOfStonesAfter stones numOfBlinks = length(iterate blinkAtList stones !! numOfBlinks)