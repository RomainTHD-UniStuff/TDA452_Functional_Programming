{- Lab 3
   Date: 22/11/2021
   Authors: Fanny Rouvel - Romain Theodet
   Lab group: 27
 -}

module Sudoku_B where

import Test.QuickCheck
import Sudoku
import Data.Maybe

---- Part B starts here ------------------------------------------------------

-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks (Sudoku rows) = concat [generateBlankPos rows r c | r <- [0..8], c <- [0..8]]
  where generateBlankPos rows r c | isNothing $ rows !! r !! c = [(r, c)]
                                  | otherwise                  = []

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = and $ zipWith (==) (blanks allBlankSudoku) [(r, c) | r <- [0..8], c <- [0..8]]

-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
(!!=) xs (n,y) | n < 0     = error "Negative index"
               | n <= last = [if (n == i) then y else xs !! i | i <- [0..last]]
               | otherwise = error "Index out of bounds"
  where last = length xs - 1

prop_bangBangEquals_correct :: Eq a => [a] -> (Int, a) -> Bool
prop_bangBangEquals_correct xs (n, y) | n < 0           = True
                                      | n < (length xs) = assertLength && assertResult
                                      | otherwise       = True
  where ownResult = (!!=) xs (n,y)
        assertResult = (take n xs ++ [y] ++ drop (n + 1) xs) == ownResult
        assertLength = length xs == length ownResult

-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
