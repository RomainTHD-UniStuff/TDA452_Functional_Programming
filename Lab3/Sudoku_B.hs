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
xs !!= (n,y) | n < 0     = error "Negative index"
               | n <= last = [if (n == i) then y else xs !! i | i <- [0..last]]
               | otherwise = error "Index out of bounds"
  where last = length xs - 1

prop_bangBangEquals_correct :: Eq a => [a] -> (Int, a) -> Bool
prop_bangBangEquals_correct xs (n, y) | n < 0           = True
                                      | n < (length xs) = assertLength && assertResult
                                      | otherwise       = True
  where ownResult = xs !!= (n,y)
        assertResult = (take n xs ++ [y] ++ drop (n + 1) xs) == ownResult
        assertLength = length xs == length ownResult

-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku rows) (row, col) cell = Sudoku $ rows !!= (row, newRow)
  where newRow = (rows !! row) !!= (col, cell)

prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated s (row, col) cell | row < 0 || row > 8 || col < 0 || col > 8 = True
                                      | otherwise = and [check r c | r <- [0..8], c <- [0..8]]
  where (Sudoku updated_rows) = update s (row, col) cell
        check r c | r == row && c == col = cellUpdated == cell                                       
                  | otherwise = cellOrig == cellUpdated
          where cellOrig = (rows s) !! r !! c
                cellUpdated = updated_rows !! r !! c

------------------------------------------------------------------------------

-- * F1

solve :: Sudoku -> Maybe Sudoku
solve s | sudokus == [] = Nothing
        | otherwise = Just $ head sudokus
  where sudokus = solve' s (blanks s)

solve' :: Sudoku -> [Pos] -> [Sudoku]
solve' s holes | not(isSudoku s) || not(isOkay s) = []
               | holes == [] = [s]
               | otherwise = concat [solve' (update s (head holes) (Just n)) (tail holes) | n <- [1..9]]

-- * F2

readAndSolve :: FilePath -> IO ()
readAndSolve path = do
  sud <- readSudoku path
  printResult $ solve sud
  where printResult :: Maybe Sudoku -> IO ()
        printResult Nothing = putStrLn "(no solution)"
        printResult (Just s) = printSudoku s

-- * F3

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sol orig | not(isSudoku sol) || not(isOkay sol) || blanks sol /= [] = False
                      | otherwise = orig == replace sol (blanks orig)
  where replace s [] = s
        replace s (p:ps) = replace (update s p Nothing) ps

-- * F4

prop_SolveSound :: Sudoku -> Property
prop_SolveSound s | isNothing sol = property True
                  | otherwise = property (isSolutionOf (fromJust sol) s)
  where sol = solve s
