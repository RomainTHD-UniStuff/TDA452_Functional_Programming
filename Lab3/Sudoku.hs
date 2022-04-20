{- Lab 3
   Date: 29/11/2021
   Authors: *Redacted due to web crawlers*
   Lab group: 27
 -}

module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.List(nub, transpose)
import Data.Char(intToDigit, digitToInt)

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row]
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) = length rows == 9 &&
                          all ((== 9) . length) rows &&
                          all (all isValidDigit) rows
    where isValidDigit n = isNothing n || (n >= Just 1 && n <= Just 9)

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rows) = all (all isJust) rows

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rows) = putStr $ unlines $ map (map toChar) rows
  where toChar Nothing = '.'
        toChar (Just n) = intToDigit n

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filePath = do
  content <- readFile filePath
  let fromChar c | c == '.' = Nothing
                 | c >= '1' && c <= '9' = Just (digitToInt c)
                 | otherwise = error "Invalid digit"
  let sudoku = Sudoku $ map (map fromChar) (lines content)
  if not (isSudoku sudoku) then error "Not a sudoku" else return sudoku

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency [(1, rJust), (9, rNothing)]
 where rJust = do n <- choose(1,9)
                  return $ Just n
       rNothing = do return Nothing


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
      content <- vectorOf 9 $ vectorOf 9 cell
      return $ Sudoku content

-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

-- Is a block valid or not
isOkayBlock :: Block -> Bool
isOkayBlock block = length justBlock == length (nub justBlock)
  where justBlock = filter isJust block

-- * D2

-- Generate all blocks, i.e. all rows, columns and 3x3 blocks
blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = rows ++ transpose rows ++ generateBlocks rows

-- Generate all 3x3 blocks
generateBlocks :: [Row] -> [Block]
generateBlocks rows = concatMap (map concat . chunksOf' 3 . transpose) (chunksOf' 3 rows)

-- Our own implementation of chunksOf, divides a list in chunks of at most n elements
chunksOf' :: Int -> [a] -> [[a]]
chunksOf' _ [] = []
chunksOf' n list
  | n > 0 = take n list : chunksOf' n (drop n list)
  | otherwise = error "Invalid chunk size"

-- Property for quickCheck
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s = (3*9 == length s_blocks) && all ((9 ==) . length) s_blocks
  where s_blocks = blocks s

-- * D3

-- If a sudoku is valid or not
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)


---- Part A ends here --------------------------------------------------------

---- Part B starts here ------------------------------------------------------

-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks (Sudoku rows) = concat [generateBlankPos r c | r <- [0..8], c <- [0..8]]
  where generateBlankPos r c | isNothing $ rows !! r !! c = [(r, c)]
                             | otherwise                  = []

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = and $ zipWith (==) (blanks allBlankSudoku) [(r, c) | r <- [0..8], c <- [0..8]]

-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (n,x) | n < 0     = error "Negative index"
             | n <= last = [if n == i then x else xs !! i | i <- [0..last]]
             | otherwise = error "Index out of bounds"
  where last = length xs - 1

prop_bangBangEquals_correct :: [Char] -> (Int, Char) -> Bool
prop_bangBangEquals_correct xs (n, x) | n < 0         = True
                                      | n < length xs = assertLength && assertResult
                                      | otherwise     = True
  where ownResult = xs !!= (n,x)
        assertResult = (take n xs ++ [x] ++ drop (n + 1) xs) == ownResult
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
                  | otherwise            = cellOrig    == cellUpdated
          where cellOrig = rows s !! r !! c
                cellUpdated = updated_rows !! r !! c

------------------------------------------------------------------------------

-- * F1

solve :: Sudoku -> Maybe Sudoku
solve s | null sudokus = Nothing
        | otherwise    = Just $ head sudokus
  where sudokus = solve' s (blanks s)

solve' :: Sudoku -> [Pos] -> [Sudoku]
solve' s holes | not(isSudoku s) || not(isOkay s) = []
               | null holes = [s]
               | otherwise = concat [solve' (update s (head holes) (Just n)) (tail holes) | n <- [1..9]]

-- * F2

readAndSolve :: FilePath -> IO ()
readAndSolve path = do
  sud <- readSudoku path
  printResult $ solve sud
  where printResult :: Maybe Sudoku -> IO ()
        printResult Nothing  = putStrLn "(no solution)"
        printResult (Just s) = printSudoku s

-- * F3

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sol orig | not(isSudoku sol) || not(isOkay sol) || not(null $ blanks sol) = False
                      | otherwise = orig == replace sol (blanks orig)
  where replace s []     = s
        replace s (p:ps) = replace (update s p Nothing) ps

-- * F4

prop_SolveSound :: Sudoku -> Property
prop_SolveSound s | isNothing sol = property True
                  | otherwise     = property (isSolutionOf (fromJust sol) s)
  where sol = solve s
