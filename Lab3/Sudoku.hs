module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.List(group, sort, nub, transpose)
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
allBlankSudoku = Sudoku [row | _ <- [1..9]]
  where row = [Nothing | _ <- [1..9]]

allBlankSudoku' :: Sudoku
allBlankSudoku' = Sudoku $ replicate 9 $ replicate 9 Nothing

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
printSudoku (Sudoku rows) = mapM_ putRowLn rows
  where putRowLn row = do
            mapM_ putCol row
            putStrLn ""
        putCol Nothing = putStr "."
        putCol (Just n) = putStr $ show n

printSudoku' :: Sudoku -> IO ()
printSudoku' (Sudoku rows) = mapM_ putRowLn rows
  where putRowLn row = putStrLn (map toChar row)
        toChar Nothing = '.'
        toChar (Just n) = intToDigit n

printSudoku'' :: Sudoku -> IO ()
printSudoku'' (Sudoku rows) = putStr $ unlines $ map (map toChar) rows
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
cell = elements $ [Just n | n <- [1..9]] ++ [Nothing]

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

isOkayBlock :: Block -> Bool
isOkayBlock block = length justBlock == length (group $ sort justBlock) 
  where justBlock = filter isJust block

isOkayBlock' :: Block -> Bool
isOkayBlock' block = length justBlock == length (nub justBlock) 
  where justBlock = filter isJust block

-- * D2

blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = rows ++ generateCols ++ generateBlocks
    where 
          generateCols = [map (getCell c) rows | c <- [1..9]]
          getCell c row = head (drop (c-1) row)
          generateCols' = [map (!! (c-1)) rows | c <- [1..9]]
          generateCols'' = transpose rows
          generateBlocks = rows

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths = undefined

-- * D3

isOkay :: Sudoku -> Bool
isOkay = undefined


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


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
