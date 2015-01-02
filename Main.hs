{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-} 
module Main where
import Data.List 
import Data.Monoid
import Text.Printf
import Options.Applicative
import Data.List.Split (splitOn)
import Control.Applicative
import Data.Char (isDigit, isSpace)
import Control.Monad (when)
import System.Process (readProcess)

-- TODO change the delimiter to tabs or whitespace

data Options = Options {
    splitter :: String -> [String]   -- delimiter splitting function
  , suppressRowDividers :: Bool 
  , maxWidth :: Int
  } 

parseOptions :: Parser Options
parseOptions = Options
  <$> (setDelimiter <|> whiteSpaceDelimiter <|> pure (splitOn "\t"))
  <*> switch (short 'r' <> help "Don't print row dividers")
  <*> parseMaxWidth

setDelimiter = 
   splitOn <$> strOption (metavar "DELIM" <> short 'd' <> help "Input field delimiter. Default is TAB (\\t).")

whiteSpaceDelimiter = flag' 
    words
    (short 's' <> help "Use any run of whitespace as input field delimiter")

parseMaxWidth = read <$> strOption (
      value "0" <> short 'w' <> metavar "WIDTH" 
      <> help "Max table width. Defaults to value of `tput cols` command.")

opts = info (helper <*> parseOptions)
            (fullDesc 
              <> progDesc "Pretty format TSV input into table with aligned and wrapped cells"
              <> header "table"
              <> footer "https://github.com/danchoi/table")

main = do 
  Options {..} <- execParser opts
  s <- getContents 
  let initialWidths = getCellWidths . map splitter . lines $ s 
  -- Adjust the max width subtract padding for gutters on side and ' | ' between cells
  maxWidth' <- do 
      if maxWidth == 0
      then (read <$> readProcess "tput" ["cols"] [])
      else (return maxWidth)
  let adjMaxWidth = maxWidth' - 2 - ((length initialWidths - 1) * 3)
  let adjustedWidths = adjustWidths adjMaxWidth initialWidths
  let rows  = map splitter . lines $ s
  let rows' = mkCells adjustedWidths rows
  mapM_ (\row -> do
      when (not suppressRowDividers) $ 
         putStrLn $ printDivider 1 $ map width row
      putStrLn . printRow 1 $ row 
    ) rows'

adjustWidths :: Int -> [Int] -> [Int]
adjustWidths maxWidth xs | sum xs <= maxWidth = xs
                    | otherwise     = adjustWidths maxWidth $ reduceWidest xs

reduceWidest :: [Int] -> [Int]
reduceWidest xs = let m = maximum xs
                  in [ if x == m then x - 1 else x | x <- xs ]

data Cell = Cell {
    content :: [String]
  , width :: Int
  , height :: Int 
  , isNumeric :: Bool
  } deriving (Show)

-- | get initial column widths
getCellWidths :: [[String]] -> [Int]
getCellWidths rows = map (maximum . map length) . transpose $ rows

{- Each row is represented as Cell, which contains dimension information. The
first value is the text content; the second is the normalized of the column
width for that cell.  -}

mkCells :: [Int] -> [[String]] -> [[Cell]]
mkCells columnWidths rows = 
    let cols     = transpose rows
        colCells = map (\(width, cell) -> addCellDimensions width cell) $ zip columnWidths cols 
    in transpose colCells

{- Input is a column of strings. Wraps data in a Cell, which adds width and
height to each cell in a column, and also a flag if the column looks numeric,
which determines the alignment.  Also wraps stings to max cell width -} 

addCellDimensions :: Int -> [String] -> [Cell]
addCellDimensions maxWidth xs = 
  let w = min (maximum . map length $ xs) maxWidth
      xs' = map (wrapString w) xs       -- wrapped string content
      numeric = all (all isDigit) (if length xs > 1 then (tail xs) else xs)
  in map (\lines -> Cell lines w (length lines) numeric) xs'

wrapString :: Int -> String -> [String]
wrapString maxWidth x = map trim . wrapLine maxWidth $ x

-- | prints a row of cells with dividers
--   gutter is the width of the blank space at left and right of table
printRow :: Int -> [Cell] -> String
printRow gutter xs = 
    let rowHeight = maximum $ map height xs
        subcells :: [[String]]
        subcells = map content xs
        lines = map (\n -> 
                       let ss :: [String] 
                           ss = map (cellLine n) xs
                       in formatRow ss)
                    [0..(rowHeight - 1)] 
    in mconcat $ intersperse "\n" $ lines
  where formatRow :: [String] -> String
        formatRow ss = 
            mconcat [margin gutter ' ' , (intercalate " | " ss) , margin gutter ' '] 

-- prints the nth line of a cell 
cellLine :: Int -> Cell -> String
cellLine n Cell {..} = 
      if n < length content 
      then printf fmt (content !! n)  
      else printf fmt ""
    where fmt | isNumeric = "%" ++ show width ++ "s"
              | otherwise = "%-" ++ show width ++ "s"

margin :: Int -> Char -> String
margin w c = take w $ repeat c

printDivider :: Int -> [Int] -> String
printDivider gutter widths = 
    mconcat [margin gutter '-'
      , (intercalate "-+-" 
        $ map (\w -> take w $ repeat '-') widths)
      , margin gutter '-']


------------------------------------------------------------------------
-- Word wrapping
-- taken from http://moreindirection.blogspot.com/2010/08/blog-post.html


trim :: String -> String
trim = trimAndReverse . trimAndReverse
  where trimAndReverse = reverse . dropWhile isSpace

reverseBreak :: (a -> Bool) -> [a] -> ([a], [a])
reverseBreak f xs = (reverse before, reverse after)
  where (after, before) = break f $ reverse xs

wrapLine :: Int -> String -> [String]
wrapLine maxLen line 
  | length line <= maxLen  = [line]
  | any isSpace beforeMax  = beforeSpace : (wrapLine maxLen $ afterSpace ++ afterMax)
  | otherwise              = beforeMax : wrapLine maxLen afterMax
    where (beforeMax, afterMax) = splitAt maxLen line
          (beforeSpace, afterSpace) = reverseBreak isSpace beforeMax

