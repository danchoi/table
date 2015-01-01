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

-- TODO change the delimiter to tabs or whitespace

data Options = Options {
    inputDelimiter :: Maybe String -- defaults to any whitespace
  , printRowDividers :: Bool 
  } deriving (Show)

parseOptions :: Parser Options
parseOptions = Options
  <$> (setDelimiter <|> tabDelimiter)
  <*> switch (short 'r' <> help "Print row dividers")

setDelimiter = optional 
  $ strOption 
      (metavar "DELIM" <> short 'd' <> help "Input field delimiter. Defaults to whitespace.")

tabDelimiter = flag' 
    (Just "\t")
    (short 't' <> help "Use tab as input field delimiter: a shortcut for -d $'\t'.")

opts = info (helper <*> parseOptions)
            (fullDesc 
              <> progDesc "Pretty format TSV input into table with aligned and wrapped cells"
              <> header "table"
              <> footer "https://github.com/danchoi/table")

main = do 
  Options delim rowDivide <- execParser opts
  let splitter = case delim of 
                    Nothing -> words
                    Just d -> splitOn d
  s <- getContents 
  let maxWidth = 35 -- CHANGE
  let rows =  cells maxWidth . map splitter . lines $ s
  mapM_ (\row -> do
      when rowDivide $ 
         putStrLn $ printDivider 1 $ map width row
      putStrLn . printRow 1 $ row 
    ) rows

data Cell = Cell {
    content :: [String]
  , width :: Int
  , height :: Int 
  , isNumeric :: Bool
  } deriving (Show)

-- | prints a row of cells with dividers
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

{- 
  Given a 2 dimensional table, generates a tuple:

  One of the header, the rest for the rest of the values.  Each row is
  represented as Cell, which contains dimension information. The first value
  is the text content; the second is the normalized of the column width for
  that cell. 
-}

cells :: Int -> [[String]] -> [[ Cell ]]
cells maxWidth = transpose . map (addCellDimensions maxWidth) . transpose 

{- Input is a column of strings. Wraps data in a Cell, which adds width and
height to each cell in a column, and also a flag if the column looks numeric,
which determines the alignment.  Also wraps stings to max cell width -} 

addCellDimensions :: Int -> [String] -> [Cell]
addCellDimensions maxWidth xs = 
  let w = min (maximum . map length $ xs) maxWidth
      xs' = map (wrapString w) xs       -- wrapped string content
      numeric = all (all isDigit) (tail xs) 
  in map (\lines -> Cell lines w (length lines) numeric) xs'

wrapString :: Int -> String -> [String]
wrapString maxWidth x = map trim . wrapLine maxWidth $ x

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

