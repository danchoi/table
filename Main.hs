{-# LANGUAGE RecordWildCards #-} 
module Main where
import Data.List 
import Data.Monoid
import Text.Printf
import Options.Applicative
import Data.List.Split (splitOn)
import Control.Applicative
import Data.Char (isDigit)

-- TODO change the delimiter to tabs or whitespace

data Options = Options {
    inputDelimiter :: Maybe String -- defaults to any whitespace
  } deriving (Show)

parseOptions :: Parser Options
parseOptions = Options
  <$> (setDelimiter <|> tabDelimiter)

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
  Options delim <- execParser opts
  let splitter = case delim of 
                    Nothing -> words
                    Just d -> splitOn d
  s <- getContents 
  let (header:rest) =  cells . map splitter . lines $ s
  putStrLn $ printRow 1 header 
  putStrLn $ printDivider 1 $ map width header
  mapM_ (putStrLn . printRow 1) rest


data Cell = Cell {
    content :: [String]
  , width :: Int
  , height :: Int 
  , isNumeric :: Bool
  } deriving (Show)

-- | prints a row of cells with dividers
printRow :: Int -> [Cell] -> String
printRow gutter xs = 
    mconcat [ margin gutter ' '
      , (intercalate " | " $ map printCell xs )
      , margin gutter ' '] 

margin :: Int -> Char -> String
margin w c = take w $ repeat c

printDivider :: Int -> [Int] -> String
printDivider gutter widths = 
    mconcat [margin gutter '-'
      , (intercalate "-+-" 
        $ map (\w -> take w $ repeat '-') widths)
      , margin gutter '-']

printCell :: Cell  -> String
printCell Cell {..} = printf fmt (head content)  
    where fmt | isNumeric = "%" ++ show width ++ "s"
              | otherwise = "%-" ++ show width ++ "s"

{- 
  Given a 2 dimensional table, generates a tuple:

  One of the header, the rest for the rest of the values.  Each row is
  represented as Cell, which contains dimension information. The first value
  is the text content; the second is the normalized of the column width for
  that cell. 
-}

cells :: [[String]] -> [[ Cell ]]
cells = transpose . map addCellDimensions . transpose 

-- | Input is a column. Add width and height to each cell in a column
addCellDimensions :: [String] -> [Cell]
addCellDimensions xs = 
  let w = maximum . map length $ xs
      h = 1 -- temporary
      -- if all the cells except the 1st (which could be a header) are numeric,
      -- flag cells as isNumeric
      numeric = all (all isDigit) (tail xs) 
  in map (\x -> Cell [x] w h numeric) xs


