module Main where
import Data.List 
import Data.Monoid
import Text.Printf
import Options.Applicative
import Data.List.Split (splitOn)
import Control.Applicative

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
  putStrLn $ printDivider 1 $ map (\(_,w,_) -> w) header
  mapM_ (putStrLn . printRow 1) rest

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

-- TODO 
printCell :: Cell  -> String
printCell (xs, width, height) = printf fmt (head xs) -- change head
    where fmt = "%" ++ show width ++ "s"

-- assume for now a header row and rest
{- Given a 2 dimensional table, generates a tuple:

    One of the header, the rest for the rest of the values.
    Each row is represented as [(String, Int)]. The first
    value is the text content; the second is the normalized of the column width
    for that cell. 
-}
type Cell = ([String], Int, Int)

-- | Generate cells with array or rows, normalized column widths and height (num rows):
cells :: [[String]] -> [[ Cell ]]
cells = transpose . map addCellDimensions . transpose 

-- | Add width and height to each cell in a column
addCellDimensions :: [String] -> [Cell]
addCellDimensions xs = 
  let width = maximum . map length $ xs
      height = 1 -- temporary
  in map (\x -> ([x], width, height)) xs


