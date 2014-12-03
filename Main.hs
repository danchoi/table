module Main where
import Data.List 
import Data.Monoid
import Text.Printf

main = do 
    s <- getContents 
    let (header, rest) = table . map words . lines $ s
    putStrLn $ printRow 1 header 
    putStrLn $ printDivider 1 (map snd header)
    mapM_ (putStrLn . printRow 1) rest

printRow :: Int -> [(String, Int)] -> String
printRow gutter xs = 
    mconcat [ margin gutter ' '
      , (intercalate " | " $ map printCell xs )
      , margin gutter ' '] 

margin :: Int -> Char -> String
margin w c = take w $ repeat c

printDivider :: Int -> [Int] -> String
printDivider gutter widths = 
    mconcat [margin gutter '-'
      , (intercalate "+" 
        $ map (\w -> take (w + 1) $ repeat '-') widths)
      , margin gutter '-']


printCell :: (String, Int)  -> String
printCell (x, width) = printf fmt x 
    where fmt = "%" ++ show width ++ "s"


-- assume for now a header row and rest

table :: [[String]] -> ([(String, Int)], [[(String, Int)]])
table xs = 
  let xs' = cells xs
      header = head xs'
      rest = tail xs'
  in (header, rest)

-- generate cells with built-in widths:
cells :: [[String]] -> [[(String, Int)]]
cells = transpose . map colw . transpose 

-- add width to each cell in a column
colw :: [String] -> [(String, Int)]
colw xs = let width = maximum . map length $ xs
  in map (\x -> (x, width)) xs


