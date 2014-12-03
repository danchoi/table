module Main where
import Data.List 

main = do 
    s <- getContents 
    let res = table . map words . lines $ s
    print res

table :: [[String]] -> [[(String, Int)]]
table = cells

-- generate cells with built-in widths:
cells :: [[String]] -> [[(String, Int)]]
cells = transpose . map colw . transpose 

-- add width to each cell in a column
colw :: [String] -> [(String, Int)]
colw xs = let width = maximum . map length $ xs
  in map (\x -> (x, width)) xs


