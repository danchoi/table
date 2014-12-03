module Main where
import Data.List 
import Text.Printf

main = do 
    s <- getContents 
    let (header, rest) = table . map words . lines $ s
    putStrLn $ printRow header 
    mapM_ (putStrLn . printRow) rest

printRow :: [(String, Int)] -> String
printRow xs = intercalate " | " $ map printCell xs 

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


