module Year2015.Day1 where

import Data.Foldable (foldl')

findFloor :: String -> String
findFloor input = show $ foldl' (\curr next -> if next == '(' then curr + 1 else curr - 1) 0 input

findWhereBasement :: String -> String
findWhereBasement input = show $ foldl' update (0, 0) input
  where
    update acc@(curr, pos) _ | curr == -1 = (curr, pos)
    update (curr, pos) '(' = (curr + 1, pos + 1)
    update (curr, pos) ')' = (curr - 1, pos + 1)
    update (curr, pos) _ = (curr, pos + 1)

run :: IO ()
run = do
  contents <- readFile "inputs/Year2015/Day1.txt"
  putStrLn $ findWhereBasement contents
