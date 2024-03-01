module Year2017.Day2 (run) where

import Data.List (foldl', sort)

parseInput :: String -> [[Int]]
parseInput str = fmap (fmap read . words) (lines str)

checksum :: [[Int]] -> Int
checksum = foldl' (\total row -> total + (maximum row - minimum row)) 0

checksum2 :: [[Int]] -> Int
checksum2 = foldl' (\total row -> total + getRow row) 0
  where
    getRow row = case [(x, y) | x <- row, y <- row, x /= y, x `mod` y == 0] of
      (pair : _) -> uncurry div pair
      _ -> error "not found"

run :: IO ()
run = do
  content <- readFile "inputs/Year2017/Day2.txt"
  print $ checksum2 $ parseInput content