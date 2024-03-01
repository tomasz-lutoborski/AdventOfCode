module Year2017.Day4 (run) where

import Data.List (sort)
import Data.Set (fromList)

countValidPassphrases :: String -> Int
countValidPassphrases input = length $ filter (\line -> length line == length (fromList line)) (words <$> lines input)

countNonAnagrams :: String -> Int
countNonAnagrams input = length $ filter (\line -> length line == length (fromList line)) (fmap sort <$> (words <$> lines input))

run :: IO ()
run = do
  content <- readFile "inputs/Year2017/Day4.txt"
  print $ countNonAnagrams content
