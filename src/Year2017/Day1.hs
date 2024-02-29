module Year2017.Day1 (run) where

import Data.Char (digitToInt)
import Data.List (foldl')
import Debug.Trace (trace)

sumInput :: String -> Int
sumInput str = foldl' (\total ch -> digitToInt ch + total) 0 nextMatch
  where
    zipped = zip str (tail str <> [head str])
    nextMatch = fst <$> filter (uncurry (==)) zipped

sumInput2 :: String -> Int
sumInput2 str = trace nextMatch $ foldl' (\total ch -> digitToInt ch + total) 0 nextMatch
  where
    splitted = splitAt (length str `div` 2) str
    zipped = zip str (snd splitted <> fst splitted)
    nextMatch = fst <$> filter (uncurry (==)) zipped

run :: IO ()
run = do
  content <- readFile "inputs/Year2017/Day1.txt"
  -- let content = "1212"
  print $ sumInput2 content