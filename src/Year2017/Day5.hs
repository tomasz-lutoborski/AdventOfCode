module Year2017.Day5 (run) where

import Data.Vector (Vector, fromList, (!), (!?), (//))
import Debug.Trace (trace)

parseInstructions :: String -> Data.Vector.Vector Int
parseInstructions = fmap read . (fromList . lines)

escapeMaze :: Data.Vector.Vector Int -> Int
escapeMaze = go 0 0
  where
    go steps pos maze' =
      let offset = maze' ! pos
       in case maze' !? (pos + offset) of
            Just _ -> go (steps + 1) (pos + offset) (maze' // [(pos, if offset >= 3 then offset - 1 else offset + 1)])
            Nothing -> steps + 1

run :: IO ()
run = do
  content <- readFile "inputs/Year2017/Day5.txt"
  -- let content = "0\n3\n0\n1\n-3"
  print $ escapeMaze $ parseInstructions content