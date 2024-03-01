{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2018.Day1 (run) where

import Data.List (foldl')
import qualified Data.Set as S

data Operand = Add | Sub deriving (Show)

data Operation = Operation
  { value :: Int,
    operand :: Operand
  }
  deriving (Show)

parseOp :: String -> Operation
parseOp str = case head str of
  '+' -> Operation {value = read (tail str), operand = Add}
  '-' -> Operation {value = read (tail str), operand = Sub}
  _ -> error "forbidden operation"

parseOps :: String -> [Operation]
parseOps = fmap parseOp . lines

getResult :: String -> Int
getResult str =
  foldl'
    ( \acc op -> case operand op of
        Add -> value op + acc
        Sub -> acc - value op
    )
    0
    (parseOps str)

getFirstTwice :: [Operation] -> Int
getFirstTwice ops = go 0 S.empty (cycle ops)
  where
    go :: Int -> S.Set Int -> [Operation] -> Int
    go currentFreq seen (op : ops') =
      let nextFreq = case operand op of
            Add -> currentFreq + value op
            Sub -> currentFreq - value op
       in if S.member nextFreq seen
            then nextFreq
            else go nextFreq (S.insert currentFreq seen) ops'

run :: IO ()
run = do
  content <- readFile "inputs/Year2018/Day1.txt"
  print $ getFirstTwice $ parseOps content