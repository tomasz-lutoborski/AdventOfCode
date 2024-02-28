module Year2016.Day2 where

import Data.List (foldl')
import Debug.Trace (trace)
import Prelude hiding (Left, Right)

keypad :: [[Int]]
keypad = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

newKeypad :: [[Char]]
newKeypad = [['0', '0', '1', '0', '0'], ['0', '2', '3', '4', '0'], ['5', '6', '7', '8', '9'], ['0', 'A', 'B', 'C', '0'], ['0', '0', 'D', '0', '0']]

newtype Position = Position (Int, Int) deriving (Eq, Show)

data Move = Up | Left | Right | Down deriving (Eq, Show)

updatePosition :: Position -> Move -> Position
updatePosition (Position (x, y)) move = case move of
  Up -> Position (x, if y > 0 then y - 1 else y)
  Down -> Position (x, if y < 2 then y + 1 else y)
  Left -> Position (if x > 0 then x - 1 else x, y)
  Right -> Position (if x < 2 then x + 1 else x, y)

updateNewPosition :: Position -> Move -> Position
updateNewPosition (Position (x, y)) move = case move of
  Up -> case (x, y) of
    (2, 1) -> Position (2, 0)
    (1, 2) -> Position (1, 1)
    (2, 2) -> Position (2, 1)
    (3, 2) -> Position (3, 1)
    (1, 3) -> Position (1, 2)
    (2, 3) -> Position (2, 2)
    (3, 3) -> Position (3, 2)
    (2, 4) -> Position (2, 3)
    _ -> Position (x, y)
  Down -> case (x, y) of
    (2, 0) -> Position (2, 1)
    (1, 1) -> Position (1, 2)
    (2, 1) -> Position (2, 2)
    (3, 1) -> Position (3, 2)
    (1, 2) -> Position (1, 3)
    (2, 2) -> Position (2, 3)
    (3, 2) -> Position (3, 3)
    (2, 3) -> Position (2, 4)
    _ -> Position (x, y)
  Right -> case (x, y) of
    (0, 2) -> Position (1, 2)
    (1, 1) -> Position (2, 1)
    (1, 2) -> Position (2, 2)
    (1, 3) -> Position (2, 3)
    (2, 1) -> Position (3, 1)
    (2, 2) -> Position (3, 2)
    (2, 3) -> Position (3, 3)
    (3, 2) -> Position (4, 2)
    _ -> Position (x, y)
  Left -> case (x, y) of
    (4, 2) -> Position (3, 2)
    (3, 1) -> Position (2, 1)
    (3, 2) -> Position (2, 2)
    (3, 3) -> Position (2, 3)
    (2, 1) -> Position (1, 1)
    (2, 2) -> Position (1, 2)
    (2, 3) -> Position (1, 3)
    (1, 2) -> Position (0, 2)
    _ -> Position (x, y)

getFinalPosition :: Position -> [Move] -> Position
getFinalPosition = foldl' updatePosition

getNewFinalPosition :: Position -> [Move] -> Position
getNewFinalPosition = foldl' updateNewPosition

parseMoves :: String -> [Move]
parseMoves = fmap parseMove
  where
    parseMove ch
      | ch == 'U' = Up
      | ch == 'D' = Down
      | ch == 'L' = Left
      | ch == 'R' = Right
      | otherwise = error "bad input"

decode :: String -> String
decode input = snd $ foldl' decodeLine (Position (1, 1), "") (parseMoves <$> lines input)
  where
    decodeLine :: (Position, String) -> [Move] -> (Position, String)
    decodeLine (startPos, decoded) moves =
      let Position newPosition = getFinalPosition startPos moves
       in (Position newPosition, decoded <> show (keypad !! snd newPosition !! fst newPosition))

decodeNew :: String -> String
decodeNew input = snd $ foldl' decodeLine (Position (0, 2), "") (parseMoves <$> lines input)
  where
    decodeLine :: (Position, String) -> [Move] -> (Position, String)
    decodeLine (startPos, decoded) moves =
      let Position newPosition = getNewFinalPosition startPos moves
       in (Position newPosition, decoded <> show (newKeypad !! snd newPosition !! fst newPosition))

run :: IO ()
run = do
  content <- readFile "inputs/Year2016/Day2.txt"
  -- let content = "ULL\nRRDDD\nLURDL\nUUUUD"
  -- let content = "UUUUD"
  print $ decodeNew content