{-# LANGUAGE NamedFieldPuns #-}

module Year2016.Day1 (run) where

import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Debug.Trace (trace)

data Direction = N | S | E | W deriving (Show, Eq)

data Turn = L | R deriving (Show, Eq)

data Instruction = Instruction
  { turn :: Turn,
    steps :: Int
  }
  deriving (Eq, Show)

data Position = Position
  { x :: Int,
    y :: Int,
    direction :: Direction
  }
  deriving (Eq, Show)

data Location = Location
  { locX :: Int,
    locY :: Int
  }
  deriving (Eq, Show)

parseInstructions :: String -> [Instruction]
parseInstructions str = parseInstruction <$> splitOn ", " str
  where
    parseInstruction ('L' : n) = Instruction {turn = L, steps = read n}
    parseInstruction ('R' : n) = Instruction {turn = R, steps = read n}
    parseInstruction _ = error "wrong input"

updatePosition :: Position -> Instruction -> Position
updatePosition (Position {x, y, direction}) (Instruction {turn, steps}) = case turn of
  L -> case direction of
    N -> Position {x = x - steps, y = y, direction = W}
    S -> Position {x = x + steps, y = y, direction = E}
    W -> Position {x = x, y = y - steps, direction = S}
    E -> Position {x = x, y = y + steps, direction = N}
  R -> case direction of
    N -> Position {x = x + steps, y = y, direction = E}
    S -> Position {x = x - steps, y = y, direction = W}
    W -> Position {x = x, y = y + steps, direction = N}
    E -> Position {x = x, y = y - steps, direction = S}

moveSanta :: Position -> [Instruction] -> Position
moveSanta = foldl' updatePosition

findFirstRep :: Position -> [Instruction] -> Position
findFirstRep pos instrs = fst $ foldl' updatePositionWithVisited (pos, []) instrs
  where
    updatePositionWithVisited :: (Position, [Location]) -> Instruction -> (Position, [Location])
    updatePositionWithVisited (currPos, visited) instr =
      let newPos = updatePosition currPos instr
          newVisited = case direction newPos of
            N -> [Location {locX = x currPos, locY = y'} | y' <- [(y currPos + 1) .. (y newPos)]]
            S -> [Location {locX = x currPos, locY = y'} | y' <-  [(y newPos) .. (y currPos - 1)]]
            E -> [Location {locX = x', locY = y currPos} | x' <- [(x currPos + 1) .. (x newPos)]]
            W -> [Location {locX = x', locY = y currPos} | x' <-  [(x newPos) .. (x currPos - 1)]]
          -- Check for repeats during the move
          repeated = dropWhile (`notElem` visited) newVisited
       in if not (null repeated)
            then (currPos, visited) -- Found a repeat; stop updating.
            else (newPos, newVisited ++ visited) -- No repeat; update position and visited list.
run :: IO ()
run = do
  content <- readFile "inputs/Year2016/Day1.txt"
  -- let content = "R4, R2, R3, R8"
  print $ findFirstRep Position {x = 0, y = 0, direction = N} $ parseInstructions content