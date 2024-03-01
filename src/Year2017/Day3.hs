module Year2017.Day3 (run) where

import Data.List (foldl')
import Data.Map (Map, findWithDefault, fromList, insert, lookup, singleton)
import Data.Maybe (fromMaybe)

start :: Int
start = 347991

memory = [1, 1, 2, 4, 5, 10, 11, 23, 25, 26, 54, 57, 59, 122, 133, 142, 147, 304, 330, 351, 362, 747, 806]

layers :: [(Int, Int)]
layers = zip [1 ..] [x | i <- [0 ..], let x = i ^ 2, odd i]

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x : xs) =
  x
    : if p x
      then takeWhileInclusive p xs
      else []

findNOfMoves :: Int -> Int
findNOfMoves n = dist
  where
    neededLayers = takeWhileInclusive (\(_, biggest) -> biggest < n) layers
    lastLayer = last neededLayers
    sLastLayer = last (init neededLayers)
    diffTwoLastMax = snd lastLayer - snd sLastLayer
    stepToCorner = diffTwoLastMax `div` 4
    distFromSecLast = n - snd sLastLayer
    modDist = distFromSecLast `mod` stepToCorner
    distFromCorner = if modDist > (stepToCorner `div` 2) then stepToCorner - modDist else modDist
    dist = (fst lastLayer - 1) + (stepToCorner `div` 2 - distFromCorner)

data Direction = E | W | N | S deriving (Eq, Show)

newtype Location = Location (Int, Int) deriving (Eq, Show, Ord)

data Position = Position
  { location :: Location,
    direction :: Direction
  }
  deriving (Eq, Show)

generateSpiral :: Int -> Map Location Int
generateSpiral finalValue = go (Position {location = Location (0, 0), direction = E}) 1 (singleton (Location (0, 0)) 1)
  where
    go pos currValue grid =
      let newPos = updatePosition pos
          newGrid = insert (location newPos) (sumAdjacents (getAdjacentLocs (location newPos)) grid) grid
       in if currValue > finalValue then newGrid else go newPos (fromMaybe 0 (Data.Map.lookup (location newPos) newGrid)) newGrid

getAdjacentLocs :: Location -> [Location]
getAdjacentLocs (Location (x, y)) = [Location (i, j) | i <- [x - 1 .. x + 1], j <- [y - 1 .. y + 1], (i, j) /= (x, y)]

sumAdjacents :: [Location] -> Map Location Int -> Int
sumAdjacents adjs grid = foldl' (\acc loc -> acc + findWithDefault 0 loc grid) 0 adjs

updatePosition :: Position -> Position
updatePosition pos =
  let Location loc@(x, y) = location pos
      dir = if abs x == abs y then updateDirection (direction pos) else direction pos
      newLoc = if (y <= 0) && x >= 0 && abs x == abs y then Location (x + 1, y) else updateLocation dir (Location loc)
   in Position {location = newLoc, direction = dir}

updateLocation :: Direction -> Location -> Location
updateLocation dir loc =
  let Location (x, y) = loc
   in case dir of
        N -> Location (x, y + 1)
        E -> Location (x + 1, y)
        S -> Location (x, y - 1)
        W -> Location (x - 1, y)

updateDirection :: Direction -> Direction
updateDirection dir = case dir of
  E -> N
  N -> W
  W -> S
  S -> E

run :: IO ()
run = do
  print $ findNOfMoves start