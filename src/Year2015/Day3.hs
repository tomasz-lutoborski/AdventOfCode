{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2015.Day3 where

import Data.Foldable (foldl')
import qualified Data.Map as Map
import qualified Data.Text as T

type Location = (Int, Int)

type Visits = Map.Map Location Int

move :: Char -> Location -> Location
move 'v' (x, y) = (x, y - 1)
move '^' (x, y) = (x, y + 1)
move '>' (x, y) = (x + 1, y)
move '<' (x, y) = (x - 1, y)
move _ _ = error "wrong direction"

moveWithRobot :: T.Text -> (Location, Location) -> (Location, Location)
moveWithRobot dirs (santaLoc, robotLoc) = case T.unpack dirs of
  [santaDir, robotDir] -> (move santaDir santaLoc, move robotDir robotLoc)
  [santaDir] -> (move santaDir santaLoc, robotLoc) -- In case of odd number of moves
  _ -> (santaLoc, robotLoc) -- Should not happen

runSanta :: String -> Visits
runSanta = snd . foldl' visit ((0, 0), Map.singleton (0, 0) 1)
  where
    visit (location, visits) dir =
      let newLocation = move dir location
       in (newLocation, Map.insertWith (+) newLocation 1 visits)

runSantaWithRobot :: T.Text -> Visits
runSantaWithRobot input = sel3 $ foldl' visit ((0, 0), (0, 0), Map.singleton (0, 0) 2) (T.chunksOf 2 input)
  where
    visit (santaLocation, robotLocation, visits) dir =
      let (newSantaLocation, newRobotLocation) = moveWithRobot dir (santaLocation, robotLocation)
       in (newSantaLocation, newRobotLocation, updateVisits newSantaLocation newRobotLocation visits)
    updateVisits santaLocation robotLocation visits =
      let withSantaVisit = Map.insertWith (+) santaLocation 1 visits
       in Map.insertWith (+) robotLocation 1 withSantaVisit
    sel3 (_, _, x) = x

run :: IO ()
run = do
  content <- readFile "inputs/Year2015/Day3.txt"
  print $ show $ Map.size $ runSantaWithRobot $ T.pack content