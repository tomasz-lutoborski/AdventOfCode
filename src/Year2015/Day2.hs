{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Year2015.Day2 where

import Data.Foldable (foldl')
import Data.List (sort)
import Data.Text (pack, split, unpack)
import Prelude hiding (length)

data Box = Box
  { height :: Int,
    width :: Int,
    length :: Int
  }
  deriving (Eq, Show)

parseBoxes :: String -> [Box]
parseBoxes input = fmap parseBox (lines input)
  where
    parseBox line =
      let [h, w, l] = (unpack <$> split (== 'x') (pack line))
       in Box {height = read h, width = read w, length = read l}

calculateNeededPaper :: [Box] -> Int
calculateNeededPaper = foldl' paperForBox 0
  where
    paperForBox acc Box {height, width, length} = acc + 2 * sideOne + 2 * sideTwo + 2 * sideThree + minimum [sideOne, sideTwo, sideThree]
      where
        sideOne = height * width
        sideTwo = width * length
        sideThree = height * length

calculateNeededRibbon :: [Box] -> Int
calculateNeededRibbon = foldl' ribbonForBox 0
  where
    ribbonForBox acc Box {height, width, length} = 2 * shortest + 2 * short + height * width * length + acc
      where
        sorted = sort [height, width, length]
        shortest = head sorted
        short = sorted !! 1

run :: IO ()
run = do
  content <- readFile "inputs/Year2015/Day2.txt"
  print $ show $ calculateNeededRibbon $ parseBoxes content