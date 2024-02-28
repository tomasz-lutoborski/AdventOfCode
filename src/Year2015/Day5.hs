{-# LANGUAGE OverloadedStrings #-}

module Year2015.Day5 (run, dups) where

import Data.List (group, isInfixOf, sort)
import qualified Data.List.Split as LS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace as DBG

isNiceString :: T.Text -> Bool
isNiceString str = threeVowels && twiceLetter && notForbidden
  where
    threeVowels = T.length (T.filter (`T.elem` "aeiou") str) >= 3
    twiceLetter = any (uncurry (==)) $ T.zip str (T.tail str)
    notForbidden = not $ any (`T.isInfixOf` str) ["ab", "cd", "pq", "xy"]

dups :: (Eq a) => [a] -> [a]
dups [] = []
dups (x : xs) = filter (== x) xs ++ dups xs

generatePairs :: String -> [((Char, Char), Int)]
generatePairs str = [((str !! i, str !! (i + 1)), i) | i <- [0 .. length str - 2]]

containsNonOverlaps :: [((Char, Char), Int)] -> Bool
containsNonOverlaps pairs = any hasDuplicates pairs
  where
    hasDuplicates ((a, b), idx1) = any (\((c, d), idx2) -> (a == c) && (b == d) && (abs idx1 - idx2) > 1) pairs

isNicerString :: String -> Bool
isNicerString str = DBG.trace ("letter twice: " <> show letterTwice <> "  two pairs: " <> show twoPairs) letterTwice && twoPairs
  where
    pairs = generatePairs str
    twoPairs = containsNonOverlaps pairs
    (str1, str2) = unzipString str
    letterTwice = any (\g -> length g > 1) (group str1) || any (\g -> length g > 1) (group str2)

unzipString :: String -> (String, String)
unzipString = foldr (\c (evens, odds) -> (odds, c : evens)) ([], [])

countNice :: T.Text -> Int
countNice = length . filter isNiceString . T.lines

countNicer :: String -> Int
countNicer = length . filter isNicerString . lines

run :: IO ()
run = do
  fileContent <- readFile "inputs/Year2015/Day5.txt"

  print $ countNicer fileContent

-- print $ isNicerString "qjhvhtzxzqqjkmpb"
-- print $ isNicerString "xxyxx"
-- print $ isNicerString "uurcxstgmygtbstg"
-- print $ isNicerString "ieodomkazucvgmuy"