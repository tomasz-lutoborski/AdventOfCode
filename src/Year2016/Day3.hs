module Year2016.Day3 where

import Data.List (sort)
import Data.List.Split (chunksOf)

newtype Triangle = Triangle (Int, Int, Int) deriving (Show, Eq)

isRightTriangle :: Triangle -> Bool
isRightTriangle triangle = head sorted + sorted !! 1 > sorted !! 2
  where
    Triangle (sideA, sideB, sideC) = triangle
    sorted = sort [sideA, sideB, sideC]

parseTriangles :: String -> [Triangle]
parseTriangles input = fmap parseTriangle strTriangles
  where
    strTriangles = words <$> lines input
    parseTriangle [sideA, sideB, sideC] = Triangle (read sideA, read sideB, read sideC)
    parseTriangle _ = error "bad triangle"

parseTrianglesByColumn :: String -> [Triangle]
parseTrianglesByColumn input = fmap parseTriangle trianglesByColumn
  where
    trianglesByLine = words <$> lines input
    trianglesByColumn = chunksOf 3 $ concat [fmap head trianglesByLine, fmap (!! 1) trianglesByLine, fmap (!! 2) trianglesByLine]
    parseTriangle [sideA, sideB, sideC] = Triangle (read sideA, read sideB, read sideC)
    parseTriangle _ = error "bad triangle"

countRightTriangles :: [Triangle] -> Int
countRightTriangles = length . filter isRightTriangle

run :: IO ()
run = do
  content <- readFile "inputs/Year2016/Day3.txt"
  print $ countRightTriangles $ parseTrianglesByColumn content