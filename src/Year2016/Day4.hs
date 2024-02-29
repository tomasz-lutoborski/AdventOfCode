module Year2016.Day4 (run) where

import Data.List (elemIndex, foldl', group, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

data Room = Room
  { letters :: String,
    hash :: String,
    roomId :: Int
  }
  deriving (Show, Eq)

parseRoom :: String -> Room
parseRoom str =
  Room
    { letters = parsedLetters,
      hash = parsedHash,
      roomId = parsedRoomId
    }
  where
    parsedLetters = concat $ init $ splitOn "-" $ head (splitOn "[" str)
    parsedRoomId = read $ last $ splitOn "-" $ head (splitOn "[" str)
    parsedHash = init $ last (splitOn "[" str)

parseRooms :: String -> [Room]
parseRooms input = fmap parseRoom roomLines
  where
    roomLines = lines input

sortStringsDesc :: [String] -> [String]
sortStringsDesc = sortBy compareStringsDesc
  where
    compareStringsDesc a b = case comparing (negate . length) a b of
      EQ -> compare a b
      other -> other

isRoomReal :: Room -> Bool
isRoomReal room = hash room == orderedLetters
  where
    orderedLetters = fmap head $ take 5 $ sortStringsDesc $ group $ sort $ letters room

addRealIds :: [Room] -> Int
addRealIds = foldl' addRealId 0
  where
    addRealId count room = if isRoomReal room then count + roomId room else count

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

shiftLetter :: Char -> Int -> Char
shiftLetter ch n = alphabet !! mod (n + fromMaybe 0 (elemIndex ch alphabet)) (length alphabet)

decryptLetter :: Int -> Char -> Char
decryptLetter n ch
  | ch `elem` alphabet = shiftLetter ch n
  | ch == '-' = ' '
  | otherwise = error "unknown character"

decryptString :: String -> Int -> String
decryptString str shiftN = fmap (decryptLetter shiftN) str

decryptRooms :: [Room] -> [String]
decryptRooms = fmap decryptRoom
  where
    decryptRoom room = decryptString (letters room) (roomId room)

run :: IO ()
run = do
  content <- readFile "inputs/Year2016/Day4.txt"
  print $ decryptRooms $ parseRooms content