{-# LANGUAGE OverloadedStrings #-}

module Year2016.Day5 (run) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (digitToInt, isDigit)
import Data.Digest.Pure.MD5 (MD5Digest, md5)

prefix :: BSL.ByteString
prefix = "wtnhxymk"

generateAndHash :: Int -> (Int, String)
generateAndHash n = (n, show $ md5 $ BSL.append prefix (BSL.pack $ show n))

startsWithFiveZeros :: String -> Bool
startsWithFiveZeros digest = "00000" `BSL.isPrefixOf` BSL.pack digest

findMatchingHashes :: [(Int, String)]
findMatchingHashes = Prelude.filter (\(_, hash) -> startsWithFiveZeros hash) $ fmap generateAndHash [1 .. 10000000000000]

getPassword :: [(Int, String)] -> [(Char, Char)]
getPassword hashes = filter (\(loc, _) -> isDigit loc && digitToInt loc < 8) partsOfPassword
  where
    partsOfPassword = fmap (\(_, hash) -> (hash !! 5, hash !! 6)) hashes

run :: IO ()
run = do
  print $ take 50 $ getPassword findMatchingHashes