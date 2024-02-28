{-# LANGUAGE OverloadedStrings #-}

module Year2015.Day4 where

import Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Digest.Pure.MD5 (MD5Digest, md5)

prefix :: BSL.ByteString
prefix = "bgvyzdsv"

generateAndHash :: Int -> (Int, MD5Digest)
generateAndHash n = (n, md5 $ BSL.append prefix (BSL.pack $ show n))

startsWithFiveZeros :: MD5Digest -> Bool
startsWithFiveZeros digest = "000000" `BSL.isPrefixOf` BSL.pack (show digest)

findMatchingHashes :: [(Int, MD5Digest)]
findMatchingHashes = Prelude.filter (\(_, hash) -> startsWithFiveZeros hash) $ fmap generateAndHash [1 .. 10000000]

run :: IO ()
run = do
  print findMatchingHashes