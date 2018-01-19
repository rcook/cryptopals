module Cryptopals.ByteStringUtil
    ( chunksOf
    , pkcs7Pad
    , pkcs7Unpad
    , xor
    ) where

import qualified Data.Bits as Bits (xor)
import           Data.Char (chr)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (append, last, length, pack, splitAt, take, zipWith)
import qualified Data.ByteString.Char8 as Char8 (replicate)
import           Data.List (unfoldr)

padding :: Int -> Int -> Int
padding chunkSize n = let r = n `mod` chunkSize in if r == 0 then 0 else chunkSize - r

pkcs7Pad :: Int -> ByteString -> ByteString
pkcs7Pad chunkSize s =
    let n = ByteString.length s
        padValue = padding chunkSize n
    in ByteString.append s (Char8.replicate padValue (chr padValue))

pkcs7Unpad :: Int -> ByteString -> ByteString
pkcs7Unpad chunkSize s =
    let n = ByteString.length s
    in if n == 0
        then s
        else
            let padValue = fromIntegral $ ByteString.last s
            in if padValue < 1 || padValue >= chunkSize then s else ByteString.take (n - padValue) s

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf chunkSize =
    unfoldr
        (\bytes ->
            let (c, t) = ByteString.splitAt chunkSize bytes
            in if ByteString.length c == 0 then Nothing else Just (c, t))

xor :: ByteString -> ByteString -> ByteString
xor s0 s1 = ByteString.pack $ ByteString.zipWith Bits.xor s0 s1
