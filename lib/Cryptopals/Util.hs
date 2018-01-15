--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module Cryptopals.Util
    ( byteStringChunksOf
    , byteStringPadPKCS7Unsafe
    , byteStringUnpadPKCS7Unsafe
    , byteStringXor
    , isPrintOrSpace
    , mostCommonChar
    ) where

import           Cryptopals.Prelude
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (append, last, length, pack, replicate, splitAt, take, zipWith)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

scoreText :: String -> HashMap Char Int
scoreText =
    foldr
        (\c m -> let k = c in HashMap.insertWith (\_ x -> x + 1) k 1 m)
        HashMap.empty

mostCommonChar :: String -> Maybe Char
mostCommonChar [] = Nothing
mostCommonChar s =
    let (c, _) = HashMap.foldlWithKey' (\p@(_, n) c' n' -> if n' > n then (c', n') else p) (' ', -1) (scoreText s)
    in Just c

isPrintOrSpace :: Char -> Bool
isPrintOrSpace x = isPrint x || isSpace x

byteStringChunksOf :: Int -> ByteString -> [ByteString]
byteStringChunksOf chunkSize =
    unfoldr
        (\bytes ->
            let (c, t) = ByteString.splitAt chunkSize bytes
            in if ByteString.length c == 0 then Nothing else Just (c, t))

byteStringXor :: ByteString -> ByteString -> ByteString
byteStringXor prev plaintext = ByteString.pack $ ByteString.zipWith xor prev plaintext

byteStringPadPKCS7Unsafe :: Int -> ByteString -> ByteString
byteStringPadPKCS7Unsafe chunkSize bytes =
    let padCount = chunkSize - ByteString.length bytes
        padByte = fromIntegral padCount
    in ByteString.append bytes (ByteString.replicate padCount padByte)

byteStringUnpadPKCS7Unsafe :: Int -> ByteString -> ByteString
byteStringUnpadPKCS7Unsafe chunkSize bytes =
    let padByte = ByteString.last bytes
        padCount = fromIntegral padByte
    in if padCount < 0 || padCount >= chunkSize
        then bytes
        else ByteString.take (chunkSize - padCount) bytes
