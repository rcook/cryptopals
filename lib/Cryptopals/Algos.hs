module Cryptopals.Algos
    ( decryptXORString
    , decryptAES128ECB
    , defaultIV
    , hamming
    , repeatingXOREncode
    ) where

import           Codec.Crypto.SimpleAES (Direction(..), Mode(..), crypt)
import           Cryptopals.Util
import           Cryptopals.XOR
import           Data.Bits (popCount, xor)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8 (length, replicate)
import qualified Data.ByteString.Lazy as Lazy (ByteString)

decryptXORString :: String -> String -> Maybe String
decryptXORString plaintext ciphertext = do
    -- Score known plaintext
    plainChar <- mostCommonChar plaintext

    -- Score ciphertext
    cipherChar <- mostCommonChar ciphertext

    -- Compute the key
    let key = xorChars plainChar cipherChar

    -- Decrypt the string
    return $ xorString key ciphertext

repeatingXOREncode :: String -> String -> String
repeatingXOREncode key = zipWith xorChars (cycle key)

hamming :: String -> String -> Int
hamming s0 s1 = sum $ zipWith intHamming (map fromEnum s0) (map fromEnum s1)
    where
    intHamming x0 x1 = popCount $ xor x0 x1

defaultIV :: ByteString
defaultIV = Char8.replicate 16 '\0'

decryptAES128ECB :: ByteString -> ByteString -> Lazy.ByteString -> Maybe Lazy.ByteString
decryptAES128ECB key iv bytes
    | Char8.length key /= 16 = Nothing
    | Char8.length iv /= 16 = Nothing
    | otherwise = Just $ crypt ECB key iv Decrypt bytes
