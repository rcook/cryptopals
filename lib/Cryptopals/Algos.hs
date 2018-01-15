module Cryptopals.Algos
    ( decryptXORString
    , decryptAES128ECB
    , defaultIV
    , detectAES128ECB
    , hamming
    , padPKCS7
    , repeatingXOREncode
    ) where

import           Codec.Crypto.SimpleAES (Direction(..), Mode(..), crypt)
import           Cryptopals.Prelude
import           Cryptopals.Util
import           Cryptopals.XOR
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8 (length, replicate)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.HashMap.Strict as HashMap (empty, insertWith)

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

score :: Int -> String -> Double
score chunkSize ciphertext =
    let
        counts =
            foldr
                (\chunk m -> HashMap.insertWith (\_ x -> x + 1) chunk (1 :: Int) m)
                HashMap.empty
                (chunksOf chunkSize ciphertext)
        (num, den) =
            foldr
                (\count (n, d) -> (if count > 1 then n + count else n, d + count))
                (0, 0)
                counts
    in fromIntegral num / fromIntegral den

aes128ECBScore :: String -> Double
aes128ECBScore = score 16

detectAES128ECB :: [String] -> ((Int, String), Double)
detectAES128ECB xs =
    maximumBy
        (compare `on` snd)
        $ zip (zip [0..] xs) (map aes128ECBScore xs)

padPKCS7 :: Int -> String -> Maybe String
padPKCS7 n s =
    let count = length s
        padding = n - count
    in
        if padding < 0
            then Nothing
            else Just $ s ++ take padding (repeat (chr padding))
