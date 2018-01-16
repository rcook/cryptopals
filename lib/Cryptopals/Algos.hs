{-# LANGUAGE OverloadedStrings #-}

module Cryptopals.Algos
    ( AESIV
    , AESKey
    , Mode(..)
    , aesChunkSize
    , aesIV
    , aesKey
    , breakEBC
    , decryptXORString
    , decryptCBC
    , decryptECB
    , detectECB
    , ecbScore
    , encryptCBC
    , encryptECB
    , encryptionOracle
    , hamming
    , newAESIV
    , newAESKey
    , padPKCS7
    , repeatingXOREncode
    , zeroIV
    ) where

import           Codec.Crypto.SimpleAES (Direction(..), Mode(..), crypt)
import           Cryptopals.Prelude
import           Cryptopals.Util
import           Cryptopals.XOR
import qualified Data.ByteString as ByteString (append, concat, length, take)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8 (pack, replicate, snoc, unpack)
import qualified Data.ByteString.Lazy as Lazy (fromStrict, toStrict)
import qualified Data.HashMap.Strict as HashMap (empty, insert, insertWith, lookup)
import           System.Random (Random, RandomGen, random, randomR)

type Encryptor = ByteString -> ByteString

newtype AESKey = AESKey ByteString deriving Show
newtype AESIV = AESIV ByteString deriving Show

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

decryptECB :: AESKey -> ByteString -> ByteString
decryptECB (AESKey key) bytes
    = Lazy.toStrict (crypt ECB key rawZeroIV Decrypt (Lazy.fromStrict bytes))

encryptECB :: AESKey -> ByteString -> ByteString
encryptECB (AESKey key) bytes
    = Lazy.toStrict (crypt ECB key rawZeroIV Encrypt (Lazy.fromStrict bytes))

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

ecbScore :: String -> Double
ecbScore = score aesChunkSize

detectECB :: [String] -> ((Int, String), Double)
detectECB xs =
    maximumBy
        (compare `on` snd)
        $ zip (zip [0..] xs) (map ecbScore xs)

padPKCS7 :: Int -> String -> Maybe String
padPKCS7 n s =
    let count = length s
        padding = n - count
    in
        if padding < 0
            then Nothing
            else Just $ s ++ take padding (repeat (chr padding))

encryptCBC :: AESKey -> AESIV -> ByteString -> ByteString
encryptCBC key (AESIV iv) plaintext =
    let chunks = byteStringChunksOf aesChunkSize plaintext
        (_, resultChunks) =
            foldl'
                (\(prev, xs) chunk ->
                    let temp0 = byteStringPadPKCS7Unsafe aesChunkSize chunk
                        temp1 = byteStringXor prev temp0
                        ciphertext = encryptECB key temp1
                    in (ciphertext, xs ++ [ciphertext]))
                (iv, [])
                chunks
    in ByteString.concat resultChunks

decryptCBC :: AESKey -> AESIV -> ByteString -> ByteString
decryptCBC key (AESIV iv) ciphertext =
    let chunks = byteStringChunksOf aesChunkSize ciphertext
        (_, resultChunks) =
            foldl'
                (\(prev, xs) chunk ->
                    let temp0 = decryptECB key chunk
                        temp1 = byteStringXor prev temp0
                        plaintext = byteStringUnpadPKCS7Unsafe aesChunkSize temp1
                    in (chunk, xs ++ [plaintext]))
                (iv, [])
                chunks
    in ByteString.concat resultChunks

randomsN :: (RandomGen a, Random b) => Int -> a -> ([b], a)
randomsN n g =
    foldl'
        (\(items, g') _ -> let (item, g'') = random g' in (items ++ [item], g''))
        ([], g)
        [1..n]

aesChunkSize :: Int
aesChunkSize = 16

rawZeroIV :: ByteString
rawZeroIV = Char8.replicate aesChunkSize '\0'

zeroIV :: AESIV
zeroIV = AESIV rawZeroIV

aesKey :: ByteString -> Maybe AESKey
aesKey s
    | ByteString.length s /= aesChunkSize = Nothing
    | otherwise = Just $ AESKey s

newAESKey :: RandomGen a => a -> (AESKey, a)
newAESKey g =
    let (cs, g') = randomsN aesChunkSize g
    in (AESKey $ Char8.pack cs, g')

aesIV :: ByteString -> Maybe AESIV
aesIV s
    | ByteString.length s /= aesChunkSize = Nothing
    | otherwise = Just $ AESIV s

newAESIV :: RandomGen a => a -> (AESIV, a)
newAESIV g =
    let (cs, g') = randomsN aesChunkSize g
    in (AESIV $ Char8.pack cs, g')

randomMode :: RandomGen a => a -> (Mode, a)
randomMode g =
    let (isECB', g') = randomR (False, True) g
    in if isECB' then (ECB, g') else (CBC, g')

encryptionOracle :: RandomGen a => a -> ByteString -> (Mode, ByteString, a)
encryptionOracle g bytes =
    let (mode, g') = randomMode g   -- Random mode
        (key, g'') = newAESKey g'   -- Random key
    in case mode of
        ECB -> (mode, encryptECB key bytes, g'')
        CBC ->
            let (iv, g''') = newAESIV g''   -- Random IV
            in (mode, encryptCBC key iv bytes, g''')

isECB :: Int -> Encryptor -> Bool
isECB chunkSize encryptor =
    let ciphertext = encryptor (Char8.replicate (2 * chunkSize) '\0')
        x0 : x1 : _ = chunksOf chunkSize $ Char8.unpack ciphertext
    in x0 == x1

breakEBC :: Encryptor -> (Int, Bool, ByteString)
breakEBC f =
    let lengths = map (\i -> ByteString.length $ f (Char8.replicate i '\0')) [0..]
        transitions = zipWith (-) (drop 1 lengths) lengths
        chunkSize = head $ dropWhile (== 0) transitions
        Just offset = findIndex (/= 0) transitions
        plaintextLength = head lengths - offset
    in
        (chunkSize, isECB chunkSize f,
            foldl'
                (\found i -> Char8.snoc found (bruteForce chunkSize f i found))
                ""
                [1..plaintextLength])

prefixLength :: Int -> Int -> Int
prefixLength chunkSize i =  chunkSize - (i - 1) `mod` chunkSize - 1

bruteForce :: Int -> Encryptor -> Int -> ByteString -> Char
bruteForce chunkSize encryptor index found =
    let pl = prefixLength chunkSize index
        targetSize = (1 + (index - 1) `quot` chunkSize) * chunkSize
        target = ByteString.take targetSize $ encryptor (Char8.replicate pl '\0')
        prefix = ByteString.append (Char8.replicate pl '\0') found
        dict =
            foldr
                (\x m ->
                    let plaintext = Char8.snoc prefix (chr x)
                        ciphertext = encryptor plaintext
                    in HashMap.insert (ByteString.take targetSize ciphertext) x m)
                HashMap.empty
                [0..255]
        c = fromJust $ HashMap.lookup target dict
    in chr c
