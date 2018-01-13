--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module Cryptopals
    ( base64Encode
    , decryptIfPrintOrSpace
    , decryptXORString
    , fromHexString
    , hexEncode
    , mostCommonChar
    , toChars
    , xorBytes
    , xorKey
    , xorString
    ) where

import           Data.Bits ((.&.), shift, xor)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Char8 (pack, unpack)
import           Data.Char (isPrint, isSpace, toLower)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List (elemIndex)
import           Text.Printf (printf)

-- |Encode a hexadecimal-encoded string as a blob of bytes
fromHexString ::
    String              -- ^ Hexadecimal-encoded string
    -> Maybe ByteString -- ^ Blob of bytes
fromHexString s = Char8.pack <$> (helper s)
    where
        helper (c0 : c1 : cs) = do
            upper <- nibble c0
            lower <- nibble c1
            l <- helper cs
            return $ toEnum (upper * 0x10 + lower) : l
        helper [] = Just []
        helper _ = Nothing
        hexChars = "0123456789abcdef"
        nibble = flip elemIndex hexChars . toLower

base64Chars :: String
base64Chars =
    foldr (:)
        (foldr (:)
            (foldr (:)
                "+/" ['0'..'9']) ['a'..'z']) ['A'..'Z']

-- |Unpack blob of bytes as string
toChars ::
    ByteString  -- ^ Blob of bytes
    -> String   -- ^ String
toChars = Char8.unpack

-- |Encode blob of bytes using Base64 encoding
-- See <http://cryptopals.com/sets/1/challenges/1>
base64Encode ::
    ByteString  -- ^ Blob of bytes
    -> String   -- ^ Base64-encoded string
base64Encode = concat . encodeChunks . toChars

encodeChunks :: String -> [String]
encodeChunks (c0 : c1 : c2 : cs) = encodeChunk3 c0 c1 c2 : encodeChunks cs
encodeChunks (c0 : c1 : _) = [encodeChunk2 c0 c1]
encodeChunks (c0 : _ ) = [encodeChunk1 c0]
encodeChunks _ = []

encodeChunk1 :: Char -> String
encodeChunk1 c0 =
    let x0 = fromEnum c0
        i0 = shift x0 (-2)
        i1 = shift (x0 .&. 0x3) 4
    in base64Chars !! i0 : base64Chars !! i1 : "=="

encodeChunk2 :: Char -> Char -> String
encodeChunk2 c0 c1 =
    let x0 = fromEnum c0
        x1 = fromEnum c1
        i0 = shift x0 (-2)
        i1 = shift (x0 .&. 0x3) 4 + shift (x1 .&. 0xf0) (-4)
        i2 = shift (x1 .&. 0x0f) 2
    in base64Chars !! i0 : base64Chars !! i1 : base64Chars !! i2 : "="

encodeChunk3 :: Char -> Char -> Char -> String
encodeChunk3 c0 c1 c2 =
    let x0 = fromEnum c0
        x1 = fromEnum c1
        x2 = fromEnum c2
        i0 = shift x0 (-2)
        i1 = shift (x0 .&. 0x3) 4 + shift (x1 .&. 0xf0) (-4)
        i2 = shift (x1 .&. 0x0f) 2 + shift (x2 .&. 0xc0) (-6)
        i3 = x2 .&. 0x3f
    in base64Chars !! i0 : base64Chars !! i1 : base64Chars !! i2 : base64Chars !! i3 : []

-- |Hex-encode a blob of bytes
hexEncode ::
    ByteString  -- ^ Blob of bytes
    -> String   -- ^ Result
hexEncode bytes = concat $ map (\x -> printf "%02x" x) (toChars bytes)

-- |Evaluate elementwise XOR of two byte blobs
-- See <http://cryptopals.com/sets/1/challenges/2>
xorBytes ::
    ByteString      -- ^ First blob of bytes
    -> ByteString   -- ^ Second blob of bytes
    -> ByteString   -- ^ Result
xorBytes bytes0 bytes1 = Char8.pack $ map toEnum (zipWith xor (map fromEnum $ toChars bytes0) (map fromEnum $ toChars bytes1))

-- |Given a string, computes character frequency map
scoreText ::
    String              -- ^ String
    -> HashMap Char Int -- ^ Map
scoreText =
    foldr
        (\c m -> let k = toLower c in HashMap.insertWith (\_ x -> x + 1) k 1 m)
        HashMap.empty

-- |Determine XOR key based on character frequencies of plaintext and decrypt ciphertext
-- See <http://cryptopals.com/sets/1/challenges/3>
decryptXORString ::
    String          -- ^ Plaintext
    -> ByteString   -- ^ Ciphertext
    -> Maybe String -- ^ Decrypted text
decryptXORString plaintext bytes = do
    let ciphertext = toChars bytes

    -- Score known plaintext
    maxPlainChar <- mostCommonChar plaintext

    -- Score ciphertext
    maxCipherChar <- mostCommonChar ciphertext

    -- Compute the key
    let key = xorKey maxPlainChar maxCipherChar

    -- Decrypt the string
    return $ xorString key ciphertext

-- |Given a string, gets most frequent character
mostCommonChar ::
    String          -- ^ String
    -> Maybe Char   -- ^ Most frequent character
mostCommonChar [] = Nothing
mostCommonChar s =
    let (c, _) = HashMap.foldlWithKey' (\p@(_, n) c' n' -> if n' > n then (c', n') else p) (' ', -1) (scoreText s)
    in Just c

-- |Compute XOR of two characters
xorKey ::
    Char    -- ^ First character
    -> Char -- ^ Second character
    -> Int  -- ^ Result
xorKey c0 c1 = xor (fromEnum c0) (fromEnum c1)

-- |Apply an XOR key to a string
xorString ::
    Int         -- ^ XOR key
    -> String   -- ^ String
    -> String   -- ^ Result
xorString key s = map (toEnum . xor key . fromEnum) s

-- |Decrypt string if result is all printable characters
-- See <http://cryptopals.com/sets/1/challenges/4>
decryptIfPrintOrSpace ::
    Char            -- ^ Most common plaintext character
    -> String       -- ^ Hex-encoded ciphertext
    -> Maybe String -- ^ @Just@ a decrypted string if printable otherwise @Nothing@
decryptIfPrintOrSpace mcpc line = do
    bytes <- fromHexString line
    let ciphertext = toChars bytes
    c <- mostCommonChar ciphertext
    let key = xorKey mcpc c
        decryptedText = xorString key ciphertext
    if all isPrintOrSpace decryptedText
        then Just decryptedText
        else Nothing

isPrintOrSpace :: Char -> Bool
isPrintOrSpace x = isPrint x || isSpace x
