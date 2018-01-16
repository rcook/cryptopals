--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module Cryptopals.Base64
    ( Base64String(unBase64String)
    , base64Decode
    , base64Encode
    , base64String
    , readBase64DataFile
    ) where

import           Cryptopals.Prelude
import           Data.Bits ((.&.), shift)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8 (pack)
import           Data.List (elemIndex)
import           Paths_cryptopals

newtype Base64String = Base64String
    { unBase64String ::String
    } deriving Eq

instance Show Base64String where
    show (Base64String s) = s

base64Chars :: String
base64Chars =
    foldr (:)
        (foldr (:)
            (foldr (:)
                "+/" ['0'..'9']) ['a'..'z']) ['A'..'Z']

-- TODO: Properly validate input!
base64String :: String -> Maybe Base64String
base64String = Just . Base64String

base64Encode :: String -> Base64String
base64Encode = Base64String . concat . encodeChunks

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

decodeChunk :: Char -> Char -> Char -> Char -> String
decodeChunk c0 c1 '=' '=' =
    let i0 = decodeOctet c0
        i1 = decodeOctet c1
        x0 = (i0 `shift` 2) + ((i1 .&. 0x30) `shift` (-4))
    in chr x0 : []
decodeChunk c0 c1 c2 '=' =
    let i0 = decodeOctet c0
        i1 = decodeOctet c1
        i2 = decodeOctet c2
        x0 = (i0 `shift` 2) + ((i1 .&. 0x30) `shift` (-4))
        x1 = ((i1 .&. 0x0f) `shift` 4) + ((i2 .&. 0x3c) `shift` (-2))
    in chr x0 : chr x1 : []
decodeChunk c0 c1 c2 c3 =
    let i0 = decodeOctet c0
        i1 = decodeOctet c1
        i2 = decodeOctet c2
        i3 = decodeOctet c3
        x0 = (i0 `shift` 2) + ((i1 .&. 0x30) `shift` (-4))
        x1 = ((i1 .&. 0x0f) `shift` 4) + ((i2 .&. 0x3c) `shift` (-2))
        x2 = ((i2 .&. 0x03) `shift` 6) + i3
    in chr x0 : chr x1 : chr x2 : []

decodeOctet :: Char -> Int
decodeOctet c = fromJust $ c `elemIndex` base64Chars

base64Decode :: Base64String -> ByteString
base64Decode s = Char8.pack $ concat (base64DecodeHelper (unBase64String s))

base64DecodeHelper :: String -> [String]
base64DecodeHelper [] = []
base64DecodeHelper (c0 : c1 : c2 : c3 : cs) = decodeChunk c0 c1 c2 c3 : base64DecodeHelper cs
base64DecodeHelper _ = error "Invalid Base64-encoded string"

readBase64DataFile :: FilePath -> IO (Maybe ByteString)
readBase64DataFile fileName = do
        dataPath <- getDataFileName fileName
        xs <- (concat . lines) <$> readFile dataPath
        case base64String xs of
            Nothing -> return Nothing
            Just base64 -> return (Just $ base64Decode base64)
