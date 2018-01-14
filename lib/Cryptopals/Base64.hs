--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module Cryptopals.Base64
    ( Base64String(unBase64String)
    , base64Encode
    , base64String
    ) where

import           Data.Bits ((.&.), shift)

newtype Base64String = Base64String
    { unBase64String :: String
    } deriving (Eq, Show)

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
