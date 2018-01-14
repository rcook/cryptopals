--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module Cryptopals.Hex
    ( HexString
    , hexDecode
    , hexEncode
    , hexString
    ) where

import           Data.Char (chr, toLower)
import           Data.List (elemIndex)
import           Text.Printf (printf)

data HexString = HexString
    { _hsSource :: String
    , _hsEncoded :: String
    } deriving (Eq, Show)

hexChars :: String
hexChars = "0123456789abcdef"

nibble :: Char -> Maybe Int
nibble = flip elemIndex hexChars . toLower

parse :: String -> Maybe String
parse (c0 : c1 : cs) = do
    hi <- nibble c0
    lo <- nibble c1
    remainder <- parse cs
    return $ chr (hi * 0x10 + lo) : remainder
parse "" = Just []
parse _ = Nothing

hexString :: String -> Maybe HexString
hexString encoded = do
    source <- parse encoded
    return $ HexString source encoded

hexDecode :: HexString -> String
hexDecode (HexString source _) = source

hexEncode :: String -> HexString
hexEncode source = HexString source (concat $ map (printf "%02x") source)
