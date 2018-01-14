--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module Cryptopals.XOR
    ( xorChars
    , xorString
    , xorStrings
    ) where

import           Data.Bits (xor)
import           Data.Char (chr)

xorChars :: Char -> Char -> Char
xorChars c0 c1 = chr (xor (fromEnum c0) (fromEnum c1))

xorString :: Char -> String -> String
xorString c s = map (xorChars c) s

xorStrings :: String -> String -> String
xorStrings s0 s1 = zipWith xorChars s0 s1
