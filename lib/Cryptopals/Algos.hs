--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module Cryptopals.Algos
    ( decryptXORString
    , hamming
    , repeatingXOREncode
    ) where

import           Cryptopals.Util
import           Cryptopals.XOR
import           Data.Bits (popCount, xor)

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
