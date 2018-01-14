--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module Cryptopals.Algos
    ( decryptXORString
    , repeatingXOREncode
    ) where

import           Cryptopals.Util
import           Cryptopals.XOR

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

