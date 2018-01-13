--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module Main (main) where

import Cryptopals
import Imports

main :: IO ()
main = do
    let op0 = fromJust $ fromHexString "1c0111001f010100061a024b53535009181c"
        op1 = fromJust $ fromHexString "686974207468652062756c6c277320657965"
        result = xorBytes op0 op1
    print $ hexEncode result
