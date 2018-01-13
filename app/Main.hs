--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module Main (main) where

import Cryptopals

main :: IO ()
main = do
    let Just bytes = fromHexString "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    print $ base64Encode bytes
