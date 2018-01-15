module Main (main) where

import Cryptopals

main :: IO ()
main = do
    putStrLn "challenge02"

    let s0 = fromJust $ hexString "1c0111001f010100061a024b53535009181c"
        s1 = fromJust $ hexString "686974207468652062756c6c277320657965"
    print s0
    print s1
    print $ hexEncode (xorStrings (hexDecode s0) (hexDecode s1))
