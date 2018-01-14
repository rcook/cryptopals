module Main (main) where

import Cryptopals
import Imports

main :: IO ()
main = do
    putStrLn "Set 1 challenge 1"

    let hs = fromJust $ hexString "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    print hs
    print $ (base64Encode . hexDecode) hs
