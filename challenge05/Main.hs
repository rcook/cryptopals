-- http://cryptopals.com/sets/1/challenges/5

module Main (main) where

import Cryptopals

main :: IO ()
main = do
    putStrLn "challenge05"

    let plaintext = "Burning 'em, if you ain't quick and nimble\n\
                    \I go crazy when I hear a cymbal"

    putStrLn plaintext
    print $ hexEncode (repeatingXOREncode "ICE" plaintext)
