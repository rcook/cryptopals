-- http://cryptopals.com/sets/2/challenges/9

module Main (main) where

import           Cryptopals

main :: IO ()
main = do
    putStrLn "challenge09"

    print $ padPKCS7 20 "YELLOW SUBMARINE"
