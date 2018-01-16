-- http://cryptopals.com/sets/2/challenges/10

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Cryptopals

main :: IO ()
main = do
    putStrLn "challenge10"

    let key = fromJust $ aesKey "YELLOW SUBMARINE"
    bytes <- fromJust <$> readBase64DataFile "10.txt"
    print $ decryptCBC key zeroIV bytes
