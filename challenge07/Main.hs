-- http://cryptopals.com/sets/1/challenges/7

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Cryptopals

main :: IO ()
main = do
    putStrLn "challenge07"

    bytes <- fromJust <$> readBase64DataFile "7.txt"
    let key = fromJust $ aesKey "YELLOW SUBMARINE"
    print $ decryptECB key bytes
