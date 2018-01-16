-- http://cryptopals.com/sets/2/challenges/11

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Cryptopals
import qualified Data.ByteString.Char8 as Char8 (replicate, unpack)
import           System.Random (newStdGen)

showMode :: Mode -> String
showMode ECB = "ECB"
showMode CBC = "CBC"

main :: IO ()
main = do
    putStrLn "challenge11"

    replicateM_ 5 $ do
        g <- newStdGen
        let (mode, ciphertext, _) = encryptionOracle g (Char8.replicate 43 '\0')
        let isECB = ecbScore (Char8.unpack ciphertext) >= 0.5
        print $ (showMode mode, isECB)
