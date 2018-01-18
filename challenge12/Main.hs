-- http://cryptopals.com/sets/2/challenges/12

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Cryptopals
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (append)
import           System.Random (newStdGen)

unknownPlaintextPadding :: ByteString
unknownPlaintextPadding =
    base64Decode $
        fromJust $ base64String
            "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\
            \aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\
            \dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\
            \YnkK"

main :: IO ()
main = do
    putStrLn "challenge12"

    g <- newStdGen
    let (unknownKey, _) = newAESKey g
        encryptor s = encryptECB unknownKey (ByteString.append s unknownPlaintextPadding)

    let (chunkSize, isECBResult, plaintext) = breakEBC encryptor
    putStrLn $ "Chunk size      : " ++ show chunkSize
    putStrLn $ "Is ECB          : " ++ show isECBResult
    print plaintext
