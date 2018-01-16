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

--unknownPlaintextPadding = "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going direct the other way – in short, the period was so far like the present period, that some of its noisiest authorities insisted on its being received, for good or for evil, in the superlative degree of comparison only."





main :: IO ()
main = do
    putStrLn "challenge12"

    g <- newStdGen
    let (unknownKey, _) = newAESKey g
        encryptor s = encryptECB unknownKey (ByteString.append s unknownPlaintextPadding)
        --plaintext = "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going direct the other way – in short, the period was so far like the present period, that some of its noisiest authorities insisted on its being received, for good or for evil, in the superlative degree of comparison only."

    let (chunkSize, isECBResult, plaintext) = breakEBC encryptor
    putStrLn $ "Chunk size      : " ++ show chunkSize
    putStrLn $ "Is ECB          : " ++ show isECBResult
    print plaintext
