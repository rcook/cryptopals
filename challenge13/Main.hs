-- http://cryptopals.com/sets/2/challenges/13

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Cryptopals
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (append, drop, take)
import qualified Data.ByteString.Char8 as Char8 (replicate, unpack)

encryptProfileFor :: AESKey -> ByteString -> ByteString
encryptProfileFor key email =
    encryptECBWithPadding key (encodeKeyValuePairs (profileFor $ Char8.unpack email))

-- "email=&uid=10&role=user" (length: 23)
attack :: Encryptor -> ByteString
attack f =
    let (chunkSize, _) = inferEBCInfo f
        email1 = ByteString.append "aaaaaaaaaaadmin" (Char8.replicate 11 (chr 11)) -- (length: 26)
        x1 = f email1
        adminBlock = ByteString.take chunkSize (ByteString.drop chunkSize x1)
        email2 = "aaaaaaaaaaaaa" -- (length: 13)
        x2 = f email2
        normalBlocks = ByteString.take (2 * chunkSize) x2
    in ByteString.append normalBlocks adminBlock

main :: IO ()
main = do
    putStrLn "challenge13"

    -- Demonstrate key-value-pair parser
    print $ decodeKeyValuePairs "foo=bar&baz=qux&zap=zazzle"

    -- Demonstrate profileFor function
    let profile = profileFor "foo@bar.com"
    print profile
    print $ encodeKeyValuePairs profile

    -- This is the encryption oracle provided to the attacker
    key <- newAESKeyIO
    let encryptor = encryptProfileFor key

    -- Attack with no knowledge of the key
    let ciphertext = attack encryptor
        plaintext = decryptECBWithPadding key ciphertext

    -- Hmm, I dunno: the padding indicates tampering!
    print plaintext
    let pairs = decodeKeyValuePairs plaintext
    print pairs
