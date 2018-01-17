-- http://cryptopals.com/sets/2/challenges/13

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Cryptopals
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (append, concat, drop, length, take)
import qualified Data.ByteString.Char8 as Char8 (replicate, unpack)

encryptedProfileFor :: AESKey -> ByteString -> ByteString
encryptedProfileFor key email =
    encryptECBWithPadding key (encodeKeyValuePairs (profileFor $ Char8.unpack email))

main :: IO ()
main = do
    putStrLn "challenge13"

    print $ decodeKeyValuePairs "foo=bar&baz=qux&zap=zazzle"
    let profile = profileFor "foo@bar.com"
    print profile
    print $ encodeKeyValuePairs profile

    key <- newAESKeyIO
    let encryptor = encryptedProfileFor key
        ciphertext = encryptor "foo@bar.com"
        plaintext = decryptECBWithPadding key ciphertext
    print plaintext

    let (chunkSize, _) = inferEBCInfo encryptor

    -- 10x A to fill the first block, then admin padding for the next block
    let padding = 11
        email = ByteString.append (Char8.replicate 10 'A') (Char8.replicate padding (chr padding))
        ciphertext = encryptor email
        adminBlock = ByteString.take 16 (ByteString.drop 16 ciphertext)
        ciphertext' = encryptor "admin1@me.com"
        hacked = ByteString.append
                    (ByteString.take ((ByteString.length ciphertext') - 16) ciphertext')
                    adminBlock
        plaintext = decryptECBWithPadding key hacked
    print plaintext
