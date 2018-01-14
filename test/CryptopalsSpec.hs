--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module CryptopalsSpec (spec) where

import           Cryptopals
import           Data.Maybe (catMaybes, fromJust)
import           Paths_cryptopals
import           Test.Hspec

spec :: Spec
spec = do
    describe "hexDecode" $
        it "matches expected result" $
            (hexDecode $ fromJust (hexString "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"))
                `shouldBe`
                "I'm killing your brain like a poisonous mushroom"

    describe "base64Encode" $ do
        it "matches expected result" $ do
            (base64Encode (hexDecode $ fromJust (hexString "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")))
                `shouldBe`
                (fromJust (base64String "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"))

        it "handles padding 1" $
            base64Encode "any carnal pleasure." `shouldBe` fromJust (base64String "YW55IGNhcm5hbCBwbGVhc3VyZS4=")

        it "handles padding 2" $
            base64Encode "any carnal pleasure" `shouldBe` fromJust (base64String "YW55IGNhcm5hbCBwbGVhc3VyZQ==")

        it "handles no padding" $
            base64Encode "any carnal pleasur" `shouldBe` fromJust (base64String "YW55IGNhcm5hbCBwbGVhc3Vy")

        it "handles padding 1 again" $
            base64Encode "any carnal pleasu" `shouldBe` fromJust (base64String "YW55IGNhcm5hbCBwbGVhc3U=")

        it "handles padding 2 again" $
            base64Encode "any carnal pleas" `shouldBe` fromJust (base64String "YW55IGNhcm5hbCBwbGVhcw==")

    describe "xorStrings" $
        it "matches expected result" $ do
            let s0 = hexDecode $ fromJust (hexString "1c0111001f010100061a024b53535009181c")
                s1 = hexDecode $ fromJust (hexString "686974207468652062756c6c277320657965")
                result = xorStrings s0 s1
            result `shouldBe` "the kid don't play"
            hexEncode result `shouldBe` (fromJust $ hexString "746865206b696420646f6e277420706c6179")

    describe "decryptXORString" $
        it "should decrypt correctly" $ do
            let
                -- Some known plaintext
                plaintext = "The ineffable talent for finding patterns in chaos cannot do its thing unless he immerses himself in the chaos first. If they do contain patterns, he does not see them just now, in any rational way. But there may be some subrational part of his mind that can go to work, now that the letters have passed before his eyes and through his pencil, and that may suddenly present him with a gift-wrapped clue--or even a full solution--a few weeks from now while he is shaving or antenna-twiddling."

                -- The ciphertext
                ciphertext = hexDecode $ fromJust (hexString "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
            decryptXORString plaintext ciphertext `shouldBe` Just "Cooking MC's like a pound of bacon"

    describe "decryptIfPrintOrSpace" $
        it "matches expected output" $ do
            let plaintext = "The ineffable talent for finding patterns in chaos cannot do its thing unless he immerses himself in the chaos first. If they do contain patterns, he does not see them just now, in any rational way. But there may be some subrational part of his mind that can go to work, now that the letters have passed before his eyes and through his pencil, and that may suddenly present him with a gift-wrapped clue--or even a full solution--a few weeks from now while he is shaving or antenna-twiddling."
                plainChar = fromJust $ mostCommonChar plaintext

            dataPath <- getDataFileName "4.txt"
            xs <- (map (hexDecode . fromJust . hexString) . lines) <$> readFile dataPath

            let tryDecrypt ciphertext = do
                    cipherChar <- mostCommonChar ciphertext
                    let key = xorChars plainChar cipherChar
                        result = xorString key ciphertext
                    if all isPrintOrSpace result
                        then Just result
                        else Nothing

            catMaybes (map tryDecrypt xs)
                `shouldBe`
                ["Now that the party is jumping\n"]

    describe "repeatingXOREncode" $
        it "matches expected output" $ do
            let plaintext = "Burning 'em, if you ain't quick and nimble\n\
                \I go crazy when I hear a cymbal"
            hexEncode (repeatingXOREncode "ICE" plaintext)
                `shouldBe`
                    (fromJust $ hexString ("0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272\
                                            \a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"))
