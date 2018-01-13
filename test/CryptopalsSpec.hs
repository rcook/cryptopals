--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module CryptopalsSpec (spec) where

import           Cryptopals
import           Data.Maybe (fromJust)
import           Test.Hspec

spec :: Spec
spec = do
    describe "fromHexString" $
        it "matches expected result" $
            fromHexString "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
                `shouldBe`
                Just "I'm killing your brain like a poisonous mushroom"

    describe "base64Encode" $ do
        it "matches expected result" $ do
            let Just bytes = fromHexString "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
                actual = base64Encode bytes
            actual `shouldBe` "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

        it "handles padding 1" $
            base64Encode "any carnal pleasure." `shouldBe` "YW55IGNhcm5hbCBwbGVhc3VyZS4="

        it "handles padding 2" $
            base64Encode "any carnal pleasure" `shouldBe` "YW55IGNhcm5hbCBwbGVhc3VyZQ=="

        it "handles no padding" $
            base64Encode "any carnal pleasur" `shouldBe` "YW55IGNhcm5hbCBwbGVhc3Vy"

        it "handles padding 1 again" $
            base64Encode "any carnal pleasu" `shouldBe` "YW55IGNhcm5hbCBwbGVhc3U="

        it "handles padding 2 again" $
            base64Encode "any carnal pleas" `shouldBe` "YW55IGNhcm5hbCBwbGVhcw=="

    describe "xorBytes" $
        it "matches expected result" $ do
            let op0 = fromJust $ fromHexString "1c0111001f010100061a024b53535009181c"
                op1 = fromJust $ fromHexString "686974207468652062756c6c277320657965"
                result = xorBytes op0 op1
            result `shouldBe` "the kid don't play"
            hexEncode result `shouldBe` "746865206b696420646f6e277420706c6179"
