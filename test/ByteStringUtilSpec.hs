{-# LANGUAGE OverloadedStrings #-}

module ByteStringUtilSpec (spec) where

import qualified Cryptopals.ByteStringUtil as ByteStringUtil
import           Cryptopals.Prelude
import qualified Data.ByteString as ByteString (append)
import qualified Data.ByteString.Char8 as Char8 (replicate)
import           Test.Hspec

spec :: Spec
spec = do
    describe "chunksOf" $
        it "should return chunks" $
            ByteStringUtil.chunksOf 4 "helloworld" `shouldBe` ["hell", "owor", "ld"]
    describe "pkcs7Pad" $
        it "should pad as expected" $ do
            ByteStringUtil.pkcs7Pad 16 "" `shouldBe` ""
            ByteStringUtil.pkcs7Pad 16 "A" `shouldBe` (ByteString.append "A" (Char8.replicate 15 (chr 15)))
            ByteStringUtil.pkcs7Pad 16 "BB" `shouldBe` (ByteString.append "BB" (Char8.replicate 14 (chr 14)))
            ByteStringUtil.pkcs7Pad 16 "CCCCCCCCCCCC" `shouldBe` (ByteString.append "CCCCCCCCCCCC" (Char8.replicate 4 (chr 4)))
            ByteStringUtil.pkcs7Pad 16 "DDDDDDDDDDDDDDD" `shouldBe` (ByteString.append "DDDDDDDDDDDDDDD" (Char8.replicate 1 (chr 1)))
            ByteStringUtil.pkcs7Pad 16 "EEEEEEEEEEEEEEEE" `shouldBe` "EEEEEEEEEEEEEEEE"
            ByteStringUtil.pkcs7Pad 16 "FFFFFFFFFFFFFFFFF" `shouldBe` (ByteString.append "FFFFFFFFFFFFFFFFF" (Char8.replicate 15 (chr 15)))
    describe "pkcs7Unpad" $
        it "should unpad as expected" $ do
            ByteStringUtil.pkcs7Unpad 16 "" `shouldBe` ""
            ByteStringUtil.pkcs7Unpad 16 (ByteString.append "A" (Char8.replicate 15 (chr 15))) `shouldBe` "A"
            ByteStringUtil.pkcs7Unpad 16 (ByteString.append "BB" (Char8.replicate 14 (chr 14))) `shouldBe` "BB"
            ByteStringUtil.pkcs7Unpad 16 (ByteString.append "CCCCCCCCCCCC" (Char8.replicate 4 (chr 4))) `shouldBe` "CCCCCCCCCCCC"
            ByteStringUtil.pkcs7Unpad 16 (ByteString.append "DDDDDDDDDDDDDDD" (Char8.replicate 1 (chr 1))) `shouldBe` "DDDDDDDDDDDDDDD"
            ByteStringUtil.pkcs7Unpad 16 "EEEEEEEEEEEEEEEE" `shouldBe` "EEEEEEEEEEEEEEEE"
            ByteStringUtil.pkcs7Unpad 16 (ByteString.append "FFFFFFFFFFFFFFFFF" (Char8.replicate 15 (chr 15))) `shouldBe` "FFFFFFFFFFFFFFFFF"
    describe "xor" $ do
        it "should return expected value" $
            "abcde" `ByteStringUtil.xor` "12345" `shouldBe` "PPPPP"
        it "should discard excess elements from first operand" $
            "abcdefg" `ByteStringUtil.xor` "12345" `shouldBe` "PPPPP"
        it "should discard excess elements from second operand" $
            "abcde" `ByteStringUtil.xor` "1234567" `shouldBe` "PPPPP"
