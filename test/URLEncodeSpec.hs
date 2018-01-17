{-# LANGUAGE OverloadedStrings #-}

module URLEncodeSpec (spec) where

import           Cryptopals.URLEncode
import           Data.Attoparsec.ByteString (endOfInput, parseOnly)
import           Test.Hspec

spec :: Spec
spec = do
    describe "token" $ do
        it "parses as expected" $ do
            parseOnly (token <* endOfInput) "foo" `shouldBe` Right "foo"
            parseOnly (token <* endOfInput) "foo@bar" `shouldBe` Right "foo@bar"
            parseOnly (token <* endOfInput) "foo.bar" `shouldBe` Right "foo.bar"
            parseOnly (token <* endOfInput) "foo123" `shouldBe` Right "foo123"
            parseOnly (token <* endOfInput) "foo123bar" `shouldBe` Right "foo123bar"
            parseOnly (token <* endOfInput) "foo123bar=" `shouldBe` Left "endOfInput"
        it "handles encoded characters" $ do
            parseOnly (token <* endOfInput) "foo%20bar" `shouldBe` Right "foo bar"
            parseOnly (token <* endOfInput) "foo%25bar" `shouldBe` Right "foo%bar"
            parseOnly (token <* endOfInput) "foo%26bar" `shouldBe` Right "foo&bar"
            parseOnly (token <* endOfInput) "foo%3Dbar" `shouldBe` Right "foo=bar"
    describe "keyValuePair" $
        it "parses as expected" $ do
            parseOnly (keyValuePair <* endOfInput) "foo=bar" `shouldBe` Right ("foo", "bar")
            parseOnly (keyValuePair <* endOfInput) "foo=bar=" `shouldBe` Left "endOfInput"
    describe "keyValuePairs" $ do
        it "parses as expected" $
            parseOnly (keyValuePairs <* endOfInput) "foo=bar&xxx=yyy" `shouldBe` Right [("foo", "bar"), ("xxx", "yyy")]
        it "parses example as expected" $
            parseOnly (keyValuePairs <* endOfInput) "foo=bar&baz=qux&zap=zazzle" `shouldBe` Right [("foo", "bar"), ("baz", "qux"), ("zap", "zazzle")]
    describe "encodeToken" $
        it "encodes as expected" $ do
            encodeToken "foo" `shouldBe` "foo"
            encodeToken "foo bar" `shouldBe` "foo%20bar"
            encodeToken "foo%bar" `shouldBe` "foo%25bar"
    describe "profileFor" $
        it "handles example" $
            profileFor "foo@bar.com" `shouldBe` [("email", "foo@bar.com"), ("uid", "10"), ("role", "user")]
    describe "encodeKeyValuePairs" $
        it "handles example" $
            encodeKeyValuePairs (profileFor "foo@bar.com") `shouldBe` "email=foo@bar.com&uid=10&role=user"
