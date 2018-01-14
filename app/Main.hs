--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module Main (main) where

import Cryptopals
import Data.Bits ((.&.), popCount, shift, xor)
import Data.Char (chr)
import Data.List (elemIndex, minimumBy, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Paths_cryptopals
import Test.Hspec

base64Chars :: String
base64Chars =
    foldr (:)
        (foldr (:)
            (foldr (:)
                "+/" ['0'..'9']) ['a'..'z']) ['A'..'Z']

hamming ::
    String
    -> String
    -> Int
hamming s0 s1 = sum $ zipWith intHamming (map fromEnum s0) (map fromEnum s1)

intHamming :: Int -> Int -> Int
intHamming x0 x1 = popCount $ xor x0 x1

decodeChunk :: Char -> Char -> Char -> Char -> String
decodeChunk c0 c1 '=' '=' =
    let i0 = decodeOctet c0
        i1 = decodeOctet c1
        x0 = (i0 `shift` 2) + ((i1 .&. 0x30) `shift` (-4))
    in chr x0 : []
decodeChunk c0 c1 c2 '=' =
    let i0 = decodeOctet c0
        i1 = decodeOctet c1
        i2 = decodeOctet c2
        x0 = (i0 `shift` 2) + ((i1 .&. 0x30) `shift` (-4))
        x1 = ((i1 .&. 0x0f) `shift` 4) + ((i2 .&. 0x3c) `shift` (-2))
    in chr x0 : chr x1 : []
decodeChunk c0 c1 c2 c3 =
    let i0 = decodeOctet c0
        i1 = decodeOctet c1
        i2 = decodeOctet c2
        i3 = decodeOctet c3
        x0 = (i0 `shift` 2) + ((i1 .&. 0x30) `shift` (-4))
        x1 = ((i1 .&. 0x0f) `shift` 4) + ((i2 .&. 0x3c) `shift` (-2))
        x2 = ((i2 .&. 0x03) `shift` 6) + i3
    in chr x0 : chr x1 : chr x2 : []

decodeOctet :: Char -> Int
decodeOctet c = fromJust $ c `elemIndex` base64Chars

base64Decode :: Base64String -> String
base64Decode s = concat (base64DecodeHelper (unBase64String s))

base64DecodeHelper :: String -> [String]
base64DecodeHelper [] = []
base64DecodeHelper (c0 : c1 : c2 : c3 : cs) = decodeChunk c0 c1 c2 c3 : base64DecodeHelper cs
base64DecodeHelper _ = error "Invalid Base64-encoded string"

spec :: Spec
spec = do
    describe "hamming" $
        it "matches expected value" $
            hamming "this is a test" "wokka wokka!!!" `shouldBe` 37

    describe "decodeChunk" $ do
        it "decodes 4 octets no padding" $
            decodeChunk 'T' 'W' 'F' 'u'
                `shouldBe` "Man"
        it "decodes 4 octets with 1 padding" $
            decodeChunk 'T' 'W' 'E' '='
                `shouldBe` "Ma"
        it "decodes 4 octets with 2 padding" $
            decodeChunk 'T' 'Q' '=' '='
                `shouldBe` "M"

    describe "base64Decode" $ do
        it "roundtrips" $ do
            dataPath <- getDataFileName "6.txt"
            content <- readFile dataPath
            let e = fromJust (base64String $ concat (lines content))
                s = base64Decode e
                encoded = base64Encode s
            encoded `shouldBe` e

normalizedDist :: Int -> String -> Double
normalizedDist keySize s =
    let (r0, t0) = splitAt keySize s
        (r1, t1) = splitAt keySize t0
        (r2, t2) = splitAt keySize t1
        (r3, _) = splitAt keySize t2
        total = hamming r0 r1 + hamming r0 r2 + hamming r0 r3 + hamming r1 r2 + hamming r1 r3 + hamming r2 r3
        dist = fromIntegral total / fromIntegral keySize / 6
    in dist

main :: IO ()
main = do
    hspec spec

    let plaintext = "The ineffable talent for finding patterns in chaos cannot do its thing unless he immerses himself in the chaos first. If they do contain patterns, he does not see them just now, in any rational way. But there may be some subrational part of his mind that can go to work, now that the letters have passed before his eyes and through his pencil, and that may suddenly present him with a gift-wrapped clue--or even a full solution--a few weeks from now while he is shaving or antenna-twiddling."
        plainChar = fromJust $ mostCommonChar plaintext

    dataPath <- getDataFileName "6.txt"
    xs <- (concat . lines) <$> readFile dataPath
    let s = base64Decode (fromJust $ base64String xs)
        (keySize, _) = minimumBy
                            (\(_, dist0) (_, dist1) -> dist0 `compare` dist1)
                            (map (\ks -> (ks, normalizedDist ks s)) [2..40])
        chunks = chunksOf keySize s
        blocks = transpose chunks
        result = foldr (\block keys ->
                    let cipherChar = fromJust $ mostCommonChar block
                        key = xorChars plainChar cipherChar
                    in key : keys)
                    []
                    blocks
    putStrLn $ repeatingXOREncode result s
