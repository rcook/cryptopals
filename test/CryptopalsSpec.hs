{-# LANGUAGE OverloadedStrings #-}

module CryptopalsSpec (spec) where

import           Cryptopals
import qualified Data.ByteString as ByteString (append)
import qualified Data.ByteString.Char8 as Char8 (pack, replicate, unpack)
import           Data.Maybe (catMaybes, fromJust)
import           Paths_cryptopals
import           System.Random (newStdGen)
import           Test.Hspec

aesModesEqual :: Mode -> Mode -> Bool
aesModesEqual ECB ECB = True
aesModesEqual CBC CBC = True
aesModesEqual _ _ = False

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

    describe "hamming" $
        it "matches expected value" $
            hamming "this is a test" "wokka wokka!!!" `shouldBe` 37

    describe "base64Decode" $ do
        it "roundtrips" $ do
            dataPath <- getDataFileName "6.txt"
            content <- readFile dataPath
            let e = fromJust (base64String $ concat (lines content))
                s = base64Decode e
                encoded = base64Encode (Char8.unpack s)
            encoded `shouldBe` e
        it "decodes 4 octets no padding" $
            base64Decode (fromJust $ base64String "TWFu") `shouldBe` "Man"
        it "decodes 4 octets 1 padding" $
            base64Decode (fromJust $ base64String "TWE=") `shouldBe` "Ma"
        it "decodes 4 octets 2 padding" $
            base64Decode (fromJust $ base64String "TQ==") `shouldBe` "M"

    describe "decryptECB" $
        it "works" $ do
            bytes <- fromJust <$> readBase64DataFile "7.txt"
            let key = fromJust $ aesKey "YELLOW SUBMARINE"
            decryptECB key bytes
                `shouldBe`
                "I'm back and I'm ringin' the bell \nA rockin' on the mike while the fly girls yell \nIn ecstasy in the back of me \nWell that's my DJ Deshay cuttin' all them Z's \nHittin' hard and the girlies goin' crazy \nVanilla's on the mike, man I'm not lazy. \n\nI'm lettin' my drug kick in \nIt controls my mouth and I begin \nTo just let it flow, let my concepts go \nMy posse's to the side yellin', Go Vanilla Go! \n\nSmooth 'cause that's the way I will be \nAnd if you don't give a damn, then \nWhy you starin' at me \nSo get off 'cause I control the stage \nThere's no dissin' allowed \nI'm in my own phase \nThe girlies sa y they love me and that is ok \nAnd I can dance better than any kid n' play \n\nStage 2 -- Yea the one ya' wanna listen to \nIt's off my head so let the beat play through \nSo I can funk it up and make it sound good \n1-2-3 Yo -- Knock on some wood \nFor good luck, I like my rhymes atrocious \nSupercalafragilisticexpialidocious \nI'm an effect and that you can bet \nI can take a fly girl and make her wet. \n\nI'm like Samson -- Samson to Delilah \nThere's no denyin', You can try to hang \nBut you'll keep tryin' to get my style \nOver and over, practice makes perfect \nBut not if you're a loafer. \n\nYou'll get nowhere, no place, no time, no girls \nSoon -- Oh my God, homebody, you probably eat \nSpaghetti with a spoon! Come on and say it! \n\nVIP. Vanilla Ice yep, yep, I'm comin' hard like a rhino \nIntoxicating so you stagger like a wino \nSo punks stop trying and girl stop cryin' \nVanilla Ice is sellin' and you people are buyin' \n'Cause why the freaks are jockin' like Crazy Glue \nMovin' and groovin' trying to sing along \nAll through the ghetto groovin' this here song \nNow you're amazed by the VIP posse. \n\nSteppin' so hard like a German Nazi \nStartled by the bases hittin' ground \nThere's no trippin' on mine, I'm just gettin' down \nSparkamatic, I'm hangin' tight like a fanatic \nYou trapped me once and I thought that \nYou might have it \nSo step down and lend me your ear \n'89 in my time! You, '90 is my year. \n\nYou're weakenin' fast, YO! and I can tell it \nYour body's gettin' hot, so, so I can smell it \nSo don't be mad and don't be sad \n'Cause the lyrics belong to ICE, You can call me Dad \nYou're pitchin' a fit, so step back and endure \nLet the witch doctor, Ice, do the dance to cure \nSo come up close and don't be square \nYou wanna battle me -- Anytime, anywhere \n\nYou thought that I was weak, Boy, you're dead wrong \nSo come on, everybody and sing this song \n\nSay -- Play that funky music Say, go white boy, go white boy go \nplay that funky music Go white boy, go white boy, go \nLay down and boogie and play that funky music till you die. \n\nPlay that funky music Come on, Come on, let me hear \nPlay that funky music white boy you say it, say it \nPlay that funky music A little louder now \nPlay that funky music, white boy Come on, Come on, Come on \nPlay that funky music \n\EOT\EOT\EOT\EOT"

    describe "detectECB" $ do
        it "works" $ do
            path <- getDataFileName "8.txt"
            xs <- map (hexDecode . fromJust . hexString) . lines <$> readFile path
            let ((index, _), score) = detectECB xs
            index `shouldBe` 132
            score `shouldBe` 0.4
        it "works with encryption oracle" $ do
            replicateM_ 100 $ do
                g <- newStdGen
                let (knownMode, ciphertext, _) = encryptionOracle g (Char8.replicate 43 '\0')
                    isECB = ecbScore (Char8.unpack ciphertext) > 0.5
                    inferredMode = if isECB then ECB else CBC
                aesModesEqual knownMode inferredMode `shouldBe` True

    describe "padPKCS7" $ do
        it "evaluates to Nothing if padding negative" $
            padPKCS7 (-1) "foo" `shouldBe` Nothing
        it "evaluates to Nothing if padding insufficient" $
            padPKCS7 2 "foo" `shouldBe` Nothing
        it "adds no characters if no padding required" $
            padPKCS7 3 "foo" `shouldBe` Just "foo"
        it "adds 1 characters if 1 padding required" $
            padPKCS7 4 "foo" `shouldBe` Just "foo\x01"
        it "adds 2 characters if 2 padding required" $
            padPKCS7 5 "foo" `shouldBe` Just "foo\x02\x02"
        it "works on test example" $
            padPKCS7 20 "YELLOW SUBMARINE" `shouldBe` Just "YELLOW SUBMARINE\x04\x04\x04\x04"

    describe "decryptCBC" $ do
        it "roundtrips" $ do
            let key = fromJust $ aesKey "YELLOW SUBMARINE"
                plaintext = Char8.pack "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going direct the other way – in short, the period was so far like the present period, that some of its noisiest authorities insisted on its being received, for good or for evil, in the superlative degree of comparison only."
                ciphertext = encryptCBC key zeroIV plaintext
                result = decryptCBC key zeroIV ciphertext
            result `shouldBe` plaintext

    describe "break EBC" $ do
        it "cracks example 1" $ do
            g <- newStdGen
            let targetPlaintext = "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going direct the other way – in short, the period was so far like the present period, that some of its noisiest authorities insisted on its being received, for good or for evil, in the superlative degree of comparison only."
                (unknownKey, _) = newAESKey g
                encryptor s = encryptECB unknownKey (ByteString.append s targetPlaintext)
                (chunkSize, isECBResult, plaintext) = breakEBC encryptor
            chunkSize `shouldBe` 16
            isECBResult `shouldBe` True
            plaintext `shouldBe` targetPlaintext
        it "cracks example 2" $ do
            g <- newStdGen
            let targetPlaintext =
                    base64Decode $
                    fromJust $ base64String
                        "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\
                        \aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\
                        \dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\
                        \YnkK"
                (unknownKey, _) = newAESKey g
                encryptor s = encryptECB unknownKey (ByteString.append s targetPlaintext)
                (chunkSize, isECBResult, plaintext) = breakEBC encryptor
            chunkSize `shouldBe` 16
            isECBResult `shouldBe` True
            plaintext `shouldBe` targetPlaintext

    describe "encryptECBWithPadding" $
        it "pads plaintext before encrypting" $ do
            let Just key = aesKey (Char8.replicate aesChunkSize '\1')
            encryptECBWithPadding key "hello" `shouldBe` "LU\166\255\218.V[@\194\ENQ\GS=\ACKv\DC4"

    describe "decryptECBWithPadding" $
        it "unpads plaintext after decrypting" $ do
            let Just key = aesKey (Char8.replicate aesChunkSize '\1')
            decryptECBWithPadding key "LU\166\255\218.V[@\194\ENQ\GS=\ACKv\DC4" `shouldBe` "hello"
