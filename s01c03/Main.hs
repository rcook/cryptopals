module Main (main) where

import Cryptopals

main :: IO ()
main = do
    putStrLn "Set 1 challenge 3"

    let
        -- Some known plaintext
        plaintext = "The ineffable talent for finding patterns in chaos cannot do its thing unless he immerses himself in the chaos first. If they do contain patterns, he does not see them just now, in any rational way. But there may be some subrational part of his mind that can go to work, now that the letters have passed before his eyes and through his pencil, and that may suddenly present him with a gift-wrapped clue--or even a full solution--a few weeks from now while he is shaving or antenna-twiddling."
        ciphertext = hexDecode $ fromJust $ hexString "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    print $ decryptXORString plaintext ciphertext
