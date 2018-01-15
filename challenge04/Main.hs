module Main (main) where

import Cryptopals
import Paths_cryptopals

tryDecrypt :: Char -> String -> Maybe String
tryDecrypt plainChar ciphertext = do
    cipherChar <- mostCommonChar ciphertext
    let key = xorChars plainChar cipherChar
        result = xorString key ciphertext
    if all isPrintOrSpace result
        then Just result
        else Nothing

main :: IO ()
main = do
    putStrLn "challenge04"

    let plaintext = "The ineffable talent for finding patterns in chaos cannot do its thing unless he immerses himself in the chaos first. If they do contain patterns, he does not see them just now, in any rational way. But there may be some subrational part of his mind that can go to work, now that the letters have passed before his eyes and through his pencil, and that may suddenly present him with a gift-wrapped clue--or even a full solution--a few weeks from now while he is shaving or antenna-twiddling."
        plainChar = fromJust $ mostCommonChar plaintext

    path <- getDataFileName "4.txt"
    xs <- (map (hexDecode . fromJust . hexString) . lines) <$> readFile path
    print $ catMaybes (map (tryDecrypt plainChar) xs)
