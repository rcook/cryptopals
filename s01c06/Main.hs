module Main (main) where

import Cryptopals
import Data.List (minimumBy, transpose)

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
    putStrLn "Set 1 challenge 6"
    
    let plaintext = "The ineffable talent for finding patterns in chaos cannot do its thing unless he immerses himself in the chaos first. If they do contain patterns, he does not see them just now, in any rational way. But there may be some subrational part of his mind that can go to work, now that the letters have passed before his eyes and through his pencil, and that may suddenly present him with a gift-wrapped clue--or even a full solution--a few weeks from now while he is shaving or antenna-twiddling."
        plainChar = fromJust $ mostCommonChar plaintext

    Just s <- readBase64DataFile "6.txt"
    let (keySize, _) = minimumBy
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
