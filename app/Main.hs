--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module Main (main) where

import Cryptopals
import Imports
import Paths_cryptopals

main :: IO ()
main = do
    let plaintext = "The ineffable talent for finding patterns in chaos cannot do its thing unless he immerses himself in the chaos first. If they do contain patterns, he does not see them just now, in any rational way. But there may be some subrational part of his mind that can go to work, now that the letters have passed before his eyes and through his pencil, and that may suddenly present him with a gift-wrapped clue--or even a full solution--a few weeks from now while he is shaving or antenna-twiddling."
        mostCommonPlaintextChar = fromJust $ mostCommonChar plaintext
    dataPath <- getDataFileName "4.txt"
    content <- readFile dataPath
    print $ catMaybes (map (decryptIfPrintOrSpace mostCommonPlaintextChar) (lines content))
