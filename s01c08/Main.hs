module Main (main) where

import           Cryptopals
import           Paths_cryptopals

main :: IO ()
main = do
    putStrLn "Set 1 challenge 8"

    path <- getDataFileName "8.txt"
    xs <- map (hexDecode . fromJust . hexString) . lines <$> readFile path
    print $ detectAES128ECB xs
