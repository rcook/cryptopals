module Main (main) where

import           Cryptopals
import           Paths_cryptopals

main :: IO ()
main = do
    putStrLn "challenge08"

    path <- getDataFileName "8.txt"
    xs <- map (hexDecode . fromJust . hexString) . lines <$> readFile path
    print $ detectAES128ECB xs
