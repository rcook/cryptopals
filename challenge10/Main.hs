-- http://cryptopals.com/sets/2/challenges/10

module Main (main) where

import           Cryptopals
import qualified Data.ByteString.Char8 as Char8 (pack)

main :: IO ()
main = do
    putStrLn "challenge10"

    let key = Char8.pack "YELLOW SUBMARINE"
        chunkSize = 16
    s <- (Char8.pack . fromJust) <$> readBase64DataFile "10.txt"
    print $ decryptCBC chunkSize key s
