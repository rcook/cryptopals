module Main (main) where

import           Cryptopals
import qualified Data.ByteString.Char8 as Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as Lazy (pack)

main :: IO ()
main = do
    putStrLn "challenge07"

    Just s <- readBase64DataFile "7.txt"
    let bytes = Lazy.pack s
        key = Char8.pack "YELLOW SUBMARINE"
    print $ decryptAES128ECB key defaultIV bytes
