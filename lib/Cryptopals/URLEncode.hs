module Cryptopals.URLEncode
    ( decodeKeyValuePairs
    , encodeKeyValuePairs
    , encodeToken
    , keyValuePair
    , keyValuePairs
    , profileFor
    , token
    ) where

import           Cryptopals.Prelude
import           Data.Attoparsec.ByteString (Parser, endOfInput, many', parseOnly, sepBy1)
import           Data.Attoparsec.ByteString.Char8 (char, isAlpha_ascii, takeWhile1)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (concat)
import qualified Data.ByteString.Char8 as Char8 (pack, unpack)

oneOf :: [Char] -> Parser Char
oneOf xs = foldl1 (<|>) (map char xs)

hexChars :: [Char]
hexChars = "0123456789ABCDEF"

escape :: Parser ByteString
escape = do
    void $ char '%'
    d0 <- oneOf hexChars
    d1 <- oneOf hexChars
    let Just x0 = d0 `elemIndex` hexChars
        Just x1 = d1 `elemIndex` hexChars
        code = x0 * 16 + x1
    return $ Char8.pack [chr code]

token :: Parser ByteString
token = do
    xs <- many' (escape <|> takeWhile1 (\x -> isAlpha_ascii x || isDigit x || x == '@' || x == '.'))
    return $ ByteString.concat xs

keyValuePair :: Parser (ByteString, ByteString)
keyValuePair = do
    key <- token
    void $ char '='
    value <- token
    return (key, value)

keyValuePairs :: Parser [(ByteString, ByteString)]
keyValuePairs = keyValuePair `sepBy1` (char '&')

encodeToken :: String -> String
encodeToken = concat . encodeTokenHelper

encodeTokenHelper :: String -> [String]
encodeTokenHelper "" = []
encodeTokenHelper (x : xs)
    | isAlpha x || isDigit x || x == '@' || x == '.' = [x] : encodeTokenHelper xs
    | otherwise = printf "%%%0X" (fromEnum x) : encodeTokenHelper xs

profileFor :: String -> [(String, String)]
profileFor email =
    [ ("email", email)
    , ("uid", "10")
    , ("role", "user")
    ]

encodeKeyValuePairs :: [(String, String)] -> ByteString
encodeKeyValuePairs ps = Char8.pack (intercalate "&" (map (\(k, v) -> encodeToken k ++ "=" ++ encodeToken v) ps))

decodeKeyValuePairs :: ByteString -> Either String [(String, String)]
decodeKeyValuePairs s = do
    result <- parseOnly (keyValuePairs <* endOfInput) s
    return $ map (\(k, v) -> (Char8.unpack k, Char8.unpack v)) result
