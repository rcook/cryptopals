--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module Cryptopals.Util
    ( isPrintOrSpace
    , mostCommonChar
    ) where

import           Data.Char (isPrint, isSpace)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

scoreText :: String -> HashMap Char Int
scoreText =
    foldr
        (\c m -> let k = c in HashMap.insertWith (\_ x -> x + 1) k 1 m)
        HashMap.empty

mostCommonChar :: String -> Maybe Char
mostCommonChar [] = Nothing
mostCommonChar s =
    let (c, _) = HashMap.foldlWithKey' (\p@(_, n) c' n' -> if n' > n then (c', n') else p) (' ', -1) (scoreText s)
    in Just c

isPrintOrSpace :: Char -> Bool
isPrintOrSpace x = isPrint x || isSpace x
