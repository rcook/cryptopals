--------------------------------------------------
-- Copyright (C) 2018, All rights reserved.
--------------------------------------------------

module CryptopalsSpec (spec) where

import           Cryptopals
import           Test.Hspec

spec :: Spec
spec = do
    describe "sample" $
        it "runs" $ sample