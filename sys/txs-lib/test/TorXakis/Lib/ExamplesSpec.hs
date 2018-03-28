{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TorXakis.Lib.ExamplesSpec where

import           Test.Hspec            (Spec, it, shouldReturn, shouldSatisfy)
import           TorXakis.Lib          (Response (Success), isError)
import           TorXakis.Lib.Examples

spec :: Spec
spec = do
    it "testEchoReactive" testEchoReactive
    it "testWrongFile"    $ do
        r <- testWrongFile
        r `shouldSatisfy` isError
    it "testInfo"         testInfo
    it "testTorXakisWithInfo" $
        testTorXakisWithInfo `shouldReturn` Success
