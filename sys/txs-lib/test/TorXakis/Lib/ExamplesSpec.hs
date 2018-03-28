{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TorXakis.Lib.ExamplesSpec where

import           Test.Hspec (Spec, it)
import           TorXakis.Lib.Examples

spec :: Spec
spec = do
    it "testEchoReactive" testEchoReactive
    it "testWrongFile"    testWrongFile
    it "testInfo"         testInfo
