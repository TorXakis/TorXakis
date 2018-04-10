{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TorXakis.Lib.ExamplesSpec where

import           Control.Monad         (replicateM)
import           Test.Hspec            (Spec, it, shouldBe, shouldSatisfy)

import           TxsDDefs              (Verdict (Pass))

import           TorXakis.Lib          (isError)
import           TorXakis.Lib.Examples

spec :: Spec
spec = do
    it "testEchoReactive" testEchoReactive
    it "testWrongFile"    $ do
        r <- testWrongFile
        r `shouldSatisfy` isError
    it "testInfo"         testInfo
    it "testTorXakisWithInfo" $ do
        Right res <- testTorXakisWithInfo
        res `shouldBe` Pass
    it "testTorXakisWithEcho" $ do
        Right res <- testTorXakisWithEcho
        res `shouldBe` Pass
    it "testPutToWReadsWorld" $ do
        res <- replicateM 100 testPutToWReadsWorld
        res `shouldSatisfy` all (True ==)
