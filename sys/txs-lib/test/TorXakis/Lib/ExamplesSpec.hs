{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TorXakis.Lib.ExamplesSpec where

import           Control.Monad         (replicateM)
import           Data.Either           (isRight)
import           Test.Hspec            (Spec, it, shouldBe, shouldSatisfy)

import           TxsDDefs              (Verdict (Pass))

import           TorXakis.Lib          (isError)
import           TorXakis.Lib.Examples
import           TorXakis.Lib.Internal

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
    it "testPutToWReadsWorld" $ do
        res <- replicateM 100 testPutToWReadsWorld
        res `shouldSatisfy` all (True ==)
