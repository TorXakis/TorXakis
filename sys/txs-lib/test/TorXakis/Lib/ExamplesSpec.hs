{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TorXakis.Lib.ExamplesSpec where

import           Control.Monad         (replicateM)
import           Data.Either           (isLeft, isRight)
import           System.FilePath       ((</>))
import           Test.Hspec            (Spec, it, shouldBe, shouldSatisfy)

import           TorXakis.Lib.Examples


spec :: Spec
spec = do
    it "testEchoReactive" testEchoReactive
    it "testWrongFile"    $ do
        r <- testWrongFile
        r `shouldSatisfy` isLeft
    it "testInfo"         testInfo
    -- it "testTorXakisWithInfo" $ do
    --     Right res <- testTorXakisWithInfo
    --     res `shouldBe` Pass
    -- it "testTorXakisWithEcho" $ do
    --     Right res <- testTorXakisWithEcho
    --     res `shouldBe` Pass
    it "testPutToWReadsWorld" $ do
        res <- replicateM 100 testPutToWReadsWorld
        res `shouldSatisfy` all (True ==)
    it "testPrematureStop" testPrematureStop
    it "Action parsing right" $ do
        eAct <- testParseAction $ "test" </> "data" </> "Echo.txs"
        eAct `shouldSatisfy` isRight
    it "Action parsing wrong" $ do
        eAct <- testParseWrongAction $ "test" </> "data" </> "Echo.txs"
        eAct `shouldSatisfy` isLeft
    it "Stepping with user actions" $ do
        res <- testWithUserActions $ "test" </> "data" </> "Echo.txs"
        res `shouldBe` Right ()
    it "testVals" testVals
    it "testTester" $ do
        res <- testTester
        res `shouldBe` Just ()
    it "testTesterWithPurpose" $ do
        res <- testTesterWithPurpose
        res `shouldBe` Just ()
    it "testTesterWithSimulatorAndStop" $ do
        res <- testTesterWithSimulatorAndStop
        res `shouldBe` Just ()
    it "testMenu" $ do
        res <- testMenu
        res `shouldBe` Just ()
    it "testVarsAndSolvers" testVarsAndSolvers
    it "testNComp" testNComp
