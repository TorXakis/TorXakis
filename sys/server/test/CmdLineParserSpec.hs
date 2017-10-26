{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module CmdLineParserSpec (spec) where

import           CmdLineParser
import           Data.Maybe
import           Network
import           Options.Applicative
import           Test.Hspec
import           Test.QuickCheck     hiding (Failure, Success)
-- | Function used to test the command line arguments parsing.
parserTesting :: [String] -> ParserResult CmdLineConfig
parserTesting = execParserPure defaultPrefs opts
  where opts = info optsP mempty

toCmdArgs :: CmdLineConfig -> [String]
toCmdArgs cfg
  =  [ show (clPortNumber cfg) ]
  ++ ["--smt-solver" | isJust (clSmtSolver cfg)]
  ++ [ solver | isJust (clSmtSolver cfg)
                   , let solver = fromJust (clSmtSolver cfg)]
  ++ ["--smt-log" |  isJust (clSmtLog cfg)
                  && fromJust (clSmtLog cfg)]
  ++ ["--no-smt-log" |  isJust (clSmtLog cfg)
                  && not (fromJust (clSmtLog cfg))]

instance Arbitrary CmdLineConfig where
  arbitrary = CmdLineConfig <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PortNumber where
  arbitrary = (fromInteger . abs) <$> arbitrary

spec :: Spec
spec =
  describe "parseCmdLine" $ do
    it "parses the arguments correctly" $ property $
      \clConfig ->
        show (Success clConfig) === show (parserTesting (toCmdArgs clConfig))
    it "fails when the port is missing" $
      getParseResult (parserTesting ["--smt-log"])
        `shouldBe` Nothing
