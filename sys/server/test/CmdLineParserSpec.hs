module CmdLineParserSpec (spec) where

import           CmdLineParser
import           Config
import           Network
import           Options.Applicative
import           Test.Hspec
import           Test.QuickCheck     hiding (Failure, Success)

-- | Function used to test the command line arguments parsing.
parserTesting = execParserPure defaultPrefs opts
  where opts = info optsP mempty

toCmdArgs :: CmdLineConfig -> [String]
toCmdArgs cfg =
  [ show (clPortNumber cfg)
  , "--smt-solver", show (clSmtSolver cfg) ]
  ++
  ["--smt-log" | clSmtLog cfg]

instance Arbitrary CmdLineConfig where
  arbitrary = CmdLineConfig <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SMTSolver where
  arbitrary = elements [Z3, CVC4]

instance Arbitrary PortNumber where
  arbitrary = (fromInteger . abs) <$> arbitrary

spec :: Spec
spec =
  describe "parseCmdLine" $ do
    it "parses the arguments correctly" $ property $
      \clConfig ->
        show (parserTesting (toCmdArgs clConfig)) === show (Success clConfig)
    it "fails when the port is missing" $
      getParseResult (parserTesting ["--smt-log"])
        `shouldBe` Nothing
    it "fails when a wrong SMT solver is given"  $
      getParseResult (parserTesting ["99", "--smt-solver", "BOOM"])
        `shouldBe` Nothing
