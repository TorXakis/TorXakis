module CmdLineParserSpec (spec) where

import Network
import Config
import Options.Applicative
import CmdLineParser
import Test.Hspec
import Test.QuickCheck hiding (Success, Failure)

-- | Function used to test the command line arguments parsing.
parserTesting xss = execParserPure defaultPrefs opts xss
  where opts = info optsP mempty

toCmdArgs :: CmdLineConfig -> [String]
toCmdArgs cfg =
  [ show (clPortNumber cfg)
  , "--smt-solver", show (clSmtSolver cfg) ]
  ++
  if clSmtLog cfg then ["--smt-log"] else []

instance Arbitrary CmdLineConfig where
  arbitrary = CmdLineConfig <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SMTSolver where
  arbitrary = elements [Z3, CVC4]

instance Arbitrary PortNumber where  
  arbitrary = (fromInteger . abs) <$> arbitrary
  
spec :: Spec
spec = do
  describe "parseCmdLine" $ do
    it "parses the arguments correctly" $ property $
      \clConfig ->
        show (parserTesting (toCmdArgs clConfig)) === show (Success clConfig)
    it "fails when the port is missing" $ do
      getParseResult (parserTesting ["--smt-log"])
        `shouldBe` Nothing
    it "fails when a wrong SMT solver is given"  $ do
      getParseResult (parserTesting ["99", "--smt-solver", "BOOM"])
        `shouldBe` Nothing
