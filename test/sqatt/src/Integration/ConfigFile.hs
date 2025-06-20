{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module Integration.ConfigFile (testSet) where

import qualified Data.Text          as T
import           Prelude     hiding (FilePath)

import           Paths
import           Sqatt

testDir :: Sqatt.FilePath
testDir = "ConfigFile"

testSet :: TxsExampleSet
testSet = TxsExampleSet "ConfigFile #long"
              $  map newValTest   paramNewValues
              ++ map emptyValTest paramDefaultValues
              ++ map wrongValTest paramDefaultValues

paramNewValues :: [(Text, Text)]
paramNewValues = [ ("IncrementChoice_IntPower", "14")
                -- only IOCO is valid, enable when changable
                --  , ("ImpRel", "")
                 , ("IncrementChoice_IntRange", "165536")
                 , ("IncrementChoice_MaxGeneratedStringLength", "110")
                -- only ANGELIC is valid, enable when changable
                --  , ("InputCompletion", "")
                 , ("RandSolve_IntHalf", "11000")
                 , ("RandSolve_IntNum", "15")
                 , ("Randomization", "TrueBins")
                 , ("Sim_inputEager", "1")
                 , ("Test_inputEager", "1")
                 , ("TrueBins_Next", "Power")
                 , ("TrueBins_NrOfBins", "18")
                 , ("TrueBins_StringLength", "16")
                 , ("TrueBins_StringMode", "Length")
                 , ("max_rand_depth", "14")
                 , ("Sut_deltaTime", "15000")
                 , ("Sut_ioTime", "110")
                 , ("Sim_deltaTime", "11000")
                 , ("Sim_ioTime", "110")
                 ]
                 

newValTest :: (Text, Text) -> TxsExample
newValTest (pNm,pVl) = templateTest "New" pNm pVl pVl

paramDefaultValues :: [(Text, Text)]
paramDefaultValues = [ ("ImpRel", "IOCO")
                     , ("IncrementChoice_IntPower", "4")
                     , ("IncrementChoice_IntRange", "65536")
                     , ("IncrementChoice_MaxGeneratedStringLength", "10")
                     , ("InputCompletion", "ANGELIC")
                     , ("RandSolve_IntHalf", "1000")
                     , ("RandSolve_IntNum", "5")
                     , ("Randomization", "IncrementBins")
                     , ("Sim_inputEager", "0")
                     , ("Test_inputEager", "3")
                     , ("TrueBins_Next", "Linear")
                     , ("TrueBins_NrOfBins", "10")
                     , ("TrueBins_StringLength", "6")
                     , ("TrueBins_StringMode", "Regex")
                     , ("max_rand_depth", "4")
                     , ("Sut_deltaTime", "2000")
                     , ("Sut_ioTime", "10")
                     , ("Sim_deltaTime", "2000")
                     , ("Sim_ioTime", "10")
                     ]

emptyValTest :: (Text, Text) -> TxsExample
emptyValTest (pNm,defVal) = templateTest "Empty" pNm T.empty defVal

wrongValTest :: (Text, Text) -> TxsExample
wrongValTest (pNm,defVal) = templateTest "Wrong" pNm "WrongVal" defVal

templateTest :: String -> Text -> Text -> Text -> TxsExample
templateTest exPrefix pNm newVal expVal = emptyExample
    { exampleName = exPrefix ++ " value for: " ++ T.unpack pNm
    , setupAction = setupParamTest pNm newVal
    , tearDownAction = tearDownParamTest pNm
    , txsCmdsFiles = [ txsCmdPath ITest testDir pNm ]
    , expectedResult = Message $ "param_" <> pNm <> " = " <> expVal
    }

txsConfigFileName :: Sqatt.FilePath
txsConfigFileName = ".torxakis.yaml"

setupParamTest :: Text -> Text -> IO ()
setupParamTest pNm pVl = do
    createParamConfigFile pNm pVl
    createParamCmdFile pNm

tearDownParamTest :: Text -> IO ()
tearDownParamTest pNm = do
    deleteParamConfigFile
    deleteParamCmdFile
  where
    deleteParamConfigFile = rm txsConfigFileName
    deleteParamCmdFile = rm $ txsCmdPath ITest testDir pNm

createParamConfigFile :: Text -> Text -> IO ()
createParamConfigFile pNm pVl =
        output txsConfigFileName
            $ fileContent ( "parameters:\n"
                          <> "- name: \"" <> pNm <> "\"\n"
                          <> "  value: \"" <> pVl <> "\"")

fileContent :: Text -> Shell Line
fileContent = select . textToLines

createParamCmdFile :: Text -> IO ()
createParamCmdFile pNm =
    output (txsCmdPath ITest testDir pNm)
           (fileContent $ "param param_" <> pNm <> "\nexit")
