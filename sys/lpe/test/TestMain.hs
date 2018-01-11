{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Main where
import System.Exit
import Test.HUnit

import TestPreGNF
import TestGNF
import TestLPE
import TestLPEPar

testList :: Test
testList = TestList
    [
      TestLabel "test1" test1
       --  TestLabel "preGNF"          testPreGNFList
       -- , TestLabel "GNF"             testGNFList
       -- , TestLabel "LPE"             testLPEList
       -- , TestLabel "LPEPar"          testLPEParList
    ]

main :: IO ()
main = do
    Counts  _c _t e f <- runTestTT testList
    if 0 == e+f
        then exitSuccess
        else exitFailure


test1 :: Test
test1 = TestCase $
   assertEqual "test1" (Just (procInst', procDefPlpe)) (lpeTransform procInst procDefs)
   where
      procInst = ProcInst procIdP [chanIdA] []
      procIdP = procIdGen "P" [chanIdA] []
      procIdQ = procIdGen "Q" [chanIdB] []
