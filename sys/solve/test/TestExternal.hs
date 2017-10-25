{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TestExternal
(
testExternalList
)
where
import           Test.HUnit

import           Control.Monad.State

import           SMT
import           SMTData
import           SMTInternal
import           TxsDefs

import           TestSolvers

testExternalList :: Test
testExternalList =
    TestList $ concatMap (\s -> map (\e -> TestLabel (fst e) $ TestCase $ do smtEnv <- createSMTEnv s False TxsDefs.empty
                                                                             evalStateT (snd e) smtEnv )
                                    ioeTestList
                         )
                         defaultSMTProcs

ioeTestList :: [(String, SMT())]
ioeTestList = [
            ("OpenClose",       testOpenClose),
            ("OpenSolveClose",  testOpenSolveClose)
        ]

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------

testOpenClose :: SMT ()
testOpenClose = do
    _ <- openSolver
    close

testOpenSolveClose :: SMT ()
testOpenSolveClose = do
    _ <- openSolver
    SMTInternal.put "(check-sat)"
    resp <- SMTInternal.getSMTresponse
    lift $ assertEqual "response" resp "sat"
    close
