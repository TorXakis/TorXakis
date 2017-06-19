{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

module TestExternal
(
testExternalList
)
where
import Test.HUnit

import Control.Monad.State
import qualified Data.Map as Map
import System.Process(CreateProcess)

import SMT
import SMTData
import SMTInternal
import TxsDefs

import SolveDefs.Params

smtSolvers :: [CreateProcess]
smtSolvers = [cmdCVC4, cmdZ3]

testExternalList :: Test
testExternalList = 
    TestList $ concatMap (\s -> map (\e -> TestLabel (fst e) $ TestCase $ do smtEnv <- createSMTEnv s False TxsDefs.empty
                                                                             evalStateT (snd e) smtEnv ) 
                                    ioeTestList
                         )
                         smtSolvers

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