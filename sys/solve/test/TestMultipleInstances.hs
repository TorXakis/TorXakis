{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module TestMultipleInstances
(
testMultipleInstancesList
)
where
import           Control.Monad.State
import           System.Process      (CreateProcess)

import           Test.HUnit

import           SMT
import           SolveDefs
import           StdTDefs
import           TxsDefs

import           TestSolvers

testMultipleInstancesList :: Test
testMultipleInstancesList = TestList $ testsSolvers defaultSMTProcs
---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
testsSolvers :: [CreateProcess] -> [Test]
testsSolvers [] = []
testsSolvers x  = map (testSolvers (head x)) x ++ testsSolvers (tail x)

testSolvers :: CreateProcess -> CreateProcess -> Test
testSolvers s1 s2 = TestLabel "Two instances" $ TestCase $ do
    smtEnv1 <- createSMTEnv s1 False TxsDefs.empty
    smtEnv2 <- createSMTEnv s2 False TxsDefs.empty

    let v = VarId "instance" 1234 sortId_Int
    smtEnv1' <- execStateT openSolver smtEnv1
    smtEnv2' <- execStateT openSolver smtEnv2

    smtEnv1'' <- execStateT (addDeclarations [v]) smtEnv1'
    smtEnv2'' <- execStateT (addDeclarations [v]) smtEnv2'

    let assertion1 = cstrLT (cstrVar v) (cstrConst (Cint 0))
    smtEnv1''' <- execStateT (addAssertions [assertion1]) smtEnv1''

    let assertion2 = cstrGT (cstrVar v) (cstrConst (Cint 0))
    smtEnv2''' <- execStateT (addAssertions [assertion2]) smtEnv2''

    (resp1, smtEnv1'''') <- runStateT getSolvable smtEnv1'''
    (resp2, smtEnv2'''') <- runStateT getSolvable smtEnv2'''

    (_,_) <- runStateT close smtEnv1''''
    (_,_) <- runStateT close smtEnv2''''

    assertEqual "sat" Sat resp1
    assertEqual "sat" Sat resp2
