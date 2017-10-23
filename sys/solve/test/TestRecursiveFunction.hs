{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module TestRecursiveFunction
(
testRecursiveFunctionList
)
where
import           Test.HUnit

import           Control.Monad.State
import qualified Data.List           as List
import qualified Data.Map            as Map
import           Data.Maybe
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as T
import           System.Process      (CreateProcess)

import           StdTDefs
import           Sum
import           TxsDefs

import           SMT
import           TXS2SMT

import           SolveDefs
import           SolveDefs.Params

import           HelperFuncDefToSMT
import           HelperVexprToSMT

import           TestSolvers

-- --------------------------------------------------------------
testRecursiveFunctionList :: Test
testRecursiveFunctionList =
    TestList $ concatMap (\s -> map (\e -> TestLabel (fst e) (snd e s) )
                                    testList
                         )
                         defaultSMTProcs



testList :: [(String, CreateProcess -> Test)]
testList = [
        ("recursive function", testRecursiveFunction)
    ]
---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
testRecursiveFunction :: CreateProcess -> Test
testRecursiveFunction s = TestLabel "recursive function" $ TestCase $ do
    -- -------------------
    -- datatype
    -- ---------------------
    let sortId_ListInt = SortId "ListInt" 1234

    let nilId = CstrId "Nil" 2345 [] sortId_ListInt
    let isNil = FuncId "isNil" 9875 [sortId_ListInt] sortId_Bool

    let constrId = CstrId "Constr" 2346 [sortId_Int, sortId_ListInt] sortId_ListInt
    let isConstr = FuncId "isConstr" 9876 [sortId_ListInt] sortId_Bool
    let tl = FuncId "tail" 6565 [sortId_ListInt] sortId_ListInt
    let hd = FuncId "head" 6566 [sortId_ListInt] sortId_Int

    -- ----------------------------------
    -- function
    -- ----------------------------------
    let lengthList = FuncId "lengthList" 19876 [sortId_ListInt] sortId_Int

    let maps = Map.insert (IdFunc lengthList) "lengthList"
               (Map.insert (IdSort sortId_ListInt) "ListInt"
                (Map.insert (IdCstr constrId) "Constr"
                 (Map.insert (IdFunc hd) "head"
                  (Map.insert (IdFunc tl) "tail"
                   (Map.insert (IdCstr nilId) "Nil"
                     (Map.fromList initialMapInstanceTxsToSmtlib)
                 )))))

    let varList = VarId "list" 645421 sortId_ListInt
    let varListIO = createVvar varList
    let ve = createVite (createIsConstructor nilId varListIO) (createVconst (Cint 0)) (createVsum [createVconst (Cint 1), createVfunc lengthList [createVfunc tl [varListIO]]])

    let (TXS2SMTFuncTest fDefs e) = createFunctionDefRecursive maps lengthList [varList] sortId_Int ve

    let sDefs = TxsDefs.insert (IdCstr nilId) (DefCstr (CstrDef isNil []) )
                 (TxsDefs.insert (IdCstr constrId) (DefCstr (CstrDef isConstr [hd, tl]) )
                   (TxsDefs.insert (IdSort sortId_ListInt) (DefSort SortDef)
                    TxsDefs.empty))
    let result = T.unpack (sortdefsToSMT maps sDefs)
    assertBool ("ListInt sortdef " ++ show result) ("(declare-datatypes () (\n    (ListInt (Constr (ListInt$Constr$0 Int) (ListInt$Constr$1 ListInt)) (Nil))\n) )" `List.isInfixOf` result)
    assertEqual "length function" e (T.unpack (funcdefsToSMT maps fDefs))

    let txsDefs = TxsDefs.union fDefs sDefs

    let v = VarId "instance" 123456 sortId_ListInt
    let (TXS2SMTVExprTest inputAssertion _) = createVgez (createVsum [createVfunc lengthList [createVvar v], createVconst (Cint 6)])

    smtEnv <- createSMTEnv s False TxsDefs.empty

    (_,env1) <- runStateT SMT.openSolver smtEnv
    env3 <- execStateT (addDefinitions txsDefs) env1
    env4 <- execStateT (addDeclarations [v]) env3
    env5 <- execStateT (addAssertions [inputAssertion]) env4
    (resp,env6) <- runStateT getSolvable env5
    assertEqual "sat" Sat resp
    (sol,env7) <- runStateT (getSolution [v]) env6

    let m = Map.lookup v sol
    assertBool "Is Just" (isJust m)
    -- let value = fromJust m
    -- Trace.trace ("value = " ++ (show value)) $ do
    _ <- execStateT close env7
    return ()

---------------------------------------------------------------------------
