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
import qualified Data.Text           as T
import           System.Process      (CreateProcess)

import           Constant
import           CstrDef
import           CstrId
import           FuncId
import           SMT
import           SMTData
import           SortDef
import           SortId
import           SolveDefs
import           TXS2SMT
import           VarId

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
    let isNil = FuncId "isNil" 9875 [sortId_ListInt] sortIdBool

    let constrId = CstrId "Constr" 2346 [sortIdInt, sortId_ListInt] sortId_ListInt
    let isConstr = FuncId "isConstr" 9876 [sortId_ListInt] sortIdBool
    let tl = FuncId "tail" 6565 [sortId_ListInt] sortId_ListInt
    let hd = FuncId "head" 6566 [sortId_ListInt] sortIdInt

    -- ----------------------------------
    -- function
    -- ----------------------------------
    let lengthList = FuncId "lengthList" 19876 [sortId_ListInt] sortIdInt

    let maps = EnvNames (Map.fromList [ (sortId_ListInt, "ListInt")
                                      , (sortIdInt, "Int")
                                      , (sortIdBool, "Bool")
                                      ])
                        (Map.fromList [ (nilId, "Nil")
                                      , (constrId, "Constr")
                                      ])
                        (Map.fromList [ (lengthList, "lengthList")
                                      , (hd, "head")
                                      , (tl, "tail")
                                      ])
    
    let varList = VarId "list" 645421 sortId_ListInt
    let varListIO = createVvar varList
    let ve = createVite (createIsConstructor nilId varListIO) (createVconst (Cint 0)) (createVsum [createVconst (Cint 1), createVfunc lengthList [createVfunc tl [varListIO]]])

    let (TXS2SMTFuncTest fDefs e) = createFunctionDefRecursive maps lengthList [varList] sortIdInt ve

    let sDefs = EnvDefs (Map.fromList [(sortId_ListInt,SortDef)])
                        (Map.fromList [(nilId,CstrDef isNil []), (constrId,CstrDef isConstr [hd, tl])])
                        Map.empty
    
    let result = T.unpack (sortdefsToSMT maps sDefs)
    assertBool ("ListInt sortdef " ++ show result) ("(declare-datatypes () (\n    (ListInt (Constr (ListInt$Constr$0 Int) (ListInt$Constr$1 ListInt)) (Nil))\n) )" `List.isInfixOf` result)
    assertEqual "length function" e (T.unpack (funcdefsToSMT maps (funcDefs fDefs)))

    let txsDefs = EnvDefs (Map.union (sortDefs fDefs) (sortDefs sDefs))
                          (Map.union (cstrDefs fDefs) (cstrDefs sDefs))
                          (Map.union (funcDefs fDefs) (funcDefs sDefs))

    let v = VarId "instance" 123456 sortId_ListInt
    let (TXS2SMTVExprTest inputAssertion _) = createVgez (createVsum [createVfunc lengthList [createVvar v], createVconst (Cint 6)])

    smtEnv <- createSMTEnv s False

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
