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
import           Data.List.NonEmpty  (fromList)
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text           as T
import           System.Process      (CreateProcess)

import           ConstDefs
import           FuncDef
import           FuncId        hiding (name)
import           Name
import           SMT
import           Sort.Internal
import           SolveDefs
import           TXS2SMT
import           VarId         hiding (name)

import           HelperFuncDefToSMT
import           HelperVexprToSMT

import           TestSolvers

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

testRecursiveFunction :: CreateProcess -> Test
testRecursiveFunction s = TestLabel "recursive function" $ TestCase $ do
    let Right listIntName = name "ListInt"
        Right intName = name "Int"
        Right nilName = name "Nil"
        Right tailName = name "tail"
        Right headName = name "head"
        Right constrName = name "Constr"

        Right nilFields = fieldDefs []
        nilRf = RefByName nilName
        nilCDef = ConstructorDef nilName nilFields

        Right constrFields = fieldDefs [ FieldDef headName intName T.empty
                                       , FieldDef tailName listIntName T.empty ]
        constrRf = RefByName constrName
        constrCDef = ConstructorDef constrName constrFields

        Right cDefs = constructorDefs [nilCDef, constrCDef]
        adtDef = ADTDef listIntName cDefs
        adtRf = RefByName listIntName
        Right aDefs = addADTDefs [adtDef] emptyADTDefs
        sort_ListInt = SortADT adtRf

        lengthListId = FuncId (fromNonEmpty $ fromList "lengthList") 19876 [sort_ListInt] SortInt
        varList = VarId (fromNonEmpty $ fromList "list") 645421 sort_ListInt
        varListIO = createVvar varList
        ve = createVite
                (createIsConstructor adtRf nilRf varListIO)
                (createVconst (Cint 0))
                (createVsum [ createVconst (Cint 1)
                            , createVfunc lengthListId [createAccessor adtRf constrRf 1 sort_ListInt varListIO]
                            ])
        (TXS2SMTFuncTest _i e) = createFunctionDefRecursive lengthListId [varList] SortInt ve
        (resultTxt,_) = adtDefsToSMT $ adtDefsToMap aDefs
    let expectedTxt = "(declare-datatypes () (\n    (" <> toADTName adtRf <> " ("
                                                 <> toCstrName adtRf constrRf <> " ("
                                                        <> toFieldName adtRf constrRf 0 <> " " <> toSortName SortInt <> ") ("
                                                        <> toFieldName adtRf constrRf 1 <> " " <> toSortName sort_ListInt <> ")) (" 
                                                 <> toCstrName adtRf nilRf <> "))\n) )"
    assertBool ("ListInt sortdef actual\n" ++ T.unpack resultTxt ++ "\nexpected\n" ++ T.unpack expectedTxt)
               (expectedTxt `T.isInfixOf` resultTxt)
    let funcDefs = Map.singleton lengthListId (FuncDef [varList] (HelperVexprToSMT.input ve))
    assertEqual "length function" e (funcdefsToSMT funcDefs)

    let v = VarId (fromNonEmpty $ fromList "instance") 123456 sort_ListInt
    let (TXS2SMTVExprTest inputAssertion _) = createVgez (createVsum [createVfunc lengthListId [createVvar v], createVconst (Cint (-1))])

    smtEnv <- createSMTEnv s False

    (_,env1) <- runStateT SMT.openSolver smtEnv
    env2 <- execStateT (addADTDefinitions aDefs) env1
    env3 <- execStateT (addFuncDefinitions funcDefs) env2
    env4 <- execStateT (addDeclarations [v]) env3
    env5 <- execStateT (addAssertions [inputAssertion]) env4
    (resp,env6) <- runStateT getSolvable env5
    assertEqual "sat" Sat resp
    (sol,env7) <- runStateT (getSolution [v]) env6

    let m = Map.lookup v sol
    assertBool "Is Just" (isJust m)
    let value = fromJust m
    assertBool ("value = " ++ show value) 
               (case value of
                    Cstr{} -> True
                    _      -> False
               )
    _ <- execStateT close env7
    return ()