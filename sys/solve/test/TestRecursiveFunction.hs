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
import           Data.Monoid
import qualified Data.Text           as T
import           Debug.Trace         as Trace
import           System.Process      (CreateProcess)

import           ConstDefs
import           FuncDef
import           FuncId
import           Identifier
import           SMT
import           SMTData
import           Sort
import           SolveDefs
import           TXS2SMT
import           VarId

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
    let listIntId = Name "ListInt"
        nilId = Name "Nil"
        tailId = Name "tail"
        headId = Name "head"
        constrId = Name "Constr"
        i2r = foldr addIdentifier Identifier.empty [listIntId,nilId,tailId,headId,constrId]

        adtRf = getReference listIntId i2r
        sort_ListInt = SortADT adtRf

        Right nilFields = fieldDefs []
        nilRf = getReference nilId i2r
        nilCDef = ConstructorDef { constructorName="Nil", fields=nilFields}

        tailRf = getReference tailId i2r
        headRf = getReference headId i2r
        Right constrFields = fieldDefs [(headRf, FieldDef "head" SortInt), (tailRf, FieldDef "tail" sort_ListInt)]
        constrRf = getReference constrId i2r
        constrCDef = ConstructorDef { constructorName="Constr", fields=constrFields}

        Right cDefs = constructorDefs [(nilRf, nilCDef),(constrRf, constrCDef)]
        adtDef = ADTDef "ListInt" cDefs
        Right aDefs = addADTDefs [(adtRf,adtDef)] emptyADTDefs

        lengthListId = FuncId "lengthList" 19876 [sort_ListInt] SortInt
        varList = VarId "list" 645421 sort_ListInt
        varListIO = createVvar varList
        ve = createVite
                (createIsConstructor adtRf nilRf varListIO)
                (createVconst (Cint 0))
                (createVsum [ createVconst (Cint 1)
                            , createVfunc lengthListId [createAccessor adtRf constrRf 1 sort_ListInt varListIO]
                            ])
        (TXS2SMTFuncTest i e) = createFunctionDefRecursive lengthListId [varList] SortInt ve
        (resultTxt,_) = adtDefsToSMT $ adtDefsToMap aDefs
    let expected = "(declare-datatypes () (\n    (" <> toADTName adtRf <> " ("
                                                 <> toCstrName adtRf constrRf <> " ("
                                                        <> toFieldName adtRf constrRf 0 <> " " <> toSortName SortInt <> ") ("
                                                        <> toFieldName adtRf constrRf 1 <> " " <> toSortName sort_ListInt <> ")) (" 
                                                 <> toCstrName adtRf nilRf <> "))\n) )"
    assertBool ("ListInt sortdef actual\n" ++ T.unpack resultTxt ++ "\nexpected\n" ++ T.unpack expected)
               (expected `T.isInfixOf` resultTxt)
    let funcDefs = Map.singleton lengthListId (FuncDef [varList] (HelperVexprToSMT.input ve))
    assertEqual "length function" e (funcdefsToSMT $ funcDefs)

    let v = VarId "instance" 123456 sort_ListInt
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
    assertBool ("value = " ++ (show value)) 
               (case value of
                    Cstr adtRf constrRf _   -> True
                    _                       -> False
               )
    _ <- execStateT close env7
    return ()