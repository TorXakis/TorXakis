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
import           System.Process      (CreateProcess)

import           ConstDefs
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
    let i2r = Identifier.empty

        listIntId = Name "ListInt"
    let i2r = addIdentifier listIntId i2r
        adtRf = getReference listIntId i2r
        sort_ListInt = SortADT adtRf

        Right nilFields = fieldDefs []
        nilId = Name "Nil"
    let i2r = addIdentifier nilId i2r
        nilRf = getReference nilId i2r
        nilCDef = ConstructorDef { constructorName="Nil", fields=nilFields}

        tailId = Name "tail"
    let i2r = addIdentifier tailId i2r
        tailRf = getReference tailId i2r

        headId = Name "head"
    let i2r = addIdentifier headId i2r
        headRf = getReference headId i2r

        Right constrFields = fieldDefs [(headRf, FieldDef "head" SortInt), (tailRf, FieldDef "tail" sort_ListInt)]
        constrId = Name "Constr"
    let i2r = addIdentifier constrId i2r
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
    assertBool ("ListInt sortdef " ++ T.unpack resultTxt)
               ("(declare-datatypes () (\n    (ListInt (Constr (ListInt$Constr$0 Int) (ListInt$Constr$1 ListInt)) (Nil))\n) )" `T.isInfixOf` resultTxt)
    {-assertEqual "length function" e (T.unpack (funcdefsToSMT maps (funcDefs fDefs)))

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
    _ <- execStateT close env7 -}
    return ()
