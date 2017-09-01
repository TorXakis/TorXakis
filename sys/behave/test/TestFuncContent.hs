{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TestFuncContent
(
testFuncContentList
)
where
import Test.HUnit
import qualified Data.Map as Map

import TestHelperFuncContent

----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
testFuncContentList :: Test
testFuncContentList = TestList [
                                    TestLabel "Constant Int"           testConstantInt,
                                    TestLabel "Constant String"        testConstantString,
                                    TestLabel "Constant Bool"          testConstantBool,
                                    TestLabel "Variable"               testVariable,
                                    TestLabel "If Then Else"           testITE,
                                    TestLabel "Substitution"           testSubstitution,
                                    TestLabel "Call Constant"          testCallConstant,
                                    TestLabel "Binary Operator"        testBinaryOperatorList,
                                    TestLabel "Unary Operator"         testUnaryOperatorList,
                                    TestLabel "Function"               testFunctionList,
                                    TestLabel "User Defined Function"  testUserDefinedFunction,
                                    TestLabel "Combinatorial"          testCombinatorialInt
                                ]

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------

createFuncDefWithKey :: String -> TypedElements -> String -> FuncContent -> (String, FuncKey)
createFuncDefWithKey funcName parameterList sortName funcContent =
    ( createFuncDef funcName parameterList sortName funcContent
    , getFuncKey funcName parameterList sortName)
---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------

testConstantInt :: Test
testConstantInt = TestCase $
    let
        funcContent = constantInt 12
        (torXakisInput, funcKey) = createFuncDefWithKey "myFunc" [] intSortName funcContent
      in
        assertEqual "Constant Int" funcContent (getFuncContent torXakisInput funcKey)

testConstantString :: Test
testConstantString = TestCase $
    let
        funcContent = constantString "Just a string"
        (torXakisInput, funcKey) = createFuncDefWithKey "myFunc" [] stringSortName funcContent
      in
        assertEqual "Constant String" funcContent (getFuncContent torXakisInput funcKey)

testConstantBool :: Test
testConstantBool = TestCase $
    let
        funcContent = constantBool "True"
        (torXakisInput, funcKey) = createFuncDefWithKey "myFunc" [] boolSortName funcContent
      in
        assertEqual "Constant Bool" funcContent (getFuncContent torXakisInput funcKey)

       
testVariable :: Test
testVariable = TestCase $
    let
        varName = "a"
        sortName = boolSortName
        parameterList = [([varName], sortName)]
        funcContent = varContent varName sortName
        (torXakisInput, funcKey) = createFuncDefWithKey "myFunc" parameterList sortName funcContent
      in
        assertEqual "Variable" funcContent (getFuncContent torXakisInput funcKey)

        
testITE :: Test
testITE = TestCase $
    let
        boolVarName :: String
        boolVarName = "b"
        intVar1Name :: String
        intVar1Name = "i1"
        intVar2Name :: String
        intVar2Name = "i2"
        parameterList :: TypedElements
        parameterList = [([boolVarName], boolSortName), ([intVar1Name,intVar2Name], intSortName)]
        funcContent :: FuncContent
        funcContent = ite (varContent boolVarName boolSortName)
                          (varContent intVar1Name intSortName)
                          (varContent intVar2Name intSortName)
                          
        (torXakisInput, funcKey) = createFuncDefWithKey "myFunc" parameterList intSortName funcContent
      in
        assertEqual "ITE" funcContent (getFuncContent torXakisInput funcKey)

testSubstitution :: Test
testSubstitution = TestCase $
    let
        varName :: String
        varName = "s"
        sortName :: String
        sortName = stringSortName
        funcContent :: FuncContent
        funcContent = subst (Map.fromList [(varContent varName sortName, constantString "New York")])
                            (varContent varName sortName)
        
        (torXakisInput, funcKey) = createFuncDefWithKey "myFunc" [] sortName funcContent
      in
        assertEqual "Substitute" funcContent (getFuncContent torXakisInput funcKey)

testBinaryOperatorList :: Test
testBinaryOperatorList = TestList (map testFunction     [   ("==", [(["l","r"], boolSortName)], boolSortName)
                                                        ,   ("<>", [(["l","r"], boolSortName)], boolSortName)
                                                        ,   ("/\\", [(["l","r"], boolSortName)], boolSortName)
                                                        ,   ("\\/", [(["l","r"], boolSortName)], boolSortName)
                                                        ,   ("=>", [(["l","r"], boolSortName)], boolSortName)
                                                        ,   ("==", [(["l","r"], intSortName)], boolSortName)
                                                        ,   ("<>", [(["l","r"], intSortName)], boolSortName)
                                                        ,   ("<",  [(["l","r"], intSortName)], boolSortName)
                                                        ,   ("<=", [(["l","r"], intSortName)], boolSortName)
                                                        ,   (">",  [(["l","r"], intSortName)], boolSortName)
                                                        ,   (">=", [(["l","r"], intSortName)], boolSortName)                                                
                                                        ,   ("+",  [(["l","r"], intSortName)], intSortName)
                                                        ,   ("-",  [(["l","r"], intSortName)], intSortName)
                                                        ,   ("*",  [(["l","r"], intSortName)], intSortName)
                                                        ,   ("/",  [(["l","r"], intSortName)], intSortName)
                                                        ,   ("%",  [(["l","r"], intSortName)], intSortName)
                                                        ,   ("==", [(["l","r"], stringSortName)], boolSortName)
                                                        ,   ("<>", [(["l","r"], stringSortName)], boolSortName)
                                                        ,   ("++", [(["l","r"], stringSortName)], stringSortName)
                                                        ])

testUnaryOperatorList :: Test
testUnaryOperatorList = TestList (map testFunction [("-", [(["x"], intSortName)], intSortName) ])
                                                
        
testFunctionList :: Test
testFunctionList = TestList (map testFunction [   ("toString", [(["b"], boolSortName)], stringSortName)
                                              ,   ("fromString", [(["s"], stringSortName)], boolSortName)
                                              ,   ("not", [(["b"], boolSortName)], boolSortName)
                                              ,   ("toString", [(["i"], intSortName)], stringSortName)
                                              ,   ("fromString", [(["s"], stringSortName)], intSortName)
                                              ,   ("abs", [(["i"], intSortName)], intSortName)
                                              ,   ("len", [(["s"], stringSortName)], intSortName)
                                              ,   ("at", [(["s"], stringSortName),(["i"], intSortName)], stringSortName)
                                              ,   ("strinre", [(["s"], stringSortName),(["r"], regexSortName)], boolSortName)
                                              ])


testFunction :: (String, TypedElements, String) -> Test
testFunction (nm, parameterList, sortOut) = TestCase $
    let
        funcCallKey :: FuncKey
        funcCallKey = getFuncKey nm parameterList sortOut
        
        funcContent :: FuncContent
        funcContent = functionCall funcCallKey (fromTypedElementsToFuncContents parameterList)
        
        (torXakisInput, funcKey) = createFuncDefWithKey "myFunc" parameterList sortOut funcContent
      in
        assertEqual "Function" funcContent (getFuncContent torXakisInput funcKey)
                                                                                                      
                
testCallConstant :: Test
testCallConstant = TestCase $
    let
        constName :: String
        constName = "myConst"
        
        constDef :: String
        constDef = createConstDef constName intSortName (constantInt 3)
        
        constCallKey :: FuncKey
        constCallKey = getFuncKey constName [] intSortName
        
        funcContent :: FuncContent
        funcContent = functionCall constCallKey []
        
        (funcDef, funcKey) = createFuncDefWithKey "myFunc" [] intSortName funcContent
      in
        assertEqual "const call" funcContent (getFuncContent (constDef ++ "\n" ++ funcDef) funcKey)

testUserDefinedFunction :: Test
testUserDefinedFunction = TestCase $
    let
        sortName = intSortName
        userDefinedParameterName = "userDefinedParameterName"
        userDefinedParameterList = [([userDefinedParameterName], sortName)]
        (userdefinedFuncDef, userdefinedFuncCallKey) = createFuncDefWithKey "userDefinedFunction" userDefinedParameterList sortName (varContent userDefinedParameterName sortName)
        
        funcContent = functionCall userdefinedFuncCallKey [constantInt 0]
        (funcDef, funcKey) = createFuncDefWithKey "myFunc" [] sortName funcContent
      in
        assertEqual "call to user defined function" funcContent (getFuncContent (userdefinedFuncDef ++ "\n" ++ funcDef) funcKey)

createConstBoolFuncContent :: Integer -> [FuncContent]
createConstBoolFuncContent _ = [constantBool "True", constantBool "False"]

                         
varBool1Name :: String
varBool1Name = "b1"

varBool2Name :: String
varBool2Name = "b2"


createVarBoolFuncContent :: Integer -> [FuncContent]
createVarBoolFuncContent _ = [varContent varBool1Name boolSortName,varContent varBool2Name boolSortName]

createBoolFuncContent :: Integer -> [FuncContent]
createBoolFuncContent boundary = 
        createConstBoolFuncContent boundary
     ++ createVarBoolFuncContent boundary


createConstIntFuncContent :: Integer -> [FuncContent]
createConstIntFuncContent _ = [constantInt 0, constantInt 10]

varInt1Name :: String
varInt1Name = "i1"

varInt2Name :: String
varInt2Name = "i2"

createVarIntFuncContent :: Integer -> [FuncContent]
createVarIntFuncContent _ = [varContent varInt1Name intSortName,varContent varInt2Name intSortName]

createUnaryIntFuncContent :: Integer -> [FuncContent]
createUnaryIntFuncContent boundary = 
        if boundary == 0 
            then []
            else  
                let 
                    minusFuncKey = getFuncKey "-" [(["x"],intSortName)] intSortName
                    absFuncKey = getFuncKey "abs" [(["x"],intSortName)] intSortName
                  in 
                    concatMap (\funcKey -> map (\x -> functionCall funcKey [x]) (createIntFuncContent (boundary-1))) [minusFuncKey, absFuncKey]

sumTuple :: Integer -> [(Integer,Integer)]
sumTuple mx =
        map createTuple [0..mx]
    where 
        createTuple :: Integer -> (Integer,Integer)
        createTuple n = (n, mx-n)
        
createBinaryIntFuncContent :: Integer -> [FuncContent]
createBinaryIntFuncContent boundary = 
        if boundary <= 1 
            then []
            else concatMap createInstances (sumTuple (boundary-2) )
    where
         createInstances :: (Integer,Integer) -> [FuncContent] 
         createInstances (n,m) = 
            let xs = createIntFuncContent n
                ys = createIntFuncContent m 
                plusFuncKey = getFuncKey "+" [(["x","y"],intSortName)] intSortName
              in 
                [functionCall plusFuncKey [x,y] | x <- xs, y <- ys]

createSubstituteIntFuncContent :: Integer -> [FuncContent]
createSubstituteIntFuncContent boundary = 
        if boundary <= 1 
            then []
            else concatMap createInstances (sumTuple (boundary-2) )
    where
         createInstances :: (Integer,Integer) -> [FuncContent] 
         createInstances (n,m) = 
            let xs = createIntFuncContent n
                ys = createIntFuncContent m
              in 
                [subst (Map.fromList [(varContent varInt1Name intSortName, x)]) y | x <- xs, y <- ys]

sumTuple3 :: Integer -> [(Integer,Integer,Integer)]
sumTuple3 mx =
        concatMap createTuple3 [0..mx]
    where 
        createTuple3 :: Integer -> [(Integer,Integer,Integer)]
        createTuple3 n = [(n,x,y) | (x,y) <- sumTuple (mx-n)]
            
                
createITEIntFuncContent :: Integer -> [FuncContent]
createITEIntFuncContent boundary = 
        if boundary <= 2 
            then []
            else concatMap createInstances (sumTuple3 (boundary-3) )
    where
         createInstances :: (Integer,Integer,Integer) -> [FuncContent] 
         createInstances (k,l,m) = 
            let 
                bs = createBoolFuncContent k
                xs = createIntFuncContent l
                ys = createIntFuncContent m
              in 
                [ite b x y | b <- bs, x <- xs, y <- ys]
                
createIntFuncContent :: Integer -> [FuncContent]
createIntFuncContent boundary = 
        createConstIntFuncContent boundary
     ++ createVarIntFuncContent boundary
     ++ createUnaryIntFuncContent boundary
     ++ createBinaryIntFuncContent boundary
     ++ createSubstituteIntFuncContent boundary
     ++ createITEIntFuncContent boundary

     
testFuncContent :: FuncContent -> Test
testFuncContent funcContent = TestCase $
    let
        parameterList = [([varInt1Name, varInt2Name], intSortName),([varBool1Name, varBool2Name], boolSortName)]        
        (torXakisInput, funcKey) = createFuncDefWithKey "myFunc" parameterList intSortName funcContent
      in
        assertEqual "FuncContent" funcContent (getFuncContent torXakisInput funcKey)

testCombinatorialInt :: Test
testCombinatorialInt = TestList (map testFuncContent (createIntFuncContent 5))