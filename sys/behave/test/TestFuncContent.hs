{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

module TestFuncContent
(
testFuncContentList
)
where
import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map

-- import qualified Debug.Trace as Trace

import TxsAlex
import TxsHappy
import TxsDefs hiding (var)
import TxsShow

import TestHelperFuncContent

----------------------------------------------------------------------------------------
-- List of Tests
----------------------------------------------------------------------------------------
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
                                    TestLabel "User Defined Function"  testUserDefinedFunction
                                ,   TestLabel "Combinatorial"          testCombinatorialInt                                    
                                ]

---------------------------------------------------------------------------
-- Helper functions
---------------------------------------------------------------------------             

-------------------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------

testConstantInt :: Test
testConstantInt = TestCase $ do
    let
        funcName :: String
        funcName = "myFunc"
        parameterList :: TypedElements
        parameterList = []
        sortName :: String
        sortName = intSortName
        funcContent :: FuncContent
        funcContent = constantInt 12
        
        torXakisInput :: String
        torXakisInput = createFuncDef funcName parameterList sortName funcContent
        
        funcKey :: FuncKey
        funcKey = getFuncKey funcName parameterList sortName
      in do
        assertEqual "Constant Int" funcContent (getFuncContent torXakisInput funcKey)

testConstantString :: Test
testConstantString = TestCase $ do
    let
        funcName :: String
        funcName = "myFunc"
        parameterList :: TypedElements
        parameterList = []
        sortName :: String
        sortName = stringSortName
        funcContent :: FuncContent
        funcContent = constantString "Just a string"
        
        torXakisInput :: String
        torXakisInput = createFuncDef funcName parameterList sortName funcContent
        
        funcKey :: FuncKey
        funcKey = getFuncKey funcName parameterList sortName
      in do
        assertEqual "Constant String" funcContent (getFuncContent torXakisInput funcKey)

testConstantBool :: Test
testConstantBool = TestCase $ do
    let
        funcName :: String
        funcName = "myFunc"
        parameterList :: TypedElements
        parameterList = []
        sortName :: String
        sortName = boolSortName
        funcContent :: FuncContent
        funcContent = constantBool "True"
        
        torXakisInput :: String
        torXakisInput = createFuncDef funcName parameterList sortName funcContent
        
        funcKey :: FuncKey
        funcKey = getFuncKey funcName parameterList sortName
      in do
        assertEqual "Constant Bool" funcContent (getFuncContent torXakisInput funcKey)

       
testVariable :: Test
testVariable = TestCase $ do
    let
        funcName :: String
        funcName = "myFunc"
        varName :: String
        varName = "a"
        sortName :: String
        sortName = boolSortName
        parameterList :: TypedElements
        parameterList = [([varName], sortName)]
        funcContent :: FuncContent
        funcContent = var varName sortName
        
        torXakisInput :: String
        torXakisInput = createFuncDef funcName parameterList sortName funcContent
        
        funcKey :: FuncKey
        funcKey = getFuncKey funcName parameterList sortName
      in do
        assertEqual "Variable" funcContent (getFuncContent torXakisInput funcKey)

        
testITE :: Test
testITE = TestCase $ do
    let
        funcName :: String
        funcName = "myFunc"
        boolVarName :: String
        boolVarName = "b"
        intVar1Name :: String
        intVar1Name = "i1"
        intVar2Name :: String
        intVar2Name = "i2"
        parameterList :: TypedElements
        parameterList = [([boolVarName], boolSortName), ([intVar1Name,intVar2Name], intSortName)]
        funcContent :: FuncContent
        funcContent = ite (var boolVarName boolSortName)
                          (var intVar1Name intSortName)
                          (var intVar2Name intSortName)
        torXakisInput :: String
        torXakisInput = createFuncDef funcName parameterList intSortName funcContent
        
        funcKey :: FuncKey
        funcKey = getFuncKey funcName parameterList intSortName
      in do
        assertEqual "ITE" funcContent (getFuncContent torXakisInput funcKey)

testSubstitution :: Test
testSubstitution = TestCase $ do
    let
        funcName :: String
        funcName = "myFunc"
        parameterList :: TypedElements
        parameterList = []
        varName :: String
        varName = "s"
        sortName :: String
        sortName = stringSortName
        funcContent :: FuncContent
        funcContent = subst (Map.fromList [((var varName sortName), constantString "New York")])
                            (var varName sortName)
        torXakisInput :: String
        torXakisInput = createFuncDef funcName parameterList sortName funcContent
        
        funcKey :: FuncKey
        funcKey = getFuncKey funcName parameterList sortName
      in do
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
testUnaryOperatorList = TestList (map testFunction [   ("+", [(["x"], intSortName)], intSortName)
                                                   ,   ("-", [(["x"], intSortName)], intSortName)
                                                   ])
                                                
        
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
testFunction (name, parameterList, sortOut) = TestCase $ do
    let
        funcName :: String
        funcName = "myFunc"

        funcCallKey :: FuncKey
        funcCallKey = getFuncKey name parameterList sortOut
        
        funcContent :: FuncContent
        funcContent = functionCall funcCallKey (fromTypedElementsToFuncContents parameterList)
        
        var name sort = FuncContent (cstrVar (expectVarId name sort) )
        
        
        torXakisInput :: String
        torXakisInput = createFuncDef funcName parameterList sortOut funcContent
        
        funcKey :: FuncKey
        funcKey = getFuncKey funcName parameterList sortOut
      in do
        assertEqual "Function" funcContent (getFuncContent torXakisInput funcKey)
                                                                                                      
                
testCallConstant :: Test
testCallConstant = TestCase $ do
    let
        sortName :: String
        sortName = intSortName
        
        constName :: String
        constName = "myConst"
        constDef :: String
        constDef = createConstDef constName sortName (constantInt 3)
        
        constCallKey :: FuncKey
        constCallKey = getFuncKey constName [] sortName
        
        funcName :: String
        funcName = "myFunc"
        parameterList :: TypedElements
        parameterList = []
                
        funcContent :: FuncContent
        funcContent = functionCall constCallKey []
        
        funcDef :: String
        funcDef = createFuncDef funcName parameterList sortName funcContent
        
        torXakisInput :: String
        torXakisInput = constDef ++ "\n" ++ funcDef
        
        funcKey :: FuncKey
        funcKey = getFuncKey funcName parameterList sortName
      in do
        assertEqual "const call" funcContent (getFuncContent torXakisInput funcKey)

testUserDefinedFunction :: Test
testUserDefinedFunction = TestCase $ do
    let
        sortName :: String
        sortName = intSortName
        
        userdefinedFunctionName :: String
        userdefinedFunctionName = "userDefinedFunction"
        userDefinedParameterName :: String
        userDefinedParameterName = "userDefinedParameterName"
        userDefinedParameterList :: TypedElements
        userDefinedParameterList = [([userDefinedParameterName], sortName)]
        userdefinedFuncDef :: String
        userdefinedFuncDef = createFuncDef userdefinedFunctionName userDefinedParameterList sortName (var userDefinedParameterName sortName)
        userdefinedFuncCallKey :: FuncKey
        userdefinedFuncCallKey = getFuncKey userdefinedFunctionName userDefinedParameterList sortName
        
        funcName :: String
        funcName = "myFunc"
        parameterList :: TypedElements
        parameterList = []
        funcContent :: FuncContent
        funcContent = functionCall userdefinedFuncCallKey [constantInt 0]
        funcDef :: String
        funcDef = createFuncDef funcName parameterList sortName funcContent
        
        torXakisInput :: String
        torXakisInput = userdefinedFuncDef ++ "\n" ++ funcDef
        
        funcKey :: FuncKey
        funcKey = getFuncKey funcName parameterList sortName
      in do
        assertEqual "call to user defined function" funcContent (getFuncContent torXakisInput funcKey)

        
        
        
        
createConstBoolFuncContent :: Integer -> [FuncContent]
createConstBoolFuncContent _ = [constantBool "True", constantBool "False"]

                         
varBool1Name :: String
varBool1Name = "b1"

varBool2Name :: String
varBool2Name = "b2"


createVarBoolFuncContent :: Integer -> [FuncContent]
createVarBoolFuncContent _ = [var varBool1Name boolSortName,var varBool2Name boolSortName]

createBoolFuncContent :: Integer -> [FuncContent]
createBoolFuncContent boundary = 
        (createConstBoolFuncContent boundary)
     ++ (createVarBoolFuncContent boundary)


createConstIntFuncContent :: Integer -> [FuncContent]
createConstIntFuncContent _ = [constantInt 0, constantInt 10]

varInt1Name :: String
varInt1Name = "i1"

varInt2Name :: String
varInt2Name = "i2"

createVarIntFuncContent :: Integer -> [FuncContent]
createVarIntFuncContent _ = [var varInt1Name intSortName,var varInt2Name intSortName]

createUnaryIntFuncContent :: Integer -> [FuncContent]
createUnaryIntFuncContent boundary = 
        if boundary == 0 
            then []
            else  
                let 
                    minusFuncKey = getFuncKey "-" [(["x"],intSortName)] intSortName
                    absFuncKey = getFuncKey "abs" [(["x"],intSortName)] intSortName
                  in 
                    concat (map (\funcKey -> map (\x -> functionCall funcKey [x]) (createIntFuncContent (boundary-1))) [minusFuncKey, absFuncKey])

sumTuple :: Integer -> [(Integer,Integer)]
sumTuple max =
        map createTuple [0..max]
    where 
        createTuple :: Integer -> (Integer,Integer)
        createTuple n = (n, max-n)
        
createBinaryIntFuncContent :: Integer -> [FuncContent]
createBinaryIntFuncContent boundary = 
        if boundary <= 1 
            then []
            else
                concat (map createInstances (sumTuple (boundary-2) ))
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
            else 
                concat (map createInstances (sumTuple (boundary-2) ) )
    where
         createInstances :: (Integer,Integer) -> [FuncContent] 
         createInstances (n,m) = 
            let xs = createIntFuncContent n
                ys = createIntFuncContent m
              in 
                [subst (Map.fromList [(var varInt1Name intSortName, x)]) y | x <- xs, y <- ys]

sumTuple3 :: Integer -> [(Integer,Integer,Integer)]
sumTuple3 max =
        concat (map createTuple3 [0..max])
    where 
        createTuple3 :: Integer -> [(Integer,Integer,Integer)]
        createTuple3 n = [(n,x,y) | (x,y) <- sumTuple (max-n)]
            
                
createITEIntFuncContent :: Integer -> [FuncContent]
createITEIntFuncContent boundary = 
        if boundary <= 2 
            then []
            else 
                concat (map createInstances (sumTuple3 (boundary-3) ) )
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
        (createConstIntFuncContent boundary)
     ++ (createVarIntFuncContent boundary)
     ++ (createUnaryIntFuncContent boundary)
     ++ (createBinaryIntFuncContent boundary) 
     ++ (createSubstituteIntFuncContent boundary)
     ++ (createITEIntFuncContent boundary)

     
testFuncContent :: FuncContent -> Test
testFuncContent funcContent = TestCase $ do
    let
        funcName :: String
        funcName = "myFunc"
        
        parameterList :: TypedElements
        parameterList = [([varInt1Name, varInt2Name], intSortName),([varBool1Name, varBool2Name], boolSortName)]
        
        torXakisInput :: String
        torXakisInput = createFuncDef funcName parameterList intSortName funcContent
        
        funcKey :: FuncKey
        funcKey = getFuncKey funcName parameterList intSortName
      in do
        assertEqual "FuncContent" funcContent (getFuncContent torXakisInput funcKey)
 
testCombinatorialInt = TestList (map testFuncContent (createIntFuncContent 5))
