{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module TestConstraint
(
testConstraintList
)
where
-- general Haskell imports
import           Control.Monad.Except
import           Control.Monad.State
import           Data.HashMap
--import           Text.Regex.TDFA

-- test specific Haskell imports
import           Test.HUnit

import           TorXakis.ContextVar
import           TorXakis.FuncDef
import           TorXakis.Name
import           TorXakis.ProblemSolver
import           TorXakis.SmtM
import           TorXakis.Sort
import           TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.Var

import           TestSolvers

testConstraintList :: Test
testConstraintList =
    TestList $ concatMap (\s -> Prelude.map (\e -> TestLabel (fst e) $ TestCase $ do
                                                                            es <- uncurry mkSmtState s False
                                                                            case es of
                                                                                Left err -> error (show err)
                                                                                Right ss -> do
                                                                                            r <- runExceptT $ execStateT (toStateT (snd e)) ss
                                                                                            case r of
                                                                                                Left err  -> error (show err)
                                                                                                Right ss' -> do
                                                                                                                me <- destroySmtState ss'
                                                                                                                case me of
                                                                                                                    Just err -> error (show err)
                                                                                                                    Nothing  -> return ()
                                    )
                                    labelTestList
                         )
                         defaultSMTProcs


labelTestList :: [(String, SmtM ())]
labelTestList = [
        ("None",                                        testNone),
        ("Negative of Negative Is Identity",            testNegativeNegativeIsIdentity),
        ("Add Equal sum = e1 + e2",                     testAdd),
        ("No Variables",                                testNoVariables),
        ("Bool",                                        testBool),
        ("Bool False",                                  testBoolFalse),
        ("Bool True",                                   testBoolTrue),
        ("Int",                                         testInt),
        ("Int Negative",                                testIntNegative) {-,
        ("Conditional Int Datatype",                    testConditionalInt),
        ("Conditional Int IsAbsent",                    testConditionalIntIsAbsent),
        ("Conditional Int IsPresent",                   testConditionalIntIsPresent),
        ("Conditional Int Present Value",               testConditionalIntPresentValue),
        ("Conditional Int Instances",                   testConditionalIntInstances),
        ("Nested Constructor",                          testNestedConstructor),
        ("Functions",                                   testFunctions),
        ("Just String",                                 testString)
   -}     ]
  {-  ++
       ioeTestStringEquals
    ++
        ioeTestStringLength
    ++
        ioeTestRegex

ioeTestStringEquals :: [(String, SMT())]
ioeTestStringEquals = [
        ("String Equals Empty",                     testStringEquals ""),
        ("String Equals Alphabet",                  testStringEquals "abcdefghijklmnopqrstuvwxyz"),
        ("String Equals SpecialChars",              testStringEquals "\a\t\n\r\0\x5E\126\127\128\xFF")
    ]
    ++ testStringEqualsChar

testStringEqualsChar :: [(String , SMT())]
testStringEqualsChar =
    map (\i -> ("char = " ++ [chr i], testStringEquals (T.pack [chr i])))  [0..255]

ioeTestStringLength :: [(String, SMT())]
ioeTestStringLength = [
        ("String Length    0",                     testStringLength    0),
        ("String Length   10",                     testStringLength   10),
        ("String Length  100",                     testStringLength  100),
        ("String Length 1000",                     testStringLength 1000)
    ]

ioeTestRegex :: [(String, SMT())]
ioeTestRegex = [
        ("Regex char",                      testRegex "X"),
        ("Regex concat chars",              testRegex "Jan"),
        ("Regex opt",                       testRegex "0?"),
        ("Regex plus",                      testRegex "a+"),
        ("Regex star",                      testRegex "b*"),
        ("Regex {n,m}",                     testRegex "c{3,5}"),
        ("Regex {n,}",                      testRegex "d{3,}"),
        ("Regex {n}",                       testRegex "r{3}"),
        ("Regex range",                     testRegex "[q-y]"),
        ("Regex 2 choices",                 testRegex "a|b"),
        ("Regex 3 choices",                 testRegex "a|b|c"),
        ("Regex group",                     testRegex "(a)"),
        ("Regex escape",                    testRegex "\\("),
        ("Regex escape operator",           testRegex "\\)+"),
        ("Regex escape range",              testRegex "\\|{2,4}"),
        ("Regex concat operator",           testRegex "ab+c*d?"),
        ("Regex mixed small",               testRegex "(ab+)|(p)"),
        ("Regex mixed large",               testRegex "(ab+c*d?)|(ef{2}g{3,6}h{3,})|(p)")
    ]
-}
-----------------------------------------------------
-- Helper function
-----------------------------------------------------
testTemplateSat :: [ValExpression] -> SmtM()
testTemplateSat createAssertions = do
    addAssertions createAssertions
    resp <- solvable
    liftIO $ assertEqual "sat" (SolvableProblem (Just True)) resp

testTemplateValue :: [ADTDef] -> [FuncDef] -> [VarDef] -> [ValExpression] -> SmtM ()
testTemplateValue as fs vs es = do
    TorXakis.ProblemSolver.addADTs as
    addFunctions fs
    _ <- push
    declareVariables vs
    ctx <- toValExprContext
    addAssertions es
    resp <- solve
    _ <- pop
    let Solved (Solution solution) = resp
        Right andExpr = mkAnd ctx es
        Right answer = subst ctx (Data.HashMap.map (\v -> case mkConst ctx v of
                                                               Right ve -> ve
                                                               Left e   -> error ("mkConst failed unexpectedly on constant " ++ show v ++ " with error " ++ show e)
                                                   )
                                                   solution
                                  )
                                  andExpr
      in do
        liftIO $ assertEqual "length" (length vs) (Data.HashMap.size solution)
        case view answer of
            Vconst (Cbool b) -> liftIO $ assertBool "Is valid solution" b
            _                -> error ("Answer of and is unexpectedly not a constant boolean, but " ++ show answer)
---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------

testNone :: SmtM ()
testNone = testTemplateSat []

testNegativeNegativeIsIdentity :: SmtM ()
testNegativeNegativeIsIdentity = 
    let ctx             = TorXakis.ContextVar.empty 
        Right x         = mkConst ctx (Cint 3)
        Right minX      = mkUnaryMinus ctx x
        Right minMinX   = mkUnaryMinus ctx minX
        Right equal     = mkEqual ctx x minMinX
     in
        testTemplateSat [equal]

testAdd :: SmtM()
testAdd =
    let ctx          = TorXakis.ContextVar.empty 
        Right a      = mkConst ctx (Cint 4)
        Right b      = mkConst ctx (Cint 9)
        Right add    = mkSum ctx [a,b]
        Right answer = mkConst ctx (Cint 13)
        Right equal  = mkEqual ctx answer add
      in
        testTemplateSat [equal]

-- --------------------------------------------------------------------------------------------------------------------
testNoVariables :: SmtM()
testNoVariables = testTemplateValue [] [] [] []

testBool :: SmtM()
testBool = 
    let Right nm = mkName "var"
        Right boolVar = mkVarDef TorXakis.ContextVar.empty nm SortBool
      in
        testTemplateValue [] [] [boolVar] []

testBoolTrue :: SmtM()
testBoolTrue = 
    let Right nm = mkName "var"
        Right boolVar = mkVarDef TorXakis.ContextVar.empty nm SortBool
        Right ctx = addVars [boolVar] TorXakis.ContextVar.empty
        ref :: RefByName VarDef
        ref = (RefByName nm)
        Right boolExpr = mkVar ctx ref
      in
        testTemplateValue [] [] [boolVar] [boolExpr]

testBoolFalse :: SmtM()
testBoolFalse = 
    let Right nm = mkName "var"
        Right boolVar = mkVarDef TorXakis.ContextVar.empty nm SortBool
        Right ctx = addVars [boolVar] TorXakis.ContextVar.empty
        ref :: RefByName VarDef
        ref = (RefByName nm)
        Right boolExpr = mkVar ctx ref
        Right notExpr = mkNot ctx boolExpr
      in
        testTemplateValue [] [] [boolVar] [notExpr]

testInt :: SmtM()
testInt = 
    let Right nm = mkName "var"
        Right intVar = mkVarDef TorXakis.ContextVar.empty nm SortInt
      in
        testTemplateValue [] [] [intVar] []

testIntNegative :: SmtM()
testIntNegative = 
    let Right nm = mkName "var"
        Right intVar = mkVarDef TorXakis.ContextVar.empty nm SortInt
        Right ctx = addVars [intVar] TorXakis.ContextVar.empty
        ref :: RefByName VarDef
        ref = (RefByName nm)
        Right intExpr = mkVar ctx ref
        Right constExpr = mkConst ctx (Cint 0)
        Right boolExpr = mkLT ctx intExpr constExpr
      in
        testTemplateValue [] [] [intVar] [boolExpr]

        {-

conditionalIntSortId :: SortId
conditionalIntSortId = SortId "conditionalInt" 234

absentCstrId :: CstrId
absentCstrId = CstrId "_absent" 2345 [] conditionalIntSortId

presentCstrId :: CstrId
presentCstrId = CstrId "_present" 2346 [sortIdInt] conditionalIntSortId

isAbsentCstrFunc :: FuncId
isAbsentCstrFunc = FuncId "is_absent" 9876 [conditionalIntSortId] sortIdBool

isPresentCstrFunc :: FuncId
isPresentCstrFunc = FuncId "is_present" 9877 [conditionalIntSortId] sortIdBool

valuePresentCstrFunc :: FuncId
valuePresentCstrFunc = FuncId "value" 6565 [conditionalIntSortId] sortIdInt

conditionalIntDef :: EnvDefs
conditionalIntDef = EnvDefs (Map.fromList [(conditionalIntSortId, SortDef)]) (Map.fromList [(absentCstrId,CstrDef isAbsentCstrFunc []), (presentCstrId, CstrDef isPresentCstrFunc [valuePresentCstrFunc])]) Map.empty

testConditionalInt :: SMT()
testConditionalInt = testTemplateValue conditionalIntDef [conditionalIntSortId] (const []) check
    where
        check :: [Constant] -> SMT()
        check [value] = case value of
            Ccstr x []       | x == absentCstrId  -> lift $ assertBool "expected pattern" True
            Ccstr x [Cint _] | x == presentCstrId -> lift $ assertBool "expected pattern" True
            _                                     -> lift $ assertBool "unexpected pattern" False
        check _         = error "One variable in problem"


testConditionalIntIsAbsent :: SMT()
testConditionalIntIsAbsent = testTemplateValue conditionalIntDef [conditionalIntSortId] createAssertions check
    where
        createAssertions :: [VarId] -> [ValExpr VarId]
        createAssertions [v] = [cstrIsCstr absentCstrId (cstrVar v)]
        createAssertions _   = error "One variable in problem"

        check :: [Constant] -> SMT()
        check [value]   = case value of
            Ccstr x [] | x == absentCstrId  -> lift $ assertBool "expected pattern" True
            _                               -> lift $ assertBool "unexpected pattern" False
        check _         = error "One variable in problem"


testConditionalIntIsPresent :: SMT()
testConditionalIntIsPresent = testTemplateValue conditionalIntDef [conditionalIntSortId] createAssertions check
    where
        createAssertions :: [VarId] -> [ValExpr VarId]
        createAssertions [v] = [cstrIsCstr presentCstrId (cstrVar v)]
        createAssertions _   = error "One variable in problem"

        check :: [Constant] -> SMT()
        check [value] = case value of
            Ccstr x [Cint _] | x == presentCstrId   -> lift $ assertBool "expected pattern" True
            _                                       -> lift $ assertBool "unexpected pattern" False
        check _         = error "One variable in problem"


testConditionalIntPresentValue :: SMT()
testConditionalIntPresentValue = testTemplateValue conditionalIntDef [conditionalIntSortId] createAssertions check
    where
        boundary :: Integer
        boundary = 4

        createAssertions :: [VarId] -> [ValExpr VarId]
        createAssertions [v]    = [ cstrIsCstr presentCstrId (cstrVar v)
                                  , cstrITE (cstrIsCstr presentCstrId (cstrVar v))
                                            (cstrGT (cstrAccess presentCstrId "value" 0 (cstrVar v)) (cstrConst (Cint boundary)) )
                                            (cstrConst (Cbool True))
                                  ]
        createAssertions _   = error "One variable in problem"

        check :: [Constant] -> SMT()
        check [value] = case value of
            Ccstr c [Cint x] | c == presentCstrId   -> lift $ assertBool "expected pattern" (x > boundary)
            _                                       -> lift $ assertBool "unexpected pattern" False
        check _         = error "One variable in problem"


check3Different :: [Constant] -> SMT()
check3Different [v1, v2, v3]    = do
                                    lift $ assertBool "value1 != value2" (v1 /= v2)
                                    lift $ assertBool "value1 != value3" (v1 /= v3)
                                    lift $ assertBool "value2 != value3" (v2 /= v3)
check3Different _               = error "Three variable in problem"

testConditionalIntInstances :: SMT()
testConditionalIntInstances = testTemplateValue conditionalIntDef
                                                [conditionalIntSortId,conditionalIntSortId,conditionalIntSortId]
                                                createAssertions
                                                check3Different
    where
        createAssertions :: [VarId] -> [ValExpr VarId]
        createAssertions [v1,v2,v3]    = [ cstrNot (cstrEqual (cstrVar v1) (cstrVar v2))
                                         , cstrNot (cstrEqual (cstrVar v2) (cstrVar v3))
                                         , cstrNot (cstrEqual (cstrVar v1) (cstrVar v3))
                                         ]
        createAssertions _   = error "Three variables in problem"


testNestedConstructor :: SMT()
testNestedConstructor = do
        let pairSortId = SortId "Pair" 12345
        let pairCstrId = CstrId "Pair" 2344 [sortIdInt,sortIdInt] pairSortId
        let absentPairCstrId = CstrId "Absent" 2345 [] conditionalPairSortId
        let presentPairCstrId = CstrId "Present" 2346 [pairSortId] conditionalPairSortId
        let conditionalPairDefs = EnvDefs (Map.fromList [ (conditionalPairSortId, SortDef), (pairSortId, SortDef) ])
                                          (Map.fromList [ (pairCstrId, CstrDef (FuncId "ignore" 9875 [] pairSortId) [FuncId "x" 6565 [] sortIdInt, FuncId "y" 6666 [] sortIdInt])
                                                        , (absentPairCstrId, CstrDef (FuncId "ignore" 9876 [] conditionalPairSortId) [])
                                                        , (presentPairCstrId, CstrDef (FuncId "ignore" 9877 [] conditionalPairSortId) [FuncId "value" 6767 [] pairSortId])
                                                        ])
                                          Map.empty

        testTemplateValue   conditionalPairDefs
                            [conditionalPairSortId,conditionalPairSortId,conditionalPairSortId]
                            createAssertions
                            check3Different
    where
        conditionalPairSortId = SortId "ConditionalPair" 9630

        createAssertions :: [VarId] -> [ValExpr VarId]
        createAssertions [v1,v2,v3]    = [ cstrNot (cstrEqual (cstrVar v1) (cstrVar v2))
                                         , cstrNot (cstrEqual (cstrVar v2) (cstrVar v3))
                                         , cstrNot (cstrEqual (cstrVar v1) (cstrVar v3))
                                         ]
        createAssertions _   = error "Three variables in problem"

testFunctions :: SMT()
testFunctions = do
        let varX = VarId "x" 645421 sortIdBool
        let varY = VarId "y" 645422 sortIdBool
        let body1 = cstrEqual (cstrVar varX) (cstrVar varY)
        let fd1 = FuncDef [varX, varY] body1
        let fd2 = FuncDef [] const2

        let tDefs = EnvDefs Map.empty Map.empty (Map.fromList [(fid1, fd1), (fid2, fd2)])

        testTemplateValue tDefs
                          [sortIdBool,sortIdBool,sortIdInt]
                          createAssertions
                          check
    where
        fid1 = FuncId "multipleArgsFunction" 123454321 [sortIdBool, sortIdBool] sortIdBool
        fid2 = FuncId "myConst" 12345678 [] sortIdInt
        const2 = cstrConst (Cint 3) :: ValExpr VarId

        createAssertions :: [VarId] -> [ValExpr VarId]
        createAssertions [b1,b2,i]    = [ cstrFunc (Map.empty :: Map.Map FuncId (FuncDef VarId)) fid1 [cstrVar b1, cstrVar b2]
                                        , cstrEqual (cstrVar i) (cstrFunc (Map.empty :: Map.Map FuncId (FuncDef VarId)) fid2 [])
                                        ]
        createAssertions _   = error "Three variables in problem"

        check :: [Constant] -> SMT()
        check [b1, b2, i] = do
                                lift $ assertEqual "booleans equal" b1 b2
                                lift $ assertEqual "i equal" const2  (cstrConst i)
        check _         = error "Three variable in problem"

testString :: SMT()
testString = testTemplateValue (EnvDefs Map.empty Map.empty Map.empty) [sortIdString] (const []) check
    where
        check :: [Constant] -> SMT()
        check [value]   = case value of
                            Cstring _   -> lift $ assertBool "expected pattern" True
                            _           -> lift $ assertBool "unexpected pattern" False
        check _         = error "One variable in problem"

testStringEquals :: Text -> SMT()
testStringEquals str = testTemplateValue (EnvDefs Map.empty Map.empty Map.empty) [sortIdString] createAssertions check
    where
        createAssertions :: [VarId] -> [ValExpr VarId]
        createAssertions [v] = [cstrEqual (cstrVar v) (cstrConst (Cstring str))]
        createAssertions _   = error "One variable in problem"

        check :: [Constant] -> SMT()
        check [value] = case value of
                            Cstring s   -> lift $ assertBool ("expected pattern s = " ++ T.unpack s)
                                                             (s == str)
                            _           -> lift $ assertBool "unexpected pattern" False
        check _         = error "One variable in problem"

testStringLength :: Int -> SMT()
testStringLength n = testTemplateValue (EnvDefs Map.empty Map.empty Map.empty) [sortIdString] createAssertions check
    where
        createAssertions :: [VarId] -> [ValExpr VarId]
        createAssertions [v] = [cstrEqual (cstrConst (Cint (Prelude.toInteger n))) (cstrLength (cstrVar v))]
        createAssertions _   = error "One variable in problem"

        check :: [Constant] -> SMT()
        check [value] = case value of
                            Cstring s   -> lift $ assertBool "expected pattern" (n == T.length s)
                            _           -> lift $ assertBool "unexpected pattern" False
        check _         = error "One variable in problem"


testRegex :: String -> SMT ()
testRegex regexStr = testTemplateValue (EnvDefs Map.empty Map.empty Map.empty) [sortIdString] createAssertions check
    where
        createAssertions :: [VarId] -> [ValExpr VarId]
        createAssertions [v] = [cstrStrInRe (cstrVar v) (cstrConst (Cregex (T.pack regexStr)))]
        createAssertions _   = error "One variable in problem"

        check :: [Constant] -> SMT()
        check [value] = case value of
                            Cstring s   -> let haskellRegex = xsd2posix . T.pack $ regexStr in
                                                lift $ assertBool ("expected pattern: smt solution " ++ T.unpack s ++ "\nXSD pattern " ++ regexStr ++ "\nHaskell pattern " ++ T.unpack haskellRegex)
                                                                  (T.unpack s =~ T.unpack haskellRegex)
                            _                  -> lift $ assertBool "unexpected pattern" False
        check _         = error "One variable in problem"
-}