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
import qualified Data.Char
import qualified Data.HashMap
import qualified Data.Text
--import           Text.Regex.TDFA

-- test specific Haskell imports
import           Test.HUnit

import           TorXakis.ContextVar
import           TorXakis.FuncDef
import           TorXakis.FunctionName
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.ProblemSolver
import           TorXakis.RandomSolver
import qualified TorXakis.Regex
import           TorXakis.SmtM
import           TorXakis.Sort
import           TorXakis.SymbolicSolver
import           TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.Var

import           TestSolvers

testConstraintList :: Test
testConstraintList =
    TestList $ concatMap (\s -> Prelude.map (\e -> TestLabel (show s ++ ": " ++ show (fst e))
                                                             $ TestCase $ do
                                                                            es <- uncurry mkSmtState s False
                                                                            case es of
                                                                                Left err -> error (show err)
                                                                                Right ss -> do
                                                                                            r <- runExceptT $ -- smt Solver
                                                                                                              execStateT (TorXakis.SmtM.toStateT
                                                                                                                                                 -- random Solver
                                                                                                                                                 (execStateT (TorXakis.RandomSolver.toStateT 
                                                                                                                                                                    -- symbolic solver
                                                                                                                                                                    (execStateT (TorXakis.SymbolicSolver.toStateT 
                                                                                                                                                                                    (snd e)
                                                                                                                                                                                )
                                                                                                                                                                                mkSymbolicState
                                                                                                                                                                    )
                                                                                                                                                             )
                                                                                                                                                             (mkRandomState 10 10 Factor)
                                                                                                                                                 )
                                                                                                                         )
                                                                                                                         ss
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


labelTestList :: ProblemSolver p => [(String, p ())]
labelTestList =
        [
            ("None",                                        testNone),
            ("True",                                        testTrue),
            ("False",                                       testFalse),
            ("Negative of Negative Is Identity",            testNegativeNegativeIsIdentity),
            ("Add Equal sum = e1 + e2",                     testAdd),
            ("No Variables",                                testNoVariables),
            ("Bool",                                        testBool),
            ("Bool False",                                  testBoolFalse),
            ("Bool True",                                   testBoolTrue),
            ("Int",                                         testInt),
            ("Int Negative",                                testIntNegative),
            ("Conditional Int Datatype",                    testConditionalInt),
            ("Conditional Int Value",                       testConditionalIntValue),
            ("Conditional Int IsAbsent",                    testConditionalIntIsAbsent),
            ("Conditional Int IsPresent",                   testConditionalIntIsPresent),
            ("Conditional Int Present Value",               testConditionalIntPresentValue),
            ("Conditional Int Instances",                   testConditionalIntInstances),
            ("Nested Constructor",                          testNestedConstructor),
            ("Function Const",                              testFunctionConst),
            ("Function Equal",                              testFunctionEqual),
            ("Recursive length",                            testRecursiveLength),
            ("Recursive sum",                               testRecursiveSum),
            ("Just String",                                 testString)
        ]
    ++
       ioeTestStringEquals
    ++
        ioeTestStringLength
    ++
        ioeTestRegex

ioeTestStringEquals :: ProblemSolver p => [(String, p ())]
ioeTestStringEquals = [
        ("String Equals Empty",                     testStringEquals ""),
        ("String Equals Alphabet",                  testStringEquals "abcdefghijklmnopqrstuvwxyz"),
        ("String Equals SpecialChars",              testStringEquals "\a\t\n\r\0\x5E\126\127\128\xFF")
    ]
    ++ testStringEqualsChar

testStringEqualsChar :: ProblemSolver p => [(String, p ())]
testStringEqualsChar =
    map (\i -> ("char = " ++ [Data.Char.chr i], testStringEquals (Data.Text.pack [Data.Char.chr i])))  [0..255]

ioeTestStringLength :: ProblemSolver p => [(String, p ())]
ioeTestStringLength = [
        ("String Length    0",                     testStringLength    0),
        ("String Length    1",                     testStringLength    1),
        ("String Length    2",                     testStringLength    2),
        ("String Length    5",                     testStringLength    5),
        ("String Length   10",                     testStringLength   10),
        ("String Length   20",                     testStringLength   20)
    ]

ioeTestRegex :: ProblemSolver p => [(String, p ())]
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
        ("Regex empty in choice",           testRegex "|a"),
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

-----------------------------------------------------
-- Helper function
-----------------------------------------------------
testTemplateSolvable :: ProblemSolver p => [ValExpression] -> Bool -> p ()
testTemplateSolvable createAssertions b = do
    addAssertions createAssertions
    resp <- solvable
    liftIO $ assertEqual "solvable problem" (SolvableProblem (Just b)) resp

testTemplateSolution :: ProblemSolver p => [ADTDef] -> [FuncDef] -> [VarDef] -> [ValExpression] -> p ()
testTemplateSolution as fs vs es = do
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

testNone :: ProblemSolver p => p ()
testNone = testTemplateSolvable [] True

testTrue :: ProblemSolver p => p ()
testTrue =
    let ctx             = TorXakis.ContextVar.empty
        Right x         = mkConst ctx (Cbool True)
    in
        testTemplateSolvable [x] True

testFalse :: ProblemSolver p => p ()
testFalse =
    let ctx             = TorXakis.ContextVar.empty
        Right x         = mkConst ctx (Cbool False)
    in
        testTemplateSolvable [x] False

testNegativeNegativeIsIdentity :: ProblemSolver p => p ()
testNegativeNegativeIsIdentity =
    let ctx             = TorXakis.ContextVar.empty
        Right x         = mkConst ctx (Cint 3)
        Right minX      = mkUnaryMinus ctx x
        Right minMinX   = mkUnaryMinus ctx minX
        Right equal     = mkEqual ctx x minMinX
     in
        testTemplateSolvable [equal] True

testAdd :: ProblemSolver p => p ()
testAdd =
    let ctx          = TorXakis.ContextVar.empty
        Right a      = mkConst ctx (Cint 4)
        Right b      = mkConst ctx (Cint 9)
        Right add    = mkSum ctx [a,b]
        Right answer = mkConst ctx (Cint 13)
        Right equal  = mkEqual ctx answer add
      in
        testTemplateSolvable [equal] True

testNoVariables :: ProblemSolver p => p ()
testNoVariables = testTemplateSolution [] [] [] []

testBool :: ProblemSolver p => p ()
testBool =
    let (_, [(boolVar, _)]) = makeVars TorXakis.ContextVar.empty [SortBool]
      in
        testTemplateSolution [] [] [boolVar] []

testBoolTrue :: ProblemSolver p => p ()
testBoolTrue =
    let (_, [(boolVar, boolExpr)]) = makeVars TorXakis.ContextVar.empty [SortBool]
      in
        testTemplateSolution [] [] [boolVar] [boolExpr]

testBoolFalse :: ProblemSolver p => p ()
testBoolFalse =
    let (ctx, [(boolVar, boolExpr)]) = makeVars TorXakis.ContextVar.empty [SortBool]
        Right notExpr = mkNot ctx boolExpr
      in
        testTemplateSolution [] [] [boolVar] [notExpr]

testInt :: ProblemSolver p => p ()
testInt =
    let (_, [(intVar, _)]) = makeVars TorXakis.ContextVar.empty [SortInt]
      in
        testTemplateSolution [] [] [intVar] []

testIntNegative :: ProblemSolver p => p ()
testIntNegative =
    let (ctx, [(intVar, intExpr)]) = makeVars TorXakis.ContextVar.empty [SortInt]
        Right constExpr = mkConst ctx (Cint 0)
        Right boolExpr = mkLT ctx intExpr constExpr
      in
        testTemplateSolution [] [] [intVar] [boolExpr]


testConditionalInt :: ProblemSolver p => p ()
testConditionalInt =
    let Right ctx = TorXakis.ContextVar.addADTs [conditionalInt] TorXakis.ContextVar.empty
        (_, [(cIntVar, _)]) = makeVars ctx [conditionalIntSort]
      in
        testTemplateSolution [conditionalInt] [] [cIntVar] []

testConditionalIntValue :: ProblemSolver p => p ()
testConditionalIntValue =
    let Right ctx = TorXakis.ContextVar.addADTs [conditionalInt] TorXakis.ContextVar.empty
        (ctx', [(cIntVar, cIntExpr)]) = makeVars ctx [conditionalIntSort]
        Right constExpr = mkConst ctx' (Cint 4)
        Right cstrExpr = mkCstr ctx' conditionalIntRef presentCstrRef [constExpr]
        Right boolExpr = mkEqual ctx' cIntExpr cstrExpr
      in
        testTemplateSolution [conditionalInt] [] [cIntVar] [boolExpr]

testConditionalIntIsAbsent :: ProblemSolver p => p ()
testConditionalIntIsAbsent =
    let Right ctx = TorXakis.ContextVar.addADTs [conditionalInt] TorXakis.ContextVar.empty
        (ctx', [(cIntVar, cIntExpr)]) = makeVars ctx [conditionalIntSort]
        boolExpr = isCstrAbsent ctx' cIntExpr
      in
        testTemplateSolution [conditionalInt] [] [cIntVar] [boolExpr]

testConditionalIntIsPresent :: ProblemSolver p => p ()
testConditionalIntIsPresent =
    let Right ctx = TorXakis.ContextVar.addADTs [conditionalInt] TorXakis.ContextVar.empty
        (ctx', [(cIntVar, cIntExpr)]) = makeVars ctx [conditionalIntSort]
        boolExpr = isCstrPresent ctx' cIntExpr
      in
        testTemplateSolution [conditionalInt] [] [cIntVar] [boolExpr]

testConditionalIntPresentValue :: ProblemSolver p => p ()
testConditionalIntPresentValue =
    let Right ctx = TorXakis.ContextVar.addADTs [conditionalInt] TorXakis.ContextVar.empty
        (ctx', [(cIntVar, cIntExpr)]) = makeVars ctx [conditionalIntSort]
        condExpr = isCstrPresent ctx' cIntExpr
        acessExpr = accessValue ctx' cIntExpr
        Right constExpr = mkConst ctx' (Cint 4)
        Right trueBranch = mkGT ctx' acessExpr constExpr
        Right falseBranch = mkConst ctx' (Cbool False)
        Right boolExpr = mkITE ctx' condExpr trueBranch falseBranch
      in
        testTemplateSolution [conditionalInt] [] [cIntVar] [boolExpr]

testConditionalIntInstances :: ProblemSolver p => p ()
testConditionalIntInstances =
    let Right ctx = TorXakis.ContextVar.addADTs [conditionalInt] TorXakis.ContextVar.empty
        (ctx', [ (cIntVar1, cIntExpr1)
               , (cIntVar2, cIntExpr2)
               , (cIntVar3, cIntExpr3)
               ]) = makeVars ctx [ conditionalIntSort
                                 , conditionalIntSort
                                 , conditionalIntSort
                                 ]
        Right equal12 = mkEqual ctx' cIntExpr1 cIntExpr2
        Right equal13 = mkEqual ctx' cIntExpr1 cIntExpr3
        Right equal23 = mkEqual ctx' cIntExpr2 cIntExpr3

        Right not12 = mkNot ctx' equal12
        Right not13 = mkNot ctx' equal13
        Right not23 = mkNot ctx' equal23
      in
        testTemplateSolution [conditionalInt] [] [cIntVar1, cIntVar2, cIntVar3] [not12, not13, not23]


testNestedConstructor :: ProblemSolver p => p ()
testNestedConstructor =
    let Right ctx = TorXakis.ContextVar.addADTs [pair, conditionalPair] TorXakis.ContextVar.empty
        (ctx', [ (var1, expr1)
               , (var2, expr2)
               , (var3, expr3)
               ]) = makeVars ctx [ conditionalPairSort
                                 , conditionalPairSort
                                 , conditionalPairSort
                                 ]
        Right equal12 = mkEqual ctx' expr1 expr2
        Right equal13 = mkEqual ctx' expr1 expr3
        Right equal23 = mkEqual ctx' expr2 expr3

        Right not12 = mkNot ctx' equal12
        Right not13 = mkNot ctx' equal13
        Right not23 = mkNot ctx' equal23
      in
        testTemplateSolution [pair, conditionalPair] [] [var1, var2, var3] [not12, not13, not23]


testFunctionConst :: ProblemSolver p => p ()
testFunctionConst =
        let (ctx, [ (iVar, iExpr)
                  ]) = makeVars TorXakis.ContextVar.empty [SortInt]
            Right fcall = mkFunc ctx functionConstRef []
            Right equal = mkEqual ctx fcall iExpr
         in
            testTemplateSolution [] [functionConst] [iVar] [equal]

testFunctionEqual :: ProblemSolver p => p ()
testFunctionEqual =
        let (ctx, [ (bVar1, bExpr1)
                  , (bVar2, bExpr2)
                  ]) = makeVars TorXakis.ContextVar.empty [SortBool, SortBool]
            Right fcall = mkFunc ctx functionEqualRef [bExpr1, bExpr2]
         in
            testTemplateSolution [] [functionEqual] [bVar1, bVar2] [fcall]

testRecursiveLength :: ProblemSolver p => p ()
testRecursiveLength =
        let Right ctx = TorXakis.ContextVar.addADTs [listInt] TorXakis.ContextVar.empty
            (ctx', [(lVar, lExpr)]) = makeVars ctx [listIntSort]
            Right fcall = mkFunc ctx' functionLengthRef [lExpr]
            Right one = mkConst ctx' (Cint 1)
            Right bExpr = mkGE ctx' fcall one
         in
            testTemplateSolution [listInt] [functionLength] [lVar] [bExpr]

testRecursiveSum :: ProblemSolver p => p ()
testRecursiveSum =
        let Right ctx = TorXakis.ContextVar.addADTs [listInt] TorXakis.ContextVar.empty
            (ctx', [(lVar, lExpr)]) = makeVars ctx [listIntSort]
            Right fcall = mkFunc ctx' functionSumRef [lExpr]
            Right twelve = mkConst ctx' (Cint 12)
            Right bExpr = mkGT ctx' fcall twelve
         in
            testTemplateSolution [listInt] [functionSum] [lVar] [bExpr]

testString :: ProblemSolver p => p ()
testString =
    let (_, [(stringVar, _)]) = makeVars TorXakis.ContextVar.empty [SortString]
      in
        testTemplateSolution [] [] [stringVar] []

testStringEquals :: ProblemSolver p => Text -> p ()
testStringEquals txt =
    let (ctx, [(stringVar, stringExpr)]) = makeVars TorXakis.ContextVar.empty [SortString]
        Right val = mkConst ctx (Cstring txt)
        Right equal = mkEqual ctx stringExpr val
      in
        testTemplateSolution [] [] [stringVar] [equal]

testStringLength :: ProblemSolver p => Integer -> p ()
testStringLength n =
    let (ctx, [(stringVar, stringExpr)]) = makeVars TorXakis.ContextVar.empty [SortString]
        Right val = mkConst ctx (Cint n)
        Right lengthExpr = mkLength ctx stringExpr
        Right equal = mkEqual ctx lengthExpr val
      in
        testTemplateSolution [] [] [stringVar] [equal]

testRegex :: ProblemSolver p => Text -> p ()
testRegex regexStr =
    let (ctx, [(stringVar, stringExpr)]) = makeVars TorXakis.ContextVar.empty [SortString]
        Right regex = TorXakis.Regex.fromXsd regexStr
        Right val = mkConst ctx (Cregex regex)
        Right strinre = mkStrInRe ctx stringExpr val
      in
        testTemplateSolution [] [] [stringVar] [strinre]

-- --------------------------------------------------------------------------------------------------------------------
-- make Var and Expressions
-- --------------------------------------------------------------------------------------------------------------------
makeVars :: VarContext c => c -> [Sort] -> (c, [(VarDef, ValExpression)])
makeVars ctx ss = foldl makeVar (ctx, []) (zip ss [1..])
    where
        makeVar :: VarContext c => (c, [(VarDef, ValExpression)]) -> (Sort, Integer) -> (c, [(VarDef, ValExpression)])
        makeVar (ctxIn, vs) (s,i) =
            let Right nm = mkName (Data.Text.append "var" (Data.Text.pack (show i)))
                ref :: RefByName VarDef
                ref = RefByName nm
                Right varDecl = mkVarDef ctxIn nm s
                Right ctxOut = addVars [varDecl] ctxIn
                Right varExpr = mkVar ctxOut ref
              in
                ( ctxOut, (varDecl, varExpr) : vs )

--------------------
-- Conditional Int
--------------------
absentName :: Name
absentName = case mkName "absent" of
                Right nm -> nm
                Left e -> error ("absentName failed with " ++ show e)

absentCstrRef :: RefByName ConstructorDef
absentCstrRef = RefByName absentName

absentCstr :: ConstructorDef
absentCstr = case mkConstructorDef absentName [] of
                Right cstr -> cstr
                Left e -> error ("absentCstr failed with " ++ show e)

isCstrAbsent :: VarContext c => c -> ValExpression -> ValExpression
isCstrAbsent ctx e = case mkIsCstr ctx conditionalIntRef absentCstrRef e of
                            Right isCstr -> isCstr
                            Left err -> error ("isCstrAbsent failed with " ++ show err)

valueName :: Name
valueName = case mkName "value" of
                Right nm -> nm
                Left e -> error ("valueName failed with " ++ show e)

presentName :: Name
presentName = case mkName "present" of
                Right nm -> nm
                Left e -> error ("presentName failed with " ++ show e)

presentCstrRef :: RefByName ConstructorDef
presentCstrRef = RefByName presentName

presentCstr :: Sort -> ConstructorDef
presentCstr srt = case mkConstructorDef presentName [FieldDef valueName srt] of
                Right cstr -> cstr
                Left e -> error ("presentCstr failed with " ++ show e)

isCstrPresent :: VarContext c => c -> ValExpression -> ValExpression
isCstrPresent ctx e = case mkIsCstr ctx conditionalIntRef presentCstrRef e of
                            Right isCstr -> isCstr
                            Left err -> error ("isCstrPresent failed with " ++ show err)

accessValue :: VarContext c => c -> ValExpression -> ValExpression
accessValue ctx e = case mkAccess ctx conditionalIntRef presentCstrRef (RefByName valueName) e of
                            Right access -> access
                            Left err -> error ("accessValue failed with " ++ show err)

conditionalIntName :: Name
conditionalIntName = case mkName "conditionalInt" of
                Right nm -> nm
                Left e -> error ("conditionalIntName failed with " ++ show e)

conditionalIntRef :: RefByName ADTDef
conditionalIntRef = RefByName conditionalIntName

conditionalIntSort :: Sort
conditionalIntSort = SortADT conditionalIntRef

conditionalInt :: ADTDef
conditionalInt = conditional conditionalIntName SortInt

conditional :: Name -> Sort -> ADTDef
conditional nm s = case mkADTDef nm [absentCstr, presentCstr s] of
                        Right adt -> adt
                        Left e -> error ("conditional failed with " ++ show e)

-- --------------------
-- Pair
-- --------------------
pairName :: Name
pairName = case mkName "pair" of
                Right nm -> nm
                Left e -> error ("pairName failed with " ++ show e)

xName :: Name
xName = case mkName "x" of
                Right nm -> nm
                Left e -> error ("xName failed with " ++ show e)

yName :: Name
yName = case mkName "y" of
                Right nm -> nm
                Left e -> error ("yName failed with " ++ show e)

pairCstr :: ConstructorDef
pairCstr = case mkConstructorDef pairName [FieldDef xName SortInt, FieldDef yName SortInt] of
                Right cstr -> cstr
                Left e -> error ("pairCstr failed with " ++ show e)

pair :: ADTDef
pair = case mkADTDef pairName [pairCstr] of
                    Right adt -> adt
                    Left e -> error ("pair failed with " ++ show e)

pairRef :: RefByName ADTDef
pairRef = RefByName pairName

pairSort :: Sort
pairSort = SortADT pairRef

conditionalPairName :: Name
conditionalPairName = case mkName "CONDITIONAL_PAIR" of
                Right nm -> nm
                Left e -> error ("conditionalPairName failed with " ++ show e)

conditionalPair :: ADTDef
conditionalPair = conditional conditionalPairName pairSort

conditionalPairRef :: RefByName ADTDef
conditionalPairRef = RefByName conditionalPairName

conditionalPairSort :: Sort
conditionalPairSort = SortADT conditionalPairRef

-- ------------------------------------
constName :: FunctionName
constName = case mkFunctionName "const" of
                Right nm -> nm
                Left e -> error ("constName failed with " ++ show e)

functionConst :: FuncDef
functionConst =
    let varContext = TorXakis.ContextVar.empty
        Right ps = mkVarsDecl varContext []
        Right bd = mkConst varContext (Cint (-12))
      in
        case mkFuncDef varContext constName ps bd of
                Right fd -> fd
                Left e -> error ("functionConst failed with " ++ show e)

functionConstRef :: RefByFuncSignature
functionConstRef = RefByFuncSignature (getFuncSignature TorXakis.ContextVar.empty functionConst)

equalName :: FunctionName
equalName = case mkFunctionName "equal" of
                Right nm -> nm
                Left e -> error ("equalName failed with " ++ show e)

functionEqual :: FuncDef
functionEqual =
    let varContext = TorXakis.ContextVar.empty
        (varContext', [ (var1, expr1)
                      , (var2, expr2)
                      ]) = makeVars varContext [SortBool, SortBool]
        Right ps = mkVarsDecl varContext' [var1,var2]
        Right bd = mkEqual varContext' expr1 expr2
      in
        case mkFuncDef varContext equalName ps bd of
                Right fd -> fd
                Left e -> error ("functionEqual failed with " ++ show e)

functionEqualRef :: RefByFuncSignature
functionEqualRef = RefByFuncSignature (getFuncSignature TorXakis.ContextVar.empty functionEqual)

-- ---------------------------------------------------------
--------------------
-- List Int
--------------------
nilName :: Name
nilName = case mkName "nil" of
                Right nm -> nm
                Left e -> error ("nilName failed with " ++ show e)

nilCstrRef :: RefByName ConstructorDef
nilCstrRef = RefByName nilName

nilCstr :: ConstructorDef
nilCstr = case mkConstructorDef nilName [] of
                Right cstr -> cstr
                Left e -> error ("nilCstr failed with " ++ show e)

isCstrNil :: VarContext c => c -> ValExpression -> ValExpression
isCstrNil ctx e = case mkIsCstr ctx listIntRef nilCstrRef e of
                            Right isCstr -> isCstr
                            Left err -> error ("isCstrNil failed with " ++ show err)

headName :: Name
headName = case mkName "head" of
                Right nm -> nm
                Left e -> error ("headName failed with " ++ show e)

tailName :: Name
tailName = case mkName "tail" of
                Right nm -> nm
                Left e -> error ("tailName failed with " ++ show e)

consName :: Name
consName = case mkName "cons" of
                Right nm -> nm
                Left e -> error ("consName failed with " ++ show e)

consCstrRef :: RefByName ConstructorDef
consCstrRef = RefByName consName

consCstr :: Sort -> Sort -> ConstructorDef
consCstr srt lsrt = case mkConstructorDef consName [FieldDef headName srt, FieldDef tailName lsrt] of
                            Right cstr -> cstr
                            Left e -> error ("consCstr failed with " ++ show e)

isCstrCons :: VarContext c => c -> ValExpression -> ValExpression
isCstrCons ctx e = case mkIsCstr ctx listIntRef consCstrRef e of
                            Right isCstr -> isCstr
                            Left err -> error ("isCstrCons failed with " ++ show err)

accessHead :: VarContext c => c -> ValExpression -> ValExpression
accessHead ctx e = case mkAccess ctx listIntRef consCstrRef (RefByName headName) e of
                            Right access -> access
                            Left err -> error ("accessHead failed with " ++ show err)

accessTail :: VarContext c => c -> ValExpression -> ValExpression
accessTail ctx e = case mkAccess ctx listIntRef consCstrRef (RefByName tailName) e of
                            Right access -> access
                            Left err -> error ("accessTail failed with " ++ show err)

listIntName :: Name
listIntName = case mkName "listInt" of
                Right nm -> nm
                Left e -> error ("listIntName failed with " ++ show e)

listIntRef :: RefByName ADTDef
listIntRef = RefByName listIntName

listIntSort :: Sort
listIntSort = SortADT listIntRef

listInt :: ADTDef
listInt = list listIntName SortInt

list :: Name -> Sort -> ADTDef
list nm srt = let lsrt = SortADT (RefByName nm) in
                    case mkADTDef nm [nilCstr, consCstr srt lsrt] of
                            Right adt -> adt
                            Left e -> error ("list failed with " ++ show e)

lengthName :: FunctionName
lengthName = case mkFunctionName "length" of
                Right nm -> nm
                Left e -> error ("lengthName failed with " ++ show e)

functionLength :: FuncDef
functionLength =
    let Right ctx = TorXakis.ContextVar.addADTs [ listInt ] TorXakis.ContextVar.empty
        (ctx', [ (var, expr) ]) = makeVars ctx [ listIntSort ]
        Right ps = mkVarsDecl ctx' [var]
        cond = isCstrNil ctx' expr
        Right one = mkConst ctx' (Cint 1)
        access = accessTail ctx' expr
        Right sign = mkFuncSignature ctx' lengthName [ listIntSort ] SortInt
        Right fc = mkFunc ctx' (RefByFuncSignature sign) [access]
        Right plus = mkPlus ctx' one fc
        Right bd = mkITE ctx' cond one plus
      in
        case mkFuncDef ctx' lengthName ps bd of
                Right fd -> fd
                Left e -> error ("functionLength failed with " ++ show e)

functionLengthRef :: RefByFuncSignature
functionLengthRef =
    let Right ctx = TorXakis.ContextVar.addADTs [ listInt ] TorXakis.ContextVar.empty in
            RefByFuncSignature (getFuncSignature ctx functionLength)

sumName :: FunctionName
sumName = case mkFunctionName "sum" of
                Right nm -> nm
                Left e -> error ("sumName failed with " ++ show e)

functionSum :: FuncDef
functionSum =
    let Right ctx = TorXakis.ContextVar.addADTs [ listInt ] TorXakis.ContextVar.empty
        (ctx', [ (var, expr) ]) = makeVars ctx [ listIntSort ]
        Right ps = mkVarsDecl ctx' [var]
        cond = isCstrCons ctx' expr
        Right zero = mkConst ctx' (Cint 0)
        tailAccess = accessTail ctx' expr
        headAccess = accessHead ctx' expr
        Right sign = mkFuncSignature ctx' sumName [ listIntSort ] SortInt
        Right fc = mkFunc ctx' (RefByFuncSignature sign) [tailAccess]
        Right plus = mkPlus ctx' headAccess fc
        Right bd = mkITE ctx' cond plus zero
      in
        case mkFuncDef ctx' sumName ps bd of
                Right fd -> fd
                Left e -> error ("functionSum failed with " ++ show e)

functionSumRef :: RefByFuncSignature
functionSumRef =
    let Right ctx = TorXakis.ContextVar.addADTs [ listInt ] TorXakis.ContextVar.empty in
            RefByFuncSignature (getFuncSignature ctx functionSum)