{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-incomplete-patterns #-}
module TestSMTValue
(
testSMTValueList
)
where
-- general Haskell imports
import qualified Data.Map          as Map
import           Data.Maybe
import           Data.String.Utils
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Test.HUnit

-- specific SMT imports
import           SMTAlex
import           SMTHappy

-- ----------------------------------------------------------------------------
testSMTValueList :: Test
testSMTValueList = TestList [
        TestCase $ assertEqual "DoubleQuote"  (Map.singleton "x" (SMTString "\""))   (smtParser (smtLexer "((x \"\"\"\"))")),
        TestCase $ assertEqual "DoubleEscape" (Map.singleton "x" (SMTString "\\"))   (smtParser (smtLexer "((x \"\\\\\"))")),
        TestCase $ assertEqual "EscapeChar"   (Map.singleton "x" (SMTString "\x02")) (smtParser (smtLexer "((x \"\\x02\"))")),
        TestLabel "Int (Positive)"                  testIntPositive,
        TestLabel "Int (Negative)"                  testIntNegative,
        TestLabel "String"                          testString,
        TestLabel "String Special Characters"       testStringSpecialCharacters,
        TestLabel "EscapeCharacters"                testEscapeCharactersList,
        TestLabel "Bool (True)"                     testBoolTrue,
        TestLabel "Bool (False)"                    testBoolFalse,
        TestLabel "Constructor Empty"               testConstructorEmpty,
        TestLabel "Constructor"                     testConstructor,
        TestLabel "Let Equals"                      testLetList,
        TestLabel "Let Equals Constructor On Var"   testLetConstructor,
        TestLabel "Let On Constructor"              testLetOnConstructor,
        TestLabel "LetNesting"                      testLetNesting,
        TestLabel "Let Pair 4"                      testLetPair4
    ]

data  TestValExpr       = TVEConstructor String [TestValExpr]
                        | TVEBool Bool
                        | TVEInt Integer
                        | TVEString String
                        | TVEVar String
     deriving (Eq,Ord,Read,Show)

data  SMTValueTest         =  SMTValueTest  { input    :: String
                                            , expected :: TestValExpr
                                            }
     deriving (Eq,Ord,Read,Show)

---------------------------------------------------------------------------
-- SMTValExpr functions
---------------------------------------------------------------------------
substitute :: Map.Map TestValExpr TestValExpr -> TestValExpr -> TestValExpr
substitute bind var@(TVEVar _) = fromMaybe var (Map.lookup var bind)
substitute bind (TVEConstructor c vals) = TVEConstructor c (map (substitute bind) vals)
substitute _ y = y

toSMTValue :: TestValExpr -> SMTValue
toSMTValue (TVEConstructor s vals)  = SMTConstructor (T.pack s) (map toSMTValue vals)
toSMTValue (TVEBool b)              = SMTBool b
toSMTValue (TVEInt i)               = SMTInt i
toSMTValue (TVEString s)            = SMTString (T.pack s)
toSMTValue (TVEVar s)               = error ("var " ++ s ++ " does not have a constant value")
---------------------------------------------------------------------------
-- SMTValue constructors
---------------------------------------------------------------------------

createSMTConstructor :: String -> [SMTValueTest] -> SMTValueTest
createSMTConstructor name [] = SMTValueTest name (TVEConstructor name [])
createSMTConstructor name neList = SMTValueTest ("(" ++ name ++ " " ++ Data.String.Utils.join " " (map input neList) ++ ")") (TVEConstructor name (map expected neList))

createSMTBool :: Bool -> SMTValueTest
createSMTBool True  = SMTValueTest "true" (TVEBool True)
createSMTBool False = SMTValueTest "false" (TVEBool False)

createSMTInt :: Integer -> SMTValueTest
createSMTInt i = SMTValueTest (show i) (TVEInt i)

createNegative :: SMTValueTest -> SMTValueTest
createNegative (SMTValueTest s (TVEInt i)) = SMTValueTest ("(- " ++ s ++ ")") (TVEInt (-1*i))

createSMTString :: String -> SMTValueTest
createSMTString s = SMTValueTest ("\"" ++ T.unpack (encodeStringLiteral (T.pack s)) ++ "\"") (TVEString s)

createSMTVar :: String -> SMTValueTest
createSMTVar s = SMTValueTest s (TVEVar s)

toLetPairs :: Map.Map SMTValueTest SMTValueTest -> String
toLetPairs bind = concatMap toLetPair (Map.toList bind)
    where toLetPair :: (SMTValueTest, SMTValueTest) -> String
          toLetPair (SMTValueTest inputX _, SMTValueTest inputY _) = "(" ++ inputX ++ " " ++ inputY ++ ")"

toLetBind :: Map.Map SMTValueTest SMTValueTest -> Map.Map TestValExpr TestValExpr
toLetBind bind = Map.fromList (map (\(SMTValueTest _ expectedX, SMTValueTest _ expectedY) -> (expectedX, expectedY) ) (Map.toList bind) )

createSMTLet :: Map.Map SMTValueTest SMTValueTest -> SMTValueTest -> SMTValueTest
createSMTLet bind (SMTValueTest input expected) =
    SMTValueTest ("(let ("++ toLetPairs bind ++") " ++ input ++ " )") (substitute (toLetBind bind) expected)

-- ---------------------------------------------------------------------------------
-- decode SMT String Literal
-- Handle escaped quote and escape
-- according to smt-lib-version 2.5 standard
-- ------------------------------------------------------------------
encodeStringLiteral :: Text -> Text
encodeStringLiteral string =
    T.replace "\\" "\\\\" (T.replace "\"" "\"\"" string)
---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
testSMTValueTest :: String -> SMTValueTest -> Test
testSMTValueTest s (SMTValueTest input expected) = TestCase $
    --Trace.trace (" SMTValue : " ++ input) $ do
    assertEqual s (Map.singleton "x" (toSMTValue expected)) (smtParser (smtLexer ("((x " ++ input ++ "))")))

testIntPositive :: Test
testIntPositive = testSMTValueTest "Int positive" (createSMTInt 3)

testIntNegative :: Test
testIntNegative = testSMTValueTest "Int negative" (createNegative int)
    where int = createSMTInt 3

testString :: Test
testString = testSMTValueTest "String" (createSMTString "jaap piet")

testStringSpecialCharacters :: Test
testStringSpecialCharacters = testSMTValueTest "String Special Characters" (createSMTString "jaap \\ piet \" klaas")

testEscapeCharactersList :: Test
testEscapeCharactersList = TestList [ -- abefnrtv
        TestCase $ assertEqual "EscapeChar a"   (Map.singleton "x" (SMTString "\a"))   (smtParser (smtLexer "((x \"\\a\"))")),
        TestCase $ assertEqual "EscapeChar b"   (Map.singleton "x" (SMTString "\b"))   (smtParser (smtLexer "((x \"\\b\"))")),
        TestCase $ assertEqual "EscapeChar e"   (Map.singleton "x" (SMTString "\x1B")) (smtParser (smtLexer "((x \"\\e\"))")),
        TestCase $ assertEqual "EscapeChar f"   (Map.singleton "x" (SMTString "\f"))   (smtParser (smtLexer "((x \"\\f\"))")),
        TestCase $ assertEqual "EscapeChar n"   (Map.singleton "x" (SMTString "\n"))   (smtParser (smtLexer "((x \"\\n\"))")),
        TestCase $ assertEqual "EscapeChar r"   (Map.singleton "x" (SMTString "\r"))   (smtParser (smtLexer "((x \"\\r\"))")),
        TestCase $ assertEqual "EscapeChar t"   (Map.singleton "x" (SMTString "\t"))   (smtParser (smtLexer "((x \"\\t\"))")),
        TestCase $ assertEqual "EscapeChar v"   (Map.singleton "x" (SMTString "\v"))   (smtParser (smtLexer "((x \"\\v\"))"))
    ]

testBoolTrue :: Test
testBoolTrue = testSMTValueTest "Bool true" (createSMTBool True)

testBoolFalse :: Test
testBoolFalse = testSMTValueTest "Bool false" (createSMTBool False)

testConstructorEmpty :: Test
testConstructorEmpty = testSMTValueTest "Constructor Empty" (createSMTConstructor "Aap" [])

testConstructor :: Test
testConstructor = testSMTValueTest "Constructor" (createSMTConstructor "Aap" [createSMTBool False, createSMTConstructor "Piet" [], createSMTInt 2])

testLetInt :: String -> Test
testLetInt name = testSMTValueTest "Let Int" (createSMTLet (Map.fromList[(var,val)]) var)
    where
        var = createSMTVar name
        val = createSMTInt 2

testLetList :: Test
testLetList = TestList [
    TestLabel "Let Int a"   (testLetInt "a"),
    TestLabel "Let Int A"   (testLetInt "A"),
    TestLabel "Let Int !"   (testLetInt "!"),
    TestLabel "Let Int aa"   (testLetInt "aa"),
    TestLabel "Let Int aA"   (testLetInt "aA"),
    TestLabel "Let Int a!"   (testLetInt "a!"),
    TestLabel "Let Int a1"   (testLetInt "a1"),
    TestLabel "Let Int Aa"   (testLetInt "Aa"),
    TestLabel "Let Int AA"   (testLetInt "AA"),
    TestLabel "Let Int A!"   (testLetInt "A!"),
    TestLabel "Let Int A1"   (testLetInt "A1"),
    TestLabel "Let Int !a"   (testLetInt "!a"),
    TestLabel "Let Int !A"   (testLetInt "!A"),
    TestLabel "Let Int !!"   (testLetInt "!!"),
    TestLabel "Let Int !1"   (testLetInt "!1")
    ]


testLetConstructor :: Test
testLetConstructor = testSMTValueTest "Let Constructor" (createSMTLet (Map.fromList[(var,val)]) var)
    where
        var = createSMTVar "a!"
        arg1 = createSMTBool False
        arg2 = createSMTConstructor "Piet" []
        arg3 = createSMTInt 2
        val = createSMTConstructor "Aap" [arg1,arg2,arg3]

testLetOnConstructor :: Test
testLetOnConstructor = testSMTValueTest "Let On Constructor" (createSMTLet (Map.fromList[(var,val)]) constructor)
    where
        var = createSMTVar "a!"
        val = createSMTInt 2
        constructor = createSMTConstructor "Aap" [var,var]

testLetNesting :: Test
testLetNesting = testSMTValueTest "Let Nesting" (createSMTLet (Map.fromList[(varA,valA),(varB,valB)]) let1)
    where
        varA = createSMTVar "a!"
        valA = createSMTInt 4
        varB = createSMTVar "b!"
        valB = createSMTInt 3
        constructor = createSMTConstructor "Aap" [varA,varB]
        let1 = createSMTLet (Map.fromList[(varA,varB),(varB,varA)]) constructor

testLetPair4 :: Test
testLetPair4 = testSMTValueTest "Let Pair 4" (createSMTLet (Map.fromList[(var,val)]) constructor4)
    where
        var = createSMTVar "a!1"
        constructor4 = createSMTConstructor "Pair4$Pair4" [var,var]
        constructor1a = createSMTConstructor "Pair1$Pair1" [createSMTInt 0,createSMTInt 1]
        constructor1b = createSMTConstructor "Pair1$Pair1" [createSMTInt 2,createSMTInt 3]
        constructor2a = createSMTConstructor "Pair2$Pair2" [constructor1a, constructor1b]
        constructor1c = createSMTConstructor "Pair1$Pair1" [createSMTInt 4,createSMTInt 5]
        constructor1d = createSMTConstructor "Pair1$Pair1" [createSMTInt 6,createSMTInt 7]
        constructor2b = createSMTConstructor "Pair2$Pair2" [constructor1c, constructor1d]
        val = createSMTConstructor "Pair3$Pair3" [constructor2a, constructor2b]
