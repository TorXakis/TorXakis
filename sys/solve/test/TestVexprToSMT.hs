{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module TestVexprToSMT
(
testVexprToSMTList
)
where
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Test.HUnit

import           TxsDefs

import           Data.Char
import           HelperVexprToSMT
import           TXS2SMT

testVexprToSMTList :: Test
testVexprToSMTList = TestList [
        TestLabel "int" testVconstCint,
        TestLabel "string" testVconstCstring,
        TestLabel "string All Chars" (TestList (map testVconstCstringSingleChar [chr 0 .. chr 255])),
        TestLabel "Vvar" testVvar,
        TestLabel "ite singleton" testViteSingleton,
        TestLabel "ite" testVite,
        TestLabel "equal" testVequal,
        TestLabel "uni minus int" testUniminusInt
    ]

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
testVconstCint :: Test
testVconstCint = TestCase $ do
    let (TXS2SMTVExprTest i e) = createVconst (Cint 3)
    assertEqual "Vconst Cint" e (T.unpack (valexprToSMT Map.empty i))

testVconstCstring :: Test
testVconstCstring = TestCase $ do
    let (TXS2SMTVExprTest i e) = createVconst (Cstring "Aap")
    assertEqual "Vconst Cstring" e (T.unpack (valexprToSMT Map.empty i))

testVconstCstringSingleChar :: Char -> Test
testVconstCstringSingleChar c = TestCase $ do
    --Trace.trace ("char c = " ++ (show c)) $ do
    let (TXS2SMTVExprTest i e) = createVconst (Cstring (T.singleton c))
    assertEqual "Vconst Cstring Char" e (T.unpack (valexprToSMT Map.empty i))

testVvar :: Test
testVvar = TestCase $ do
    let sortId = SortId "Pierre" 67
    let varId = VarId "x" 1234 sortId
    let (TXS2SMTVExprTest i e) = createVvar varId
    assertEqual "Vvar" e (T.unpack (valexprToSMT Map.empty i))

testViteSingleton :: Test
testViteSingleton = TestCase $ do
    let thenExpr = createVconst (Cint 3)
    let elseExpr = createVconst (Cint 12)
    let sortId = SortId "Pierre" 67
    let varId = VarId "x" 1234 sortId
    let cond = createVequal (createVconst (Cint 13)) (createVvar varId)  -- TODO: order should not be relevant!
    let (TXS2SMTVExprTest i e) = createVite cond thenExpr elseExpr
    assertEqual "ite singleton" e (T.unpack (valexprToSMT Map.empty i))

testVite :: Test
testVite = TestCase $ do
    let thenExpr = createVconst (Cint 33)
    let elseExpr = createVconst (Cint 123)
    let sortId = SortId "Jan" 67
    let var1Id = VarId "v1" 1234 sortId
    let var2Id = VarId "v2" 1235 sortId
    let conds = createVand (Set.fromList [ createVequal (createVconst (Cint 56)) (createVvar var1Id)
                                         , createVequal (createVconst (Cint 5)) (createVvar var2Id)
                                         ]
                           )
    let (TXS2SMTVExprTest i e) = createVite conds thenExpr elseExpr
    assertEqual "ite" e (T.unpack (valexprToSMT Map.empty i))

testVequal :: Test
testVequal = TestCase $ do
    let ie1 = createVconst (Cint 7)
    let sortId = SortId "Pierre" 67
    let varId = VarId "x" 1234 sortId
    let ie2 = createVvar varId
    let (TXS2SMTVExprTest i e) = createVequal ie1 ie2
    assertEqual "equal" e (T.unpack (valexprToSMT Map.empty i))

testUniminusInt :: Test
testUniminusInt = TestCase $ do
    let ie = createVconst (Cint 3)
    let (TXS2SMTVExprTest i e) = createUniminusInt ie
    assertEqual "UniminusInt" e (T.unpack (valexprToSMT (Map.fromList initialMapInstanceTxsToSmtlib) i))  -- Need '-' function
