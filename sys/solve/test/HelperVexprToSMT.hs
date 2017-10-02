{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module HelperVexprToSMT

where
import qualified Data.Set          as Set
import           Data.String.Utils
import           Data.Text         (Text)
import qualified Data.Text         as T

import           FreeMonoidX
import           StdTDefs
import           Sum
import           TxsDefs

import           FuncId
import           VarId

import           HelperToSMT

data  TXS2SMTVExprTest         =  TXS2SMTVExprTest  { input    :: VExpr
                                                    , expected :: String
                                                    }
     deriving (Eq,Ord,Read,Show)
---------------------------------------------------------------------------
-- Vexpr constructors
---------------------------------------------------------------------------
toSMTVar :: VarId -> String
toSMTVar v = T.unpack (VarId.name v) ++ "$$" ++ show (VarId.unid v)

createIsConstructor :: FuncId -> [TXS2SMTVExprTest] -> TXS2SMTVExprTest
createIsConstructor funcId ies =
    TXS2SMTVExprTest (cstrFunc funcId (map input ies))
                     ("(is-" ++ drop 2 (T.unpack (FuncId.name funcId)) ++ " " ++ join " " (map expected ies) ++ ")")

createVfunc :: FuncId -> [TXS2SMTVExprTest] -> TXS2SMTVExprTest
createVfunc funcId ies =
   TXS2SMTVExprTest (cstrFunc funcId (map input ies))
              ("(" ++ T.unpack (FuncId.name funcId) ++ " " ++ join " " (map expected ies) ++ ")")

createVsum :: [TXS2SMTVExprTest] -> TXS2SMTVExprTest
createVsum ies =
    TXS2SMTVExprTest (cstrSum (fromListT (map input ies)))
               ("(+ " ++ join " " (map expected ies) ++ ")")

createVconst :: Const -> TXS2SMTVExprTest
createVconst c@(Cint n) =
    if n < 0
        then TXS2SMTVExprTest (cstrConst c) ("(- " ++ show (abs n) ++ ")")
        else TXS2SMTVExprTest (cstrConst c) (show n)
createVconst c@(Cstring str) =
    TXS2SMTVExprTest (cstrConst c) ("\"" ++ HelperToSMT.escape (T.unpack str) ++ "\"")
createVconst _               = error "Not supported / Not possible"

createVvar :: VarId -> TXS2SMTVExprTest
createVvar v =
    TXS2SMTVExprTest (cstrVar v) (toSMTVar v)

createVite :: TXS2SMTVExprTest -> TXS2SMTVExprTest -> TXS2SMTVExprTest -> TXS2SMTVExprTest
createVite (TXS2SMTVExprTest inputc expectedc) (TXS2SMTVExprTest input1 expected1) (TXS2SMTVExprTest input2 expected2) =
    TXS2SMTVExprTest (cstrITE inputc input1 input2)
           ("(ite " ++ expectedc ++ " " ++ expected1 ++ " " ++ expected2 ++ ")")

createVequal :: TXS2SMTVExprTest -> TXS2SMTVExprTest -> TXS2SMTVExprTest
createVequal (TXS2SMTVExprTest input1 expected1) (TXS2SMTVExprTest input2 expected2) =
    TXS2SMTVExprTest (cstrEqual input1 input2) ("(= " ++ expected1 ++ " " ++ expected2 ++ ")" )     -- TODO: TXS can change order -- order should not be fixed ?? use regex??

createVand :: Set.Set TXS2SMTVExprTest -> TXS2SMTVExprTest
createVand conds =
    TXS2SMTVExprTest (cstrAnd (Set.map input conds))
                     ("(and " ++ join " " (Set.toList (Set.map expected conds)) ++ ")")

createVgez :: TXS2SMTVExprTest -> TXS2SMTVExprTest
createVgez (TXS2SMTVExprTest input expected) =
    TXS2SMTVExprTest (cstrGEZ input ) ("(<= 0 " ++ expected ++ ")" )

----------------------------------------------------------------
-- functions
------------------------------------------------------------------
createUniminusInt :: TXS2SMTVExprTest -> TXS2SMTVExprTest
createUniminusInt ie =
    TXS2SMTVExprTest (cstrUnaryMinus (input ie))
               ("(- " ++ expected ie ++ ")")
