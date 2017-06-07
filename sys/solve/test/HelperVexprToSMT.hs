{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

module HelperVexprToSMT

where
import qualified Data.Set as Set
import Data.String.Utils

import TxsDefs
import StdTDefs

import VarId
import FuncId

import HelperToSMT

data  TXS2SMTVExprTest         =  TXS2SMTVExprTest  { input    :: VExpr
                                                    , expected :: String
                                                    }
     deriving (Eq,Ord,Read,Show)
---------------------------------------------------------------------------
-- Vexpr constructors
---------------------------------------------------------------------------
toSMTVar :: VarId -> String
toSMTVar v = VarId.name v ++ "$$" ++ show (VarId.unid v)

createIsConstructor :: FuncId -> [TXS2SMTVExprTest] -> TXS2SMTVExprTest
createIsConstructor funcId ies =
    TXS2SMTVExprTest (cstrFunc funcId (map input ies))
                     ("(is-" ++ drop 2 (FuncId.name funcId) ++ " " ++ join " " (map expected ies) ++ ")")

createVfunc :: FuncId -> [TXS2SMTVExprTest] -> TXS2SMTVExprTest
createVfunc funcId ies =
    TXS2SMTVExprTest (cstrFunc funcId (map input ies))
               ("(" ++ FuncId.name funcId ++ " " ++ join " " (map expected ies) ++ ")")

createVconst :: Const -> TXS2SMTVExprTest
createVconst c@(Cint n) = 
    if n < 0 
        then TXS2SMTVExprTest (cstrConst c) ("(- " ++ show (abs n) ++ ")")
        else TXS2SMTVExprTest (cstrConst c) (show n)
createVconst c@(Cstring str) = 
    TXS2SMTVExprTest (cstrConst c) ("\"" ++ escape str ++ "\"")  
createVconst _               = error "Not supported / Not possible"

createVvar :: VarId -> TXS2SMTVExprTest
createVvar v =
    TXS2SMTVExprTest (cstrVar v) (toSMTVar v)

createVite :: Set.Set TXS2SMTVExprTest -> TXS2SMTVExprTest -> TXS2SMTVExprTest -> TXS2SMTVExprTest 
createVite conds (TXS2SMTVExprTest input1 expected1) (TXS2SMTVExprTest input2 expected2) =
    let listConds = Set.toList (Set.map expected conds) in
    if length listConds == 1
    then
        TXS2SMTVExprTest (cstrIte (Set.toList (Set.map input conds)) input1 input2)
               ("(ite " ++ head listConds ++ " " ++ expected1 ++ " " ++ expected2 ++ ")")
    else
        TXS2SMTVExprTest (cstrIte (Set.toList (Set.map input conds)) input1 input2)
               ("(ite (and " ++ join " " listConds ++ ") " ++ expected1 ++ " " ++ expected2 ++ ")")

createVequal :: TXS2SMTVExprTest -> TXS2SMTVExprTest -> TXS2SMTVExprTest
createVequal (TXS2SMTVExprTest input1 expected1) (TXS2SMTVExprTest input2 expected2) =
    TXS2SMTVExprTest (cstrEqual input1 input2) ("(= " ++ expected1 ++ " " ++ expected2 ++ ")" )     -- TODO: TXS can change order -- order should not be fixed ?? use regex??
    
createVpredef :: PredefKind -> FuncId -> [TXS2SMTVExprTest] -> TXS2SMTVExprTest
createVpredef predefkind funcId ies =
    TXS2SMTVExprTest (cstrPredef predefkind funcId (map input ies))
               ("(" ++ FuncId.name funcId ++ " " ++ join " " (map expected ies) ++ ")")
               
----------------------------------------------------------------
-- functions
------------------------------------------------------------------
createUniminusInt :: TXS2SMTVExprTest -> TXS2SMTVExprTest
createUniminusInt ie = 
    createVpredef SSI funcId_uniminusInt [ie]