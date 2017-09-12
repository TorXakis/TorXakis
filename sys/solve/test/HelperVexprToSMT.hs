{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
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

createVite :: TXS2SMTVExprTest -> TXS2SMTVExprTest -> TXS2SMTVExprTest -> TXS2SMTVExprTest 
createVite (TXS2SMTVExprTest inputc expectedc) (TXS2SMTVExprTest input1 expected1) (TXS2SMTVExprTest input2 expected2) =
    TXS2SMTVExprTest (cstrIte inputc input1 input2)
           ("(ite " ++ expectedc ++ " " ++ expected1 ++ " " ++ expected2 ++ ")")

createVequal :: TXS2SMTVExprTest -> TXS2SMTVExprTest -> TXS2SMTVExprTest
createVequal (TXS2SMTVExprTest input1 expected1) (TXS2SMTVExprTest input2 expected2) =
    TXS2SMTVExprTest (cstrEqual input1 input2) ("(= " ++ expected1 ++ " " ++ expected2 ++ ")" )     -- TODO: TXS can change order -- order should not be fixed ?? use regex??
    
createVpredef :: PredefKind -> FuncId -> [TXS2SMTVExprTest] -> TXS2SMTVExprTest
createVpredef predefkind funcId ies =
    TXS2SMTVExprTest (cstrPredef predefkind funcId (map input ies))
               ("(" ++ FuncId.name funcId ++ " " ++ join " " (map expected ies) ++ ")")

createVand :: Set.Set TXS2SMTVExprTest -> TXS2SMTVExprTest 
createVand conds =
    TXS2SMTVExprTest (cstrAnd (Set.map input conds))
                     ("(and " ++ join " " (Set.toList (Set.map expected conds)) ++ ")")
               
----------------------------------------------------------------
-- functions
------------------------------------------------------------------
createUniminusInt :: TXS2SMTVExprTest -> TXS2SMTVExprTest
createUniminusInt ie = 
    createVpredef SSI funcId_uniminusInt [ie]