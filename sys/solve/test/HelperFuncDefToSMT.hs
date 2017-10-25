{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module HelperFuncDefToSMT
where
import qualified Data.Map          as Map
import           Data.Text         (Text)
import qualified Data.Text         as T

import           Data.String.Utils
import           HelperVexprToSMT
import           TXS2SMT
import           TxsDefs
import           VarId
------------------------------
-- Data types
---------------------------------------------------------------------------
data  TXS2SMTFuncTest         =  TXS2SMTFuncTest { input    :: TxsDefs
                                                 , expected :: String
                                                 }
     deriving (Eq,Ord,Read,Show)
---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

createFunctionId :: Text -> Int -> [VarId] -> SortId -> FuncId
createFunctionId n u vs = FuncId  n u (map varsort vs)

-- non-recursive function (is mapped on recursive function)
createFunctionDef :: Map.Map Ident Text -> FuncId -> [VarId] -> SortId -> TXS2SMTVExprTest -> TXS2SMTFuncTest
createFunctionDef = createFunctionDefRecursive

-- one recursive function
createFunctionDefRecursive :: Map.Map Ident Text -> FuncId -> [VarId] -> SortId -> TXS2SMTVExprTest -> TXS2SMTFuncTest
createFunctionDefRecursive mapI fn vs sort expr =
    createFunctionDefsRecursive mapI [(fn,vs,sort,expr)]

-- group of recusive functions
createFunctionDefsRecursive :: Map.Map Ident Text -> [(FuncId, [VarId], SortId, TXS2SMTVExprTest)] -> TXS2SMTFuncTest
createFunctionDefsRecursive mapI l =
    TXS2SMTFuncTest (foldr insert TxsDefs.empty l)
                    ("(define-funs-rec\n  (\n" ++ concatMap define l ++ "  )\n  (\n" ++ concatMap body l ++ "  )\n)\n")
  where
    insert (fn, vs, _sort, expr) = TxsDefs.insert (IdFunc fn) (DefFunc (FuncDef vs (HelperVexprToSMT.input expr)))
    define (fn, vs, sort, _expr) = "    (" ++ T.unpack (justLookup mapI (IdFunc fn)) ++ "(" ++ join " " (map (\vi -> "(" ++ toSMTVar vi ++ " " ++ T.unpack (justLookup mapI (IdSort (varsort vi))) ++ ")") vs) ++ ") " ++ T.unpack (justLookup mapI (IdSort sort)) ++ ")\n"
    body (_, _, _, expr)    = "    " ++ HelperVexprToSMT.expected expr ++ "\n"

