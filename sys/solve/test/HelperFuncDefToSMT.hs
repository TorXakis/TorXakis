{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE OverloadedStrings #-}
module HelperFuncDefToSMT
where
import qualified Data.Map          as Map
import           Data.Maybe
import           Data.String.Utils
import           Data.Text         (Text)
import qualified Data.Text         as T

import           TorXakis.Sort
import           TorXakis.ValExpr.FuncId
import           FuncDef
import           HelperVexprToSMT
import           SMTData
import           VarId
------------------------------
-- Data types
---------------------------------------------------------------------------
data  TXS2SMTFuncTest         =  TXS2SMTFuncTest { input    :: EnvDefs
                                                 , expected :: String
                                                 }
     deriving (Eq,Ord,Read,Show)
---------------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------------

createFunctionId :: Text -> Int -> [VarId] -> SortId -> FuncId
createFunctionId n u vs = FuncId  n (Id u) (map varsort vs)

-- non-recursive function (is mapped on recursive function)
createFunctionDef :: EnvNames -> FuncId -> [VarId] -> SortId -> TXS2SMTVExprTest -> TXS2SMTFuncTest
createFunctionDef = createFunctionDefRecursive

-- one recursive function
createFunctionDefRecursive :: EnvNames -> FuncId -> [VarId] -> SortId -> TXS2SMTVExprTest -> TXS2SMTFuncTest
createFunctionDefRecursive mapI fn vs sort expr =
    createFunctionDefsRecursive mapI [(fn,vs,sort,expr)]

-- group of recusive functions
createFunctionDefsRecursive :: EnvNames -> [(FuncId, [VarId], SortId, TXS2SMTVExprTest)] -> TXS2SMTFuncTest
createFunctionDefsRecursive mapI l =
    TXS2SMTFuncTest (EnvDefs Map.empty Map.empty (foldr insert Map.empty l) )
                    ("(define-funs-rec\n  (\n" ++ concatMap define l ++ "  )\n  (\n" ++ concatMap body l ++ "  )\n)\n")
  where
    insert :: (FuncId, [VarId], SortId, TXS2SMTVExprTest) -> Map.Map FuncId (FuncDef VarId) -> Map.Map FuncId (FuncDef VarId)
    insert (fn, vs, _sort, expr) = Map.insert fn (FuncDef vs (HelperVexprToSMT.input expr))
    define :: (FuncId, [VarId], SortId, TXS2SMTVExprTest) -> String
    define (fn, vs, sort, _expr) = "    (" ++ T.unpack (justLookupFunc fn mapI) ++ "(" ++ join " " (map (\vi -> "(" ++ toSMTVar vi ++ " " ++ T.unpack (justLookupSort (varsort vi) mapI) ++ ")") vs) ++ ") " ++ T.unpack (justLookupSort sort mapI) ++ ")\n"
    body :: (FuncId, [VarId], SortId, TXS2SMTVExprTest) -> String
    body (_, _, _, expr)    = "    " ++ HelperVexprToSMT.expected expr ++ "\n"

justLookupSort :: SortId -> EnvNames -> Text
justLookupSort sd enames = fromMaybe (error $ "SortId " ++ show sd ++ " not found in mapping with keys: " ++ show (Map.keys (sortNames enames)) ++ "\n") (Map.lookup sd (sortNames enames))

justLookupFunc :: FuncId -> EnvNames -> Text
justLookupFunc fd enames = fromMaybe (error $ "FuncId " ++ show fd ++ " not found in mapping with keys: " ++ show (Map.keys (funcNames enames)) ++ "\n") (Map.lookup fd (funcNames enames))
