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
import           Data.Monoid
import           Data.Text         (Text)
import qualified Data.Text         as T

import           FuncDef
import           FuncId
import           HelperVexprToSMT
import           Id
import           Name
import           Sort
import           VarId

data  TXS2SMTFuncTest         =  TXS2SMTFuncTest { input    :: Map.Map FuncId (FuncDef VarId)
                                                 , expected :: Text
                                                 }
     deriving (Eq,Ord,Read,Show)


-- QUESTION: This toSortName converters are an exact copy from TXS2SMT.hs
-- (Kerem)   Doesn't this defeat the purpose of test?
--           Isn't it better to have these texts static within the tests,
--           so we see what we expect?
toSortName :: Sort -> Text
toSortName SortError   = error "Error is not defined in SMT"
toSortName SortBool    = "Bool"
toSortName SortInt     = "Int"
toSortName SortChar    = error "Char is not yet supported"
toSortName SortString  = "String"
toSortName SortRegex   = error "Regex is not defined in SMT"
toSortName (SortADT r) = toADTName r

createFunctionId :: Name -> Int -> [VarId] -> Sort -> FuncId
createFunctionId n u vs = FuncId n (Id u) (map varsort vs)

-- non-recursive function (is mapped on recursive function)
createFunctionDef :: FuncId -> [VarId] -> Sort -> TXS2SMTVExprTest -> TXS2SMTFuncTest
createFunctionDef = createFunctionDefRecursive

-- one recursive function
createFunctionDefRecursive :: FuncId -> [VarId] -> Sort -> TXS2SMTVExprTest -> TXS2SMTFuncTest
createFunctionDefRecursive fn vs sort expr =
    createFunctionDefsRecursive [(fn,vs,sort,expr)]

-- group of recusive functions
createFunctionDefsRecursive :: [(FuncId, [VarId], Sort, TXS2SMTVExprTest)] -> TXS2SMTFuncTest
createFunctionDefsRecursive l =
    TXS2SMTFuncTest (foldr insert Map.empty l)
                    ("(define-funs-rec\n  (\n" <> T.concat (map define l) <> "  )\n  (\n" <> T.concat (map body l) <> "  )\n)\n")
    where
        insert (fn, vs, _sort, expr) = Map.insert fn (FuncDef vs (HelperVexprToSMT.input expr))
        define (fn, vs, sort, _expr) = "    (" <> toFuncName fn <> "(" <> T.intercalate " " (map (\vi -> "(" <> toSMTVar vi <> " " <> toSortName (varsort vi) <> ")") vs) <> ") " <> toSortName sort <> ")\n"
        body (_, _, _, expr)         = "    " <> HelperVexprToSMT.expected expr <> "\n"
