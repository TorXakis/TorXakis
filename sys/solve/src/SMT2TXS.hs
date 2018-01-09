{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}
module SMT2TXS

-- ----------------------------------------------------------------------------------------- --
--
-- Translate SMT results to TorXakis ValExpr's
--
-- ----------------------------------------------------------------------------------------- --
-- export

( smtValueToValExpr   --  :: SMTValue -> TxsDefs -> SortId -> Walue
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import qualified Data.Map          as Map
import           Data.Monoid
import           Data.Text         (Text)
import qualified Data.Text         as T

import           ConstDefs
import           CstrDef
import           CstrId
import           SMTHappy
import           SortId


-- ---------------------------------------------------------
-- Note:
-- performance might improve, when parsing of smt value
-- and mapping onto Torxakis data structure is combined
-- into a single attribute grammar
-- ----------------------------------------------------------

-- ----------------------------------------------------------------------------------------- --
-- | lookup a constructor given its sort and constructor name in the given TorXakis definitions
lookupConstructor :: Map.Map CstrId CstrDef -> SortId -> Text -> CstrId
lookupConstructor cstrMap sid n
  =  case [ cstr
          | cstr@CstrId{ CstrId.name = n', cstrsort = sid' } <- Map.keys cstrMap
          , n == n'
          , sid == sid'
          ] of
     { [c] -> c
     ; _   -> error $ "TXS SMT2TXS lookupConstructor: No (unique) constructor for sort " ++
                      show sid ++ " and name " ++ show n ++ "\n"
     }



-- | convert an SMT expression to a ValExpr given a varName the varName is the
-- name of a SMT identifier that refers to a SMT variable.
smtValueToValExpr :: SMTValue -> Map.Map CstrId CstrDef -> SortId -> Const
smtValueToValExpr (SMTBool b) _ srt
  =  if sortIdBool == srt
       then Cbool b
       else Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - " ++
                     "Bool expected, got " ++ show srt ++ "\n"

smtValueToValExpr (SMTInt i) _ srt
  =  if sortIdInt == srt
       then Cint i
       else Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - " ++
                     "Int expected, got " ++ show srt ++ "\n"

smtValueToValExpr (SMTString s) _ srt
  =  if sortIdString == srt
       then Cstring s
       else Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - " ++
                     "String expected, got " ++ show srt ++ "\n"

smtValueToValExpr (SMTConstructor cname argValues) cstrMap srt =
    let nameSort = SortId.name srt in
        if T.isPrefixOf (nameSort <> "$") cname
            then  -- look up internal CstrId
                let cstrid = lookupConstructor cstrMap srt (T.drop (1 + T.length nameSort) cname) in
                    if length (cstrargs cstrid) == length argValues
                        then  -- recursively translate the arguments:
                            let vexprArgs = map (\(argValue, sort') -> smtValueToValExpr argValue cstrMap sort')
                                                (zip argValues (cstrargs cstrid)) in
                                Cstr cstrid vexprArgs
                        else Cerror $ "TXS SMT2TXS smtValueToValExpr: Number of arguments mismatch " ++
                                      "in constructor " ++ show cname ++ " of sort " ++ show nameSort ++
                                      " : definition " ++ show (length (cstrargs cstrid)) ++
                                      " vs actual " ++ show (length argValues) ++ "\n"
            else Cerror $ "TXS SMT2TXS smtValueToValExpr: CstrName " ++ show cname ++
                          " does not start with sort " ++ show nameSort ++ "\n"
