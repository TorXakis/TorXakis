{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SMT2TXS
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Translate SMT results to TorXakis ValExpr's
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module SMT2TXS
( smtValueToValExpr   --  :: SMTValue -> TxsDefs -> SortId -> Walue
)
where

import qualified Data.Map          as Map
import           Data.Monoid
import           Data.Text         (Text)
import qualified Data.Text         as T

import           ADTDef
import           ConstDefs
import           ConstructorDef
import           FieldDef
import           Identifier
import           SMTHappy
import           SortDef
import           StandardSortRefs

-- ---------------------------------------------------------
-- Note:
-- performance might improve, when parsing of smt value
-- and mapping onto Torxakis data structure is combined
-- into a single attribute grammar
-- ----------------------------------------------------------

-- | convert an SMT expression to a ValExpr given a varName the varName is the
-- name of a SMT identifier that refers to a SMT variable.
smtValueToValExpr :: SMTValue -> ADTDefs -> TRef SortDef -> Const
smtValueToValExpr (SMTBool b) _ sortRef
  =  if sortRefBool == sortRef
       then Cbool b
       else Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - " ++
                     "Bool expected, got " ++ show sortRef ++ "\n"

smtValueToValExpr (SMTInt i) _ sortRef
  =  if sortRefInt == sortRef
       then Cint i
       else Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - " ++
                     "Int expected, got " ++ show sortRef ++ "\n"

smtValueToValExpr (SMTString s) _ sortRef
  =  if sortRefString == sortRef
       then Cstring s
       else Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - " ++
                     "String expected, got " ++ show sortRef ++ "\n"

smtValueToValExpr (SMTConstructor adtRef cstrRef argValues) adtDefs sortRef =
    if adtRef == sortRef
        then let adtDef = getADTDef adtRef adtDefs
                 cstrDef = getConstructorDef cstrRef $ constructors adtDef
             in if FieldDef.nrOfFieldDefs $ fields cstrDef == length argValues
                    then  -- recursively translate the arguments:
                        let vexprArgs = map (\(argValue, sort') -> smtValueToValExpr argValue cstrMap sort')
                                            (zip argValues (cstrargs cstrid)) in
                            Cstr cstrid vexprArgs
                    else Cerror $ "TXS SMT2TXS smtValueToValExpr: Number of arguments mismatch " ++
                                "in constructor " ++ show cname ++ " of sort " ++ show nameSort ++
                                " : definition " ++ show (length (cstrargs cstrid)) ++
                                " vs actual " ++ show (length argValues) ++ "\n"
                Cstr adtref cstRef veprargs
            else Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - "
                        "ADTDef '" ++ show sortRef ++ "' expected, got ADTDef '" ++ show adtRef ++ "'\n" 
