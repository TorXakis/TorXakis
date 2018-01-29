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
( smtValueToValExpr
)
where

import qualified Data.Map          as Map
import           Data.Maybe
import           Data.Text         (Text)

import           ConstDefs
import           Identifier
import           SMTHappy
import           Sort

-- ---------------------------------------------------------
-- Note:
-- performance might improve, when parsing of smt value
-- and mapping onto Torxakis data structure is combined
-- into a single attribute grammar
-- ----------------------------------------------------------

-- | convert an SMT expression to a ValExpr given a varName the varName is the
-- name of a SMT identifier that refers to a SMT variable.
smtValueToValExpr :: SMTValue -> ADTDefs -> Sort -> Const
smtValueToValExpr (SMTBool b) _ srt
  = case srt of
        SortBool -> Cbool b
        _        -> Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - " ++
                        "Sort '" ++ show srt ++ "' expected, got Bool\n"

smtValueToValExpr (SMTInt i) _ srt
  = case srt of
        SortInt -> Cint i
        _       -> Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - " ++
                        "Sort '" ++ show srt ++ "' expected, got Int\n"

smtValueToValExpr (SMTString s) _ srt
  =  case srt of
       SortString -> Cstring s
       _          -> Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - " ++
                        "Sort '" ++ show srt ++ "' expected, got String\n"

smtValueToValExpr (SMTConstructor s argValues) adtDefs srt
  =  case srt of
        SortADT adtRf ->
            let (rfAdt, refCstr) = decode s 
            in  if adtRf == rfAdt 
                    then let adtDef  = fromMaybe (error $ "error in encode/decode ADTDef: " ++ show s)
                                        $ Map.lookup rfAdt
                                        $ adtDefsToMap adtDefs
                             cstrDef = fromMaybe (error $ "error in encode/decode ConstructorDef: " ++ show s)
                                        $ Map.lookup refCstr
                                        $ cDefsToMap $ constructors adtDef
                         in  if nrOfFieldDefs (fields cstrDef) == length argValues
                                then  -- recursively translate the arguments:
                                    let vexprArgs = map (\(argValue, srt') -> smtValueToValExpr argValue adtDefs srt')
                                                        (zip argValues $ sortsOfFieldDefs $ fields cstrDef)
                                    in  Cstr adtRf refCstr vexprArgs
                                else Cerror $ "TXS SMT2TXS smtValueToValExpr: Number of arguments mismatch " ++
                                            "in constructor " ++ show (constructorName cstrDef) ++ " of srt " ++ show (adtName adtDef) ++
                                            " : definition " ++ show (nrOfFieldDefs $ fields cstrDef) ++
                                            " vs actual " ++ show (length argValues) ++ "\n"
                    else Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - " ++
                            "Sort '" ++ show srt ++ "' expected, got ADTDef '" ++ show rfAdt ++ "'\n"
        _ -> Cerror $ "TXS SMT2TXS smtValueToValExpr: Type mismatch - " ++
                "Sort '" ++ show srt ++ "' expected, got ADTDef '" ++ show s ++ "'\n" 

decode :: Text -> (Ref ADTDef, Ref ConstructorDef)
decode = undefined