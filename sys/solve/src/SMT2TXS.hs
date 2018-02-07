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
-- Translate SMT results to TorXakis Const's
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module SMT2TXS
( smtValueToConst
)
where

import qualified Data.Map          as Map
import           Data.Maybe
import           Data.Text         (Text)

import           ConstDefs
import           Identifier
import           SMTHappy
import           Sort

-- | convert an SMT expression to a Constant.
smtValueToConst :: SMTValue -> Sort -> ADTDefs -> Map.Map Text (Ref ADTDef, Ref ConstructorDef) ->  Const
smtValueToConst (SMTBool b) srt _ _
  = case srt of
        SortBool -> Cbool b
        _        -> Cerror $ "TXS SMT2TXS smtValueToConst: Type mismatch - " ++
                        "Sort '" ++ show srt ++ "' expected, got Bool\n"

smtValueToConst (SMTInt i) srt _ _ 
  = case srt of
        SortInt -> Cint i
        _       -> Cerror $ "TXS SMT2TXS smtValueToConst: Type mismatch - " ++
                        "Sort '" ++ show srt ++ "' expected, got Int\n"

smtValueToConst (SMTString s) srt _ _
  =  case srt of
       SortString -> Cstring s
       _          -> Cerror $ "TXS SMT2TXS smtValueToConst: Type mismatch - " ++
                        "Sort '" ++ show srt ++ "' expected, got String\n"

smtValueToConst (SMTConstructor s argValues) srt aDefs decoder
  =  case srt of
        SortADT adtRf ->
            let (rfAdt, refCstr) = fromMaybe (error $ "error in encode/decode Name: " ++ show s)
                                     $ Map.lookup s decoder 
            in  if adtRf == rfAdt 
                    then let adtDef  = fromMaybe (error $ "error in encode/decode ADTDef: " ++ show s)
                                        $ Map.lookup rfAdt
                                        $ adtDefsToMap aDefs
                             cstrDef = fromMaybe (error $ "error in encode/decode ConstructorDef: " ++ show s)
                                        $ Map.lookup refCstr
                                        $ cDefsToMap $ constructors adtDef
                         in  if nrOfFieldDefs (fields cstrDef) == length argValues
                                then  -- recursively translate the arguments:
                                    let vexprArgs = map (\(argValue, srt') -> smtValueToConst argValue srt' aDefs decoder)
                                                        (zip argValues $ sortsOfFieldDefs $ fields cstrDef)
                                    in Cstr adtRf refCstr vexprArgs
                                else Cerror $ "TXS SMT2TXS smtValueToConst: Number of arguments mismatch " ++
                                            "in constructor " ++ show (constructorName cstrDef) ++ " of srt " ++ show (adtName adtDef) ++
                                            " : definition " ++ show (nrOfFieldDefs $ fields cstrDef) ++
                                            " vs actual " ++ show (length argValues) ++ "\n"
                    else Cerror $ "TXS SMT2TXS smtValueToConst: Type mismatch - " ++
                            "Sort '" ++ show srt ++ "' expected, got ADTDef '" ++ show rfAdt ++ "'\n"
        _ -> Cerror $ "TXS SMT2TXS smtValueToConst: Type mismatch - " ++
                "Sort '" ++ show srt ++ "' expected, got ADTDef '" ++ show s ++ "'\n" 