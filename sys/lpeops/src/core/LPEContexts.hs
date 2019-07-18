{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  LPEContexts
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module LPEContexts (
LPEContext,
getContextFromIds,
getAbbrevContextFromIds,
getLPEContext,
getLPESummandContext,
getLPEParamEqsContext,
getValExprContext,
getAbbrevLPEContext,
getAbbrevLPESummandContext,
getAbbrevLPEParamEqsContext,
getAbbrevValExprContext
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified TxsDefs
import           LPETypes
import           LPEContextIds
import           LPEChanMap

type LPEContext = Map.Map TxsDefs.Ident String

getContextFromIds :: Set.Set TxsDefs.Ident -> LPEContext
getContextFromIds = Map.fromSet (Text.unpack . TxsDefs.name)

getAbbrevContextFromIds :: Set.Set TxsDefs.Ident -> LPEContext
getAbbrevContextFromIds ids =
    Map.mapWithKey abbrevName (Map.fromList (zip (Set.toList ids) [1..]))
  where
    abbrevName :: TxsDefs.Ident -> Int -> String
    abbrevName j@(TxsDefs.IdSort _) i   = "Sort" ++ show i ++ "_" ++ Text.unpack (TxsDefs.name j) -- Must be capitalized!
    abbrevName j@(TxsDefs.IdCstr _) i   = "Cstr" ++ show i ++ "_" ++ Text.unpack (TxsDefs.name j) -- Must be capitalized!
    abbrevName j@(TxsDefs.IdFunc _) i   = "f" ++ show i ++ "_" ++ Text.unpack (TxsDefs.name j)
    abbrevName (TxsDefs.IdProc _) i   = "proc" ++ show i
    abbrevName (TxsDefs.IdChan _) i   = "Chan" ++ show i -- Must be capitalized!
    abbrevName (TxsDefs.IdVar _) i    = "v" ++ show i
    abbrevName (TxsDefs.IdStat _) i   = "Stat" ++ show i
    abbrevName (TxsDefs.IdModel _) i  = "Model" ++ show i
    abbrevName (TxsDefs.IdPurp _) i   = "Purp" ++ show i
    abbrevName (TxsDefs.IdGoal _) i   = "Goal" ++ show i
    abbrevName (TxsDefs.IdMapper _) i = "Mapper" ++ show i
    abbrevName (TxsDefs.IdCnect _) i  = "Cnect" ++ show i
-- getAbbrevContextFromIds

getLPEContext :: LPE -> LPEContext
getLPEContext = getContextFromIds . getLPEIds

getLPESummandContext :: LPEChanMap -> LPESummand -> LPEContext
getLPESummandContext chanMap summand = getContextFromIds (getLPESummandIds chanMap summand)

getLPEParamEqsContext :: LPEParamEqs -> LPEContext
getLPEParamEqsContext = getContextFromIds . getLPEParamEqsIds

getValExprContext :: TxsDefs.VExpr -> LPEContext
getValExprContext = getContextFromIds . getValExprIds

getAbbrevLPEContext :: LPE -> LPEContext
getAbbrevLPEContext = getAbbrevContextFromIds . getLPEIds

getAbbrevLPESummandContext :: LPEChanMap -> LPESummand -> LPEContext
getAbbrevLPESummandContext chanMap summand = getAbbrevContextFromIds (getLPESummandIds chanMap summand)

getAbbrevLPEParamEqsContext :: LPEParamEqs -> LPEContext
getAbbrevLPEParamEqsContext = getAbbrevContextFromIds . getLPEParamEqsIds

getAbbrevValExprContext :: TxsDefs.VExpr -> LPEContext
getAbbrevValExprContext = getAbbrevContextFromIds . getValExprIds





