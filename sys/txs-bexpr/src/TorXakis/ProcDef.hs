{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ProcDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Process Definition
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.ProcDef
( ProcDef
, procName
, channelDefs
, paramDefs
, body
, mkProcDef
, undefinedSortsInProcs
, undefinedVariablesInProcs
, undefinedChannelsInProcs
, undefinedFuncSignaturesInProcs
)
where

import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import           Data.Maybe             (mapMaybe)
import qualified Data.Set               as Set
import           GHC.Generics           (Generic)

import           TorXakis.BExpr.BExpr
import           TorXakis.Chan
import           TorXakis.ContextChanVar
import           TorXakis.ContextVar
import           TorXakis.Error
import           TorXakis.FuncContext
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.ProcSignature hiding (procName)
import           TorXakis.Sort
import           TorXakis.Var

-- | Data structure to store the information of a Process Definition:
-- * A Name
-- * A list of Channels
-- * A list of variables
-- * A body (possibly using the channels and variables)
data ProcDef = ProcDef { -- | The name of the process (of type 'TorXakis.Name')
                         procName :: Name
                         -- | The process channel definitions
                       , channelDefs:: ChansDecl
                         -- | The process parameter definitions
                       , paramDefs:: VarsDecl
                         -- | The body of the process
                       , body :: BExpression
                       }
     deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

-- | constructor for ProcDef
mkProcDef :: FuncContext c => c -> Name -> ChansDecl -> VarsDecl -> BExpression -> Either Error ProcDef
mkProcDef ctx n cs ps b | not (Set.null sortsUndefined) = Left $ Error ("Process definition has undefined sorts: " ++ show sortsUndefined)
                        | not (Set.null varsUndefined)  = Left $ Error ("Process definition has undefined variables in body: " ++ show varsUndefined)
                        | not (Set.null chansUndefined) = Left $ Error ("Process definition has undefined channels in body: " ++ show chansUndefined)
                        | not (Set.null funcsUndefined) = Left $ Error ("Process definition has undefined function signatures in body: " ++ show funcsUndefined)
                        | otherwise                     = Right $ ProcDef n cs ps b
    where
        sortsUndefined = undefinedSorts ctx cs ps b
        varsUndefined  = undefinedVariables ps b
        chansUndefined = undefinedChannels cs b
        funcsUndefined = undefinedFuncSignatures ctx b

toChanVarContext :: SortContext c => c -> ChansDecl -> VarsDecl -> ContextChanVar
toChanVarContext ctx cs ps =
    case addVars (TorXakis.Var.toList ps) (TorXakis.ContextChanVar.fromSortContext ctx) of
         Left e      -> error ("toChanVarContext is unable to add variables: " ++ show e)
         Right vctx  -> case addChans (TorXakis.Chan.toList cs) vctx of
                             Left e      -> error ("toChanVarContext is unable to add channels: " ++ show e)
                             Right cvctx -> cvctx

-- | Helper process for construction: check for undefined sorts per process definition
undefinedSortsInProcs :: SortContext c => c -> [ProcDef] -> [(ProcSignature, Set.Set Sort)]
undefinedSortsInProcs ctx = mapMaybe maybeUndefinedSortsInProc
    where
        maybeUndefinedSortsInProc :: ProcDef -> Maybe (ProcSignature, Set.Set Sort)
        maybeUndefinedSortsInProc p = let sortsUndefined = undefinedSorts ctx (channelDefs p) (paramDefs p) (body p)
                                         in if Set.null sortsUndefined
                                               then Nothing
                                               else Just (getProcSignature ctx p, sortsUndefined)

-- | defined sorts: we have to include all since
--    * Defined channels and variables might not be used in the body.
--    * In the body additional sorts can be used
undefinedSorts :: SortContext c => c -> ChansDecl -> VarsDecl -> BExpression -> Set.Set Sort
undefinedSorts ctx cs ps b = let sortsDefined   = Set.fromList (elemsSort ctx)
                                 sortsUsed      = Set.unions [ usedSorts ctx cs
                                                             , usedSorts ctx ps
                                                             , usedSorts (toChanVarContext ctx cs ps) b
                                                             ]
                            in sortsUsed `Set.difference` sortsDefined

-- | Helper process for construction: check for undefined funcSignatures per process definition
undefinedFuncSignaturesInProcs :: FuncContext c => c -> [ProcDef] -> [(ProcSignature, Set.Set FuncSignature)]
undefinedFuncSignaturesInProcs ctx = mapMaybe maybeUndefinedFuncSignaturesInProc
    where
        maybeUndefinedFuncSignaturesInProc :: ProcDef -> Maybe (ProcSignature, Set.Set FuncSignature)
        maybeUndefinedFuncSignaturesInProc p = let funcSignaturesUndefined = undefinedFuncSignatures ctx (body p)
                                                in if Set.null funcSignaturesUndefined
                                                      then Nothing
                                                      else Just (getProcSignature ctx p, funcSignaturesUndefined)

-- | defined funcSignatures: 
--    * Only in the body funcSignatures are used.
undefinedFuncSignatures :: FuncContext c => c -> BExpression -> Set.Set FuncSignature
undefinedFuncSignatures ctx b = let funcSignaturesDefined = Set.fromList (funcSignatures ctx)
                                    funcSignaturesUsed    = usedFuncSignatures b
                                 in funcSignaturesUsed `Set.difference` funcSignaturesDefined

-- | Helper process for construction: check for undefined variables per process definition
undefinedVariablesInProcs :: SortContext c => c -> [ProcDef] -> [(ProcSignature, Set.Set (RefByName VarDef))]
undefinedVariablesInProcs ctx = mapMaybe maybeUndefinedVariablesInProc
    where
        maybeUndefinedVariablesInProc :: ProcDef -> Maybe (ProcSignature, Set.Set (RefByName VarDef))
        maybeUndefinedVariablesInProc p = let variablesUndefined = undefinedVariables (paramDefs p) (body p)
                                            in if Set.null variablesUndefined
                                                  then Nothing
                                                  else Just (getProcSignature ctx p, variablesUndefined)

undefinedVariables :: VarsDecl -> BExpression -> Set.Set (RefByName VarDef)
undefinedVariables ps b = let variablesDefined :: Set.Set (RefByName VarDef)
                              variablesDefined   = Set.fromList (map (RefByName . name) (TorXakis.Var.toList ps))
                              variablesUsed      = freeVars b
                            in variablesUsed `Set.difference` variablesDefined

-- | Helper process for construction: check for undefined channels per process definition
undefinedChannelsInProcs :: SortContext c => c -> [ProcDef] -> [(ProcSignature, Set.Set (RefByName ChanDef))]
undefinedChannelsInProcs ctx = mapMaybe maybeUndefinedChannelsInProc
    where
        maybeUndefinedChannelsInProc :: ProcDef -> Maybe (ProcSignature, Set.Set (RefByName ChanDef))
        maybeUndefinedChannelsInProc p = let channelsUndefined = undefinedChannels (channelDefs p) (body p)
                                            in if Set.null channelsUndefined
                                                  then Nothing
                                                  else Just (getProcSignature ctx p, channelsUndefined)

undefinedChannels :: ChansDecl -> BExpression -> Set.Set (RefByName ChanDef)
undefinedChannels cs b = let channelsDefined :: Set.Set (RefByName ChanDef)
                             channelsDefined   = Set.fromList (map (RefByName . chanName) (TorXakis.Chan.toList cs))
                             channelsUsed      = freeChans b
                           in channelsUsed `Set.difference` channelsDefined

instance SortContext a => HasProcSignature a ProcDef
    where
        getProcSignature sctx (ProcDef fn cds pds bd) = case addVars (TorXakis.Var.toList pds) (TorXakis.ContextVar.fromSortContext sctx) of
                                                             Left e     -> error ("getProcSignature is unable to add vars to sort context" ++ show e)
                                                             Right vctx -> ProcSignature fn
                                                                                         (map chanSort (TorXakis.Chan.toList cds))
                                                                                         (map (getSort sctx) (TorXakis.Var.toList pds))
                                                                                         (getProcExit vctx bd)
-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
