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
import           TorXakis.ContextVar
import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.ProcSignature
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
mkProcDef :: SortContext c => c -> Name -> ChansDecl -> VarsDecl -> BExpression -> Either Error ProcDef
mkProcDef ctx n cs ps b | not (Set.null sortsUndefined) = Left $ Error ("Process definition has undefined sorts: " ++ show sortsUndefined)
                        | not (Set.null varsUndefined)  = Left $ Error ("Process definition has undefined variables in body: " ++ show varsUndefined)
                        | otherwise                     = Right $ ProcDef n cs ps b
    where
        sortsUndefined = undefinedSorts ctx cs ps b
        varsUndefined  = undefinedVariables ps b

toVarContext :: SortContext c => c -> VarsDecl -> ContextVar
toVarContext ctx ps =
    case addVars (TorXakis.Var.toList ps) (fromSortContext ctx) of
        Left e      -> error ("toVarContext is unable to make new context" ++ show e)
        Right vctx  -> vctx

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
--    * In the body additional sorts can be used.
undefinedSorts :: SortContext c => c -> ChansDecl -> VarsDecl -> BExpression -> Set.Set Sort
undefinedSorts ctx cs ps b = let sortsDefined   = Set.fromList (elemsSort ctx)
                                 sortsUsed      = Set.unions [ usedSorts ctx cs
                                                             , usedSorts ctx ps
                                                             , usedSorts (toVarContext ctx ps) b        -- TODO: don't we need channels as well?
                                                             ]
                            in sortsUsed `Set.difference` sortsDefined

-- | Helper process for construction: check for undefined variables per process definition
undefinedVariablesInProcs :: SortContext c => c -> [ProcDef] -> [(ProcSignature, Set.Set (RefByName VarDef))]
undefinedVariablesInProcs ctx = mapMaybe maybeUndefinedVariablesInProc
    where
        maybeUndefinedVariablesInProc :: ProcDef -> Maybe (ProcSignature, Set.Set (RefByName VarDef))
        maybeUndefinedVariablesInProc p = let variablesUndefined = undefinedVariables (paramDefs p) (body p)
                                            in if Set.null variablesUndefined
                                                  then Nothing
                                                  else Just (getProcSignature ctx f, variablesUndefined)

undefinedVariables :: VarsDecl -> BExpression -> Set.Set (RefByName VarDef)
undefinedVariables ps b = let variablesDefined :: Set.Set (RefByName VarDef)
                              variablesDefined   = Set.fromList (map (RefByName . name) (TorXakis.Var.toList ps))
                              variablesUsed      = freeVars b
                            in variablesUsed `Set.difference` variablesDefined

instance SortContext a => HasProcSignature a ProcDef
    where
        getProcSignature sctx (ProcDef fn cds pds bd) = case addVars (TorXakis.Var.toList pds) (fromSortContext sctx) of
                                                             Left e     -> error ("getProcSignature is unable to add vars to sort context" ++ show e)
                                                             Right vctx -> ProcSignature fn
                                                                                         (map chanSort cds)
                                                                                         (map (getSort sctx) (TorXakis.Var.toList pds))
                                                                                         (getProcExit vctx bd)
-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
