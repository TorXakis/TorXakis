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
( ProcDef(..)
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           GHC.Generics        (Generic)

import           TorXakis.BExpr.BExpr
import           TorXakis.ChanDef
import           TorXakis.Name
import           TorXakis.ProcExit
import           TorXakis.ProcSignature
import           TorXakis.Sort
import           TorXakis.VarContext
import           TorXakis.VarsDecl

-- | Data structure to store the information of a Process Definition:
-- * A Name
-- * A list of Channels
-- * A list of variables
-- * A body (possibly using the channels and variables)
data ProcDef = ProcDef { -- | The name of the process (of type 'TorXakis.Name')
                         procName :: Name
                         -- | The process channel definitions
                       , channelDefs:: [ChanDef]
                         -- | The process parameter definitions
                       , paramDefs:: VarsDecl
                         -- | The body of the process
                       , body :: BExpression
                       }
     deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

instance SortContext a => HasProcSignature a ProcDef
    where
        getProcSignature sctx (ProcDef fn cds pds bd) = case addVarDefs (fromSortContext sctx) (toList pds) of
                                                             Left e     -> error ("getProcSignature is unable to add vars to sort context" ++ show e)
                                                             Right vctx -> ProcSignature fn (map chanSort cds) (map (getSort sctx) (toList pds)) (getProcExit vctx bd)
-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
