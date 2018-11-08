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
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
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
import           TorXakis.ProcSignature
import           TorXakis.Sort
import           TorXakis.VarDef

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
                       , paramDefs:: [MinimalVarDef]
                         -- | The body of the process
                       , body :: BExpr
                       }
     deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

instance HasProcSignature ProcDef
    where
        getProcSignature (ProcDef fn cds pds bd) = ProcSignature fn (map chanSort cds) (map getSort pds) (getExitSort bd)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
