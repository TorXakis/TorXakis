{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FuncDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Function Definition
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.FuncDef
( FuncDef(..)
, toFuncSignature
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           GHC.Generics        (Generic)

import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.VarDef
import           TorXakis.ValExpr.ValExpr

-- | Data structure to store the information of a Function Definition:
-- * A Name
-- * A list of variables
-- * A body (possibly using the variables)
data FuncDef v = FuncDef { -- | The name of the function (of type 'TorXakis.Name')
                           funcName :: Name
                           -- | The function parameter definitions
                         , paramDefs:: [v]
                           -- | The body of the function
                         , body :: ValExpr v
                         }
     deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

-- | return the function signature of the given function definition
toFuncSignature :: VarDef v => FuncDef v -> FuncSignature
toFuncSignature (FuncDef fn pds bd) = FuncSignature fn (map getSort pds) (getSort bd)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
