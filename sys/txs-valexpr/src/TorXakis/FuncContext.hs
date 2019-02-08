{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FuncContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Function Context
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.FuncContext
( -- * Context
  -- ** Func Context class
  FuncContext (..)
)
where

import           TorXakis.Error
import           TorXakis.FuncDef
import           TorXakis.FuncSignature
import           TorXakis.FuncSignatureContext

-- | A FuncContext Context instance contains all definitions to work with 'TorXakis.FuncDef'.
class FuncSignatureContext a => FuncContext a where
    -- | lookup FuncDef
    lookupFunc :: a -> FuncSignature -> Maybe FuncDef
    -- | All FuncDef elements in the context
    elemsFunc :: a -> [FuncDef]
    -- | Add FuncDefs to func context.
    --   A func context is returned when the following constraints are satisfied:
    --
    --   * All func Signatures are distinct
    --
    --   * All references are known (sort, variables, func signatures)
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addFuncs :: a -> [FuncDef] -> Either Error a
-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
