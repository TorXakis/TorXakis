{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FuncSignatureContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for FuncSignature.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.FuncSignatureContext
( -- * Context
  -- ** Func Signature Context
  FuncSignatureContext (..)
)
where
import qualified Data.Set               as Set

import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.Sort (SortContext)


------------------------------------------------------------------------------------------------------------------
-- Context
------------------------------------------------------------------------------------------------------------------

-- | A FuncSignatureContext Context instance contains all definitions to work with 'TorXakis.FuncSignature'.
class SortContext a => FuncSignatureContext a where
    -- | Accessor for defined variables
    funcSignatures :: a -> Set.Set (FuncSignature)

    -- | Add funcSignatures to funcSignature context.
    --   A funcSignature context is returned when the following constraints are satisfied:
    --
    --   * The signatures of the all funcSignatures are distinct
    --
    --   * All sorts are known
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addFuncSignatures :: a -> [FuncSignature] -> Either MinError a
