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
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.FuncSignatureContext
( -- * Context
  -- ** Func Signature Context class
  FuncSignatureContext (..)
  -- ** Func Signature Modify Context class
, FuncSignatureModifyContext (..)
)
where
import           TorXakis.Error
import           TorXakis.FuncSignature
import           TorXakis.SortContext

-- | A FuncSignatureContext Context instance contains all definitions to work with 'TorXakis.FuncSignature'.
class SortContext a => FuncSignatureContext a where
    -- | Is the provided FuncSignature a member of the context?
    memberFunc :: a -> FuncSignature -> Bool
    -- | All funcSignatures in the context.
    -- 
    -- Since all funcSignatures are distinct the following properties hold:
    --
    -- prop> List.nub (funcSignatures x) == funcSignatures x
    -- 
    -- prop> Set.toList (Set.fromList (funcSignatures x)) == funcSignatures x
    funcSignatures :: a -> [FuncSignature]

-- | A FuncSignatureModifyContext instance contains all operations that provide as output a modified context of 'TorXakis.FuncSignature'.
class (FuncSignatureContext a, FuncSignatureContext b) => FuncSignatureModifyContext a b where
    -- | Add funcSignatures to funcSignature context.
    --   A funcSignature context is returned when the following constraints are satisfied:
    --
    --   * All funcSignatures are distinct
    --
    --   * All sorts are known
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addFuncSignatures :: a -> [FuncSignature] -> Either Error b