{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ProcSignatureContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for ProcSignatures.
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.ProcSignatureContext
( -- * Context
  -- ** Func Signature Context
  ProcSignatureContext (..)
)
where
import qualified Data.Set               as Set

import           TorXakis.Error
import           TorXakis.ProcSignature
import           TorXakis.Sort (SortContext)


------------------------------------------------------------------------------------------------------------------
-- Context
------------------------------------------------------------------------------------------------------------------

-- | A ProcSignatureContext Context instance contains all definitions to work with 'TorXakis.ProcSignatures'.
class SortContext a => ProcSignatureContext a where
    -- | Accessor for defined variables
    procSignatures :: a -> Set.Set ProcSignature

    -- | Add procSignatures to procSignature context.
    --   A procSignature context is returned when the following constraints are satisfied:
    --
    --   * All procSignatures are distinct
    --
    --   * All sorts are known
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addProcSignatures :: a -> [ProcSignature] -> Either MinError a
