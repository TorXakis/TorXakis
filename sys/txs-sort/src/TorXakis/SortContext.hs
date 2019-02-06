{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Sort.SortContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for Sort: all defined sorts and necessary other definitions
-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.SortContext
( -- * Sort Read Context
  SortReadContext (..)
  -- * Sort Context (Read and Write)
, SortContext (..)
, prettyPrintSortContext
)
where
import qualified Data.Text           as T

import           TorXakis.Error                 (Error)
import           TorXakis.Name                  (RefByName (RefByName), getName)
import           TorXakis.PrettyPrint.TorXakis
import           TorXakis.SortADT               (Sort(..), ADTDef)

-- | Sort Read Context
class SortReadContext a where
    -- | Is the provided sort a member of the context?
    memberSort :: a -> Sort -> Bool
    -- | All Sort elements in the context
    elemsSort :: a -> [Sort]
    elemsSort ctx =   SortBool
                    : SortInt
                    : SortChar
                    : SortString
                    : SortRegex
                    : map (SortADT . RefByName . getName) (elemsADTDef ctx)

    -- | Refers the provided ADTDef name to an ADTDef in the context?
    memberADTDef :: a -> RefByName ADTDef -> Bool
    -- | lookup ADTDef
    lookupADTDef :: a -> RefByName ADTDef -> Maybe ADTDef
    -- | All ADTDef elements in the context
    elemsADTDef :: a -> [ADTDef]

-- | Generic Pretty Printer for all instance of 'TorXakis.SortReadContext'.
prettyPrintSortContext :: SortReadContext a => Options -> a -> TxsString
prettyPrintSortContext o sc = TxsString (T.intercalate (T.pack "\n") (map (TorXakis.PrettyPrint.TorXakis.toText . prettyPrint o sc) (elemsADTDef sc)))

-- | A Sort Context instance 
-- contains all definitions to work with sorts and references thereof.
class SortReadContext a => SortContext a where
    -- | An empty sort context (initial state)
    empty :: a
    -- | Add adt definitions to sort context.
    --   A sort context is returned when the following constraints are satisfied:
    --
    --   * The 'Name's of ADTDef are unique
    --
    --   * All references are known
    --
    --   * All ADTs are constructable
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    addAdtDefs :: a -> [ADTDef] -> Either Error a