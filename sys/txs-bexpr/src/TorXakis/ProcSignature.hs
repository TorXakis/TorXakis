{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ProcSignature
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Kerem Ispirli <kerem.ispirli@tno.nl>
--                Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the data structure for a Signature of a Process.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module TorXakis.ProcSignature
( -- * Process Signature
  ProcSignature (..)
  -- * Has Process Signature class
, HasProcSignature (..)
  -- ** Conversion List to Map By Process Signature
, toMapByProcSignature
  -- ** Repeated Process Signatures functions
, repeatedByProcSignature
, repeatedByProcSignatureIncremental
) where

import           Control.DeepSeq      (NFData)
import           Data.Data            (Data)
import           Data.Hashable        (Hashable(hashWithSalt))
import           Data.HashMap         (Map, fromList)
import           Data.List.Unique     (repeated)
import           GHC.Generics         (Generic)

import           TorXakis.BExpr.ExitKind
import           TorXakis.ChanDef
import           TorXakis.Name
import           TorXakis.Sort

-- | A generalized, type-safe reference.
data ProcSignature = ProcSignature { -- | The 'Name' of the process.
                                     procName :: Name
                                     -- | The 'ChanSort's of the process' channels.
                                   , channels :: [ChanSort]
                                     -- | The 'Sort's of the process' arguments.
                                   , args :: [Sort]
                                     -- | The 'ExitKind' of the process.
                                   , exitKind :: ExitKind
                                   }
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

-- | Enables 'ProcSignature's of entities to be accessed in a common way.
class HasProcSignature e where
    -- | return the process signature of the given element
    getProcSignature :: e -> ProcSignature

instance HasProcSignature ProcSignature where
    getProcSignature = id

instance Hashable ProcSignature where
    hashWithSalt s (ProcSignature n cs as r) = s `hashWithSalt`
                                               n `hashWithSalt`
                                               cs `hashWithSalt`
                                               as `hashWithSalt`
                                               r

-- | Return 'Data.HashMap.Map' where the 'ProcSignature' of the element is taken as key
--   and the element itself is taken as value.
toMapByProcSignature :: HasProcSignature a => [a] -> Map ProcSignature a
toMapByProcSignature = fromList . map (\e -> (getProcSignature e,e))

-- |  Return the elements with non-unique process signatures that the second list contains in the combination of the first and second list.
repeatedByProcSignatureIncremental :: (HasProcSignature a, HasProcSignature b) => [a] -> [b] -> [b]
repeatedByProcSignatureIncremental xs ys = filter ((`elem` nuProcSignatures) . getProcSignature) ys
    where nuProcSignatures = repeated $ map getProcSignature xs ++ map getProcSignature ys

-- | Return the elements with non-unique process signatures: 
-- the elements with a 'ProcSignature' that is present more than once in the list.
repeatedByProcSignature :: (HasProcSignature a) => [a] -> [a]
repeatedByProcSignature = repeatedByProcSignatureIncremental ([] :: [ProcSignature])