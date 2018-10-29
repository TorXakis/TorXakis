{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FuncSignature
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Kerem Ispirli <kerem.ispirli@tno.nl>
--                Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the data structure for a Signature of a Function.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module TorXakis.FuncSignature
( -- * Function Signature
  FuncSignature (..)
  -- * Has Function Signature class
, HasFuncSignature (..)
  -- ** Conversion List to Map By Function Signature
, toMapByFuncSignature
-- ** Repeated Function Signatures functions
, repeatedByFuncSignature
, repeatedByFuncSignatureIncremental
) where

import           Control.DeepSeq      (NFData)
import           Data.Data            (Data)
import           Data.Hashable        (Hashable(hashWithSalt))
import           Data.HashMap         (Map, fromList)
import           Data.List.Unique     (repeated)
import           GHC.Generics         (Generic)

import           TorXakis.Name
import           TorXakis.Sort

-- | A generalized, type-safe reference.
data FuncSignature = FuncSignature { -- | The 'Name' of the function.
                                     funcName :: Name
                                     -- | The 'Sort's of the function arguments.
                                   , args :: [Sort]
                                     -- | The 'Sort' of the result that the function returns.
                                   , returnSort :: Sort
                                   }
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)

-- | Enables 'FuncSignature's of entities to be accessed in a common way.
class HasFuncSignature e where
    -- | return the function signature of the given element
    getFuncSignature :: e -> FuncSignature

instance HasFuncSignature FuncSignature where
    getFuncSignature = id

instance Hashable FuncSignature where
    hashWithSalt s (FuncSignature n as r) = s `hashWithSalt`
                                            n `hashWithSalt`
                                            as `hashWithSalt`
                                            r

-- | Return 'Data.HashMap.Map' where the 'FuncSignature' of the element is taken as key
--   and the element itself is taken as value.
toMapByFuncSignature :: HasFuncSignature a => [a] -> Map FuncSignature a
toMapByFuncSignature = fromList . map (\e -> (getFuncSignature e,e))

-- |  Return the elements with non-unique function signatures that the second list contains in the combination of the first and second list.
repeatedByFuncSignatureIncremental :: (HasFuncSignature a, HasFuncSignature b) => [a] -> [b] -> [b]
repeatedByFuncSignatureIncremental xs ys = filter ((`elem` nuFuncSignatures) . getFuncSignature) ys
    where nuFuncSignatures = repeated $ map getFuncSignature xs ++ map getFuncSignature ys

-- | Return the elements with non-unique function signatures: 
-- the elements with a 'FuncSignature' that is present more than once in the list.
repeatedByFuncSignature :: (HasFuncSignature a) => [a] -> [a]
repeatedByFuncSignature = repeatedByFuncSignatureIncremental ([] :: [FuncSignature])