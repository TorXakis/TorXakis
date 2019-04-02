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
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.FuncSignature.FuncSignature
( -- * Function Signature
  FuncSignature (funcName, args, returnSort)
, mkFuncSignature
  -- * Has Function Signature class
, HasFuncSignature (..)
  -- ** Conversion List to Map By Function Signature
, toMapByFuncSignature
-- ** Repeated Function Signatures functions
, repeatedByFuncSignature
, repeatedByFuncSignatureIncremental
  -- dependencies, yet part of interface
, Error
, FunctionName
, Sort
) where
import           Control.DeepSeq      (NFData)
import           Data.Data            (Data)
import           Data.Hashable        (Hashable(hashWithSalt))
import           Data.HashMap         (Map, fromList)
import           Data.List.Unique     (repeated)
import qualified Data.Set             as Set
import           GHC.Generics         (Generic)

import           TorXakis.Error
import           TorXakis.FunctionName
import           TorXakis.Sort
import           TorXakis.SortContext

-- | A generalized, type-safe reference.
data FuncSignature = FuncSignature { -- | The 'Name' of the function/operator.
                                     funcName :: FunctionName
                                     -- | The 'Sort's of the function arguments.
                                   , args :: [Sort]
                                     -- | The 'Sort' of the result that the function returns.
                                   , returnSort :: Sort
                                   }
    deriving (Eq, Ord, Show, Read, Generic, NFData, Data)


-- | Smart constructor for 'TorXakis.FuncSignature.FuncSignature'.
--   A FuncSignature is returned when the following constraints are satisfied:
--
--   * FuncSignature is not a reserved signature (both default and depending on sort).
--
--   * Sorts of arguments and return value are defined.
--
--   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
mkFuncSignature :: SortContext c => c -> FunctionName -> [Sort] -> Sort -> Either Error FuncSignature
mkFuncSignature ctx n as s | not $ null undefinedSorts              = Left $ Error ("mkPrefixFuncSignature: Arguments have undefined sorts " ++ show undefinedSorts)
                           | memberSort s ctx                       = Right $ FuncSignature n as s
                           | otherwise                              = Left $ Error ("mkPrefixFuncSignature: Return sort has undefined sort " ++ show s)
    where
        undefinedSorts :: [Sort]
        undefinedSorts = filter (not . flip memberSort ctx) as

-- | Enables 'FuncSignature's of entities to be accessed in a common way.
class HasFuncSignature c e where
    -- | return the function signature of the given element
    getFuncSignature :: c -> e -> FuncSignature

instance HasFuncSignature a FuncSignature where
    getFuncSignature _ = id

    -- TODO: used FuncNames can also be keywords/ reserved words

instance UsedSorts c FuncSignature where
    usedSorts _ fs = Set.fromList (returnSort fs : args fs)

instance Hashable FuncSignature where
    s `hashWithSalt` (FuncSignature n as r) = s `hashWithSalt`
                                              n `hashWithSalt`
                                              as `hashWithSalt`
                                              r

-- | Return 'Data.HashMap.Map' where the 'FuncSignature' of the element is taken as key
--   and the element itself is taken as value.
toMapByFuncSignature :: HasFuncSignature c a => c -> [a] -> Map FuncSignature a
toMapByFuncSignature ctx = fromList . map (\e -> (getFuncSignature ctx e,e))

-- |  Return the elements with non-unique function signatures that the second list contains in the combination of the first and second list.
repeatedByFuncSignatureIncremental :: (HasFuncSignature c a, HasFuncSignature c b) => c -> [a] -> [b] -> [b]
repeatedByFuncSignatureIncremental ctx xs ys = filter ((`elem` nuFuncSignatures) . getFuncSignature ctx) ys
    where nuFuncSignatures = repeated $ map (getFuncSignature ctx) xs ++ map (getFuncSignature ctx) ys

-- | Return the elements with non-unique function signatures: 
-- the elements with a 'FuncSignature' that is present more than once in the list.
repeatedByFuncSignature :: (HasFuncSignature c a) => c -> [a] -> [a]
repeatedByFuncSignature ctx = repeatedByFuncSignatureIncremental ctx ([] :: [FuncSignature])