{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Name
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl> (Embedded Systems Innovation)
--                Damian Nadales <damian.nadalesagut@tno.nl>
--                Kerem Ispirli <kerem.ispirli@tno.nl>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a Name for entities.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.Name
( 
-- * Name
-- ** Name data type
  Name
-- ** Name conversion
, toText
-- ** Smart constructor for Name
, mkName
-- * HasName class
, HasName (..)
-- ** Repeated Names functions
, repeatedByName
, repeatedByNameIncremental

)
where

import           Control.DeepSeq (NFData)
import           Data.Data (Data)
import           Data.List.Unique (repeated)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics     (Generic)

import           TorXakis.Error(Error(Error))

-- | Definition of the name of entities.
newtype Name = Name
    { -- | 'Data.Text.Text' representation of Name.
      toText :: Text
    }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Enables 'Name's of entities to be accessed in a common way.
class HasName a where
    getName :: a -> Name

instance HasName Name where
    getName = id

instance (HasName a, HasName b) => HasName (Either a b) where
    getName = either getName getName
    
-- | Smart constructor for Name.
--
--   A Name is returned when the following constraint is satisfied:
--
--   * The provided 'Data.Text.Text' value is non-empty
--
--   Otherwise an error is return. The error reflects the violations of the formentioned constraint.
mkName :: Text -> Either Error Name
mkName s | T.null s = Left $ Error (T.pack "Illegal input: Empty String")
mkName s            = Right $ Name s

-- |  Return the elements with non-unique names that the second list contains in the combination of the first and second list.
repeatedByNameIncremental :: (HasName a, HasName b) => [a] -> [b] -> [b]
repeatedByNameIncremental xs ys = filter ((`elem` nuNames) . getName) ys
    where nuNames = repeated $ map getName xs ++ map getName ys

-- | Return the elements with non-unique names: 
-- the elements with a 'Name' that is present more than once in the list.
repeatedByName :: (HasName a) => [a] -> [a]
repeatedByName = repeatedByNameIncremental ([] :: [Name])
