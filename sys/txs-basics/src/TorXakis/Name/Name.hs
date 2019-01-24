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
module TorXakis.Name.Name
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

import           Control.DeepSeq    (NFData)
import           Data.Data          (Data)
import           Data.Hashable      (Hashable(hashWithSalt))
import           Data.List.Unique   (repeated)
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as T
import           GHC.Generics       (Generic)

import           TorXakis.Error     (MinError(MinError))

-- | Definition of the name of entities.
newtype Name = Name
    { -- | 'Data.Text.Text' representation of Name.
      toText :: Text
    }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Hashable Name where
    hashWithSalt s = hashWithSalt s . toText

-- | Enables 'Name's of entities to be accessed in a common way.
class HasName a where
    getName :: a -> Name

instance HasName Name where
    getName = id

instance (HasName a, HasName b) => HasName (Either a b) where
    getName = either getName getName
    
-- | Smart constructor for Name.
--
--   A Name is returned when the following constraints are satisfied:
--
--   * The provided 'Data.Text.Text' value is non-empty
--
--   * The start character adheres to [A-Z] | \'_\' | [a-z]
--
--   * The remaining characters adhere to [A-Z] | \'_\' | [a-z] | \'-\' | [0-9]
--
--   Otherwise an error is returned. The error reflects the violations of the aforementioned constraints.
--
--   These constraints are enforced to be able to use Names as fields in XML.
--   See e.g. http://www.w3.org/TR/REC-xml/#NT-NameStartChar and http://www.w3.org/TR/REC-xml/#NT-NameChar
mkName :: Text -> Either MinError Name
mkName s = case T.unpack s of
            []     -> Left $ MinError (T.pack "Illegal input: Empty String")
            (x:xs) -> if isNameStartChar x && all isNameChar xs 
                        then Right $ Name s
                        else Left $ MinError (T.pack "String contains illegal characters: " <> s)
    where
        isNameStartChar :: Char -> Bool
        isNameStartChar c =      ('A' <= c && c <= 'Z')
                              || ('_' == c)
                              || ('a' <= c && c <= 'z')
        isNameChar :: Char -> Bool
        isNameChar c =     isNameStartChar c 
                        || ('-' == c)
                        || ('0' <= c && c <= '9')
                              
-- |  Return the elements with non-unique names that the second list contains in the combination of the first and second list.
repeatedByNameIncremental :: (HasName a, HasName b) => [a] -> [b] -> [b]
repeatedByNameIncremental xs ys = filter ((`elem` nuNames) . getName) ys
    where nuNames = repeated $ map getName xs ++ map getName ys

-- | Return the elements with non-unique names: 
-- the elements with a 'Name' that is present more than once in the list.
repeatedByName :: (HasName a) => [a] -> [a]
repeatedByName = repeatedByNameIncremental ([] :: [Name])
