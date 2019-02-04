{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMLName
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl> (Embedded Systems Innovation)
--                Damian Nadales <damian.nadalesagut@tno.nl>
--                Kerem Ispirli <kerem.ispirli@tno.nl>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides an name valid for usage with XML.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.XMLName
( 
-- * XMLName
-- ** Name data type
  XMLName
-- ** Name conversion
, toText
-- ** Smart constructor for Name
, mkXMLName
)
where

import           Control.DeepSeq    (NFData)
import           Data.Data          (Data)
import           Data.Hashable      (Hashable(hashWithSalt))
import           Data.Text          (Text)
import qualified Data.Text          as T
import           GHC.Generics       (Generic)

import           TorXakis.Error     (Error(Error))

-- | Definition of the XML name.
newtype XMLName = XMLName
    { -- | 'Data.Text.Text' representation of Name.
      toText :: Text
    }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Hashable XMLName where
    hashWithSalt s = hashWithSalt s . toText

-- | Smart constructor for XMLName.
--
--   A XMLName is returned when the following constraints are satisfied:
--
--   * The provided 'Data.Text.Text' value is non-empty
--
--   * The start character adheres to [A-Z] | \'_\' | [a-z]
--
--   * The remaining characters adhere to [A-Z] | \'_\' | [a-z] | \'-\' | [0-9]
--
--   Otherwise an error is returned. The error reflects the violations of the aforementioned constraints.
--
--   These constraints are enforced by XML.
--   See e.g. http://www.w3.org/TR/REC-XML/#NT-NameStartChar and http://www.w3.org/TR/REC-XML/#NT-NameChar
mkXMLName :: Text -> Either Error XMLName
mkXMLName s = case T.unpack s of
            []     -> Left $ Error "Illegal input: Empty String"
            (x:xs) -> if isNameStartChar x && all isNameChar xs 
                        then Right $ XMLName s
                        else Left $ Error ("String contains illegal characters: " ++ show s)
    where
        isNameStartChar :: Char -> Bool
        isNameStartChar c =      ('A' <= c && c <= 'Z')
                              || ('_' == c)
                              || ('a' <= c && c <= 'z')
        isNameChar :: Char -> Bool
        isNameChar c =     isNameStartChar c 
                        || ('-' == c)
                        || ('0' <= c && c <= '9')
