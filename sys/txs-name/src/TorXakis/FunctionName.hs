{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FunctionName
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl> (Embedded Systems Innovation)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides an operator name.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module TorXakis.FunctionName
( 
-- * FunctionName
-- ** Name data type
  FunctionName
-- ** Name conversion
, toText
, toString
-- ** Smart constructor for Operator Name
, mkFunctionName
  -- dependencies, yet part of interface
, Text
)
where

import           Control.DeepSeq    (NFData)
import           Data.Data          (Data)
import           Data.Hashable      (Hashable(hashWithSalt))
import           Data.Text          (Text, unpack)
import           GHC.Generics       (Generic)

import           TorXakis.Language  (satisfyTxsFuncOperator, isTxsReserved)
import           TorXakis.Error     (Error(Error))

-- | Definition of the Operator name.
newtype FunctionName = FunctionName
    { -- | 'Data.Text.Text' representation of FunctionName.
      toText :: Text
    }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | 'Data.String' representation of FunctionName.
toString :: FunctionName -> String
toString = unpack . toText

instance Hashable FunctionName where
    hashWithSalt s = hashWithSalt s . toText

-- | Smart constructor for FunctionName.
--
--   A FunctionName is returned when the following constraints are satisfied:
--
--   * The provided 'Data.Text.Text' value is not a reserved name.
--
--   * The provided 'Data.Text.Text' value is an XmlName, i.e. [A-Za-z_][A-Za-z_0-9-]*, or an OperatorName, i.e. [-+*^=<>%/\\|@&]+.
--     TODO: what about the characters ~#$!? (! and ? have meaning in TorXakis)
--
--   Otherwise an error is returned. The error reflects the violations of the aforementioned constraints.
mkFunctionName :: Text -> Either Error FunctionName
mkFunctionName s | isTxsReserved (unpack s) = Left $ Error ("Provided name is a reserved token: " ++ show s)
                 | otherwise                = if satisfyTxsFuncOperator (unpack s)
                                                then Right $ FunctionName s
                                                else Left $ Error ("String violates regular expression requirement: " ++ show s)
