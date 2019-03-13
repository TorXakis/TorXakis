{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  OperatorName
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
module TorXakis.OperatorName
( 
-- * OperatorName
-- ** Name data type
  OperatorName
-- ** Name conversion
, toText
-- ** Smart constructor for Operator Name
, mkOperatorName
)
where

import           Control.DeepSeq    (NFData)
import           Data.Data          (Data)
import           Data.Hashable      (Hashable(hashWithSalt))
import           Data.Text          (Text)
import qualified Data.Text          as T
import           GHC.Generics       (Generic)

import           TorXakis.Error     (Error(Error))

-- | Definition of the Operator name.
newtype OperatorName = OperatorName
    { -- | 'Data.Text.Text' representation of Name.
      toText :: Text
    }
    deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Hashable OperatorName where
    hashWithSalt s = hashWithSalt s . toText

-- | Smart constructor for OperatorName.
--
--   A OperatorName is returned when the following constraints are satisfied:
--
--   * The provided 'Data.Text.Text' value is non-empty
--
--   * All characters adheres to [=+-*/\\^<>|@&%] 
--   TODO: what about the characters !#$?
--
--   Otherwise an error is returned. The error reflects the violations of the aforementioned constraints.
mkOperatorName :: Text -> Either Error OperatorName
mkOperatorName s = case T.unpack s of
            [] -> Left $ Error "Illegal input: Empty String"
            xs -> if all isOperatorChar xs
                        then Right $ OperatorName s
                        else Left $ Error ("String contains illegal characters: " ++ show s)
    where
        isOperatorChar :: Char -> Bool
        isOperatorChar '='  = True
        isOperatorChar '+'  = True
        isOperatorChar '-'  = True
        isOperatorChar '*'  = True
        isOperatorChar '/'  = True
        isOperatorChar '\\' = True
        isOperatorChar '^'  = True
        isOperatorChar '<'  = True
        isOperatorChar '>'  = True
        isOperatorChar '|'  = True
        isOperatorChar '@'  = True
        isOperatorChar '&'  = True
        isOperatorChar '%'  = True
        isOperatorChar _    = False
