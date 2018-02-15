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
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Name
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
module Name
( Name
, toText
, name
, nameOf
)
where

import           Control.DeepSeq
import           Data.Data
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics     (Generic)

-- | Definition of names of entities.
newtype Name = Name { toText :: Text -- ^ 'Data.Text'(ual) representation of Name
                    }
    deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

-- | Smart constructor for Name
--
--   Precondition:
--
--   * Name should be non-empty
--
--   Given a 'Data.Text',
--
--   * either an error message indicating violation of precondition
--
--   * or a 'Name' structure containing the 'Data.Text'
--
--   is returned.
name :: Text -> Either Text Name
-- QUESTION: What if we have field with no name?
name s | T.null s = Left $ T.pack "Illegal input: Empty String"
name s            = Right $ Name s

-- | Creates a name from a given 'show'able entity.
nameOf :: Show t => t -> Name
nameOf = Name . T.pack . show

instance IsString Name where
    fromString s =
        case (name . T.pack) s of
            -- QUESTION: I'm not sure this is right. If we declare an instance
            -- of "IsString" I would expect an isomorphism between the literal
            -- strings and the data-type that is an instance of this class...
            Left err -> error $ T.unpack err
            Right n  -> n

