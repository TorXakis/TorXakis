{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module CnectId

where

import           Control.DeepSeq
import           GHC.Generics    (Generic)

import           TorXakis.Sort

data CnectId = CnectId
    { name :: Name
    , unid :: Id
    } deriving (Eq, Ord, Read, Show, Generic, NFData)

instance Resettable CnectId
instance Identifiable CnectId
