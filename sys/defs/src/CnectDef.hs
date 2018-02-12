{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module CnectDef
where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Text (Text)

import ConnectionDefs

data  CnectDef       = CnectDef (Maybe Text) CnectType [ConnDef]
     deriving (Eq,Ord,Read,Show, Generic, NFData)


-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

