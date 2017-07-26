{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module GoalId
  
where

import GHC.Generics (Generic)
import Control.DeepSeq

import Name

data GoalId         = GoalId    { name       :: Name
                                , unid       :: Int
                                }
     deriving (Eq,Ord,Read,Show, Generic, NFData)

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

