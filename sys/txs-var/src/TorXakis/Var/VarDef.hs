{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  VarDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Variable Definition
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module TorXakis.Var.VarDef
( VarDef
, name
, sort
, mkVarDef
  -- dependencies, yet part of interface
, Name
, Sort
, module TorXakis.Referable
)
where

import           Control.DeepSeq      (NFData)
import           Data.Data            (Data)
import           Data.Hashable        (Hashable(hashWithSalt))
import           GHC.Generics         (Generic)

import TorXakis.Error
import TorXakis.Name
import TorXakis.Referable
import TorXakis.Sort (Sort, HasSort(getSort))
import TorXakis.SortContext

-- | Data for a variable definition.
-- TODO: should VarDef have additional type to separated different variables (e.g. user defined, channel variables, etc.)
data VarDef = VarDef {   -- | Name
                         name :: Name
                         -- | Sort
                     ,   sort :: Sort 
                     }
         deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

mkVarDef :: SortContext a => a -> Name -> Sort -> Either Error VarDef
mkVarDef ctx n s | memberSort s ctx = Right $ VarDef n s
                 | otherwise        = Left $ Error ("Sort not defined in context " ++ show s)

instance Hashable VarDef where
    hashWithSalt s (VarDef nm srt) = s `hashWithSalt` nm
                                       `hashWithSalt` srt

instance Referable VarDef where
    type Ref VarDef = Name
    toRef = name

instance HasName VarDef where
    getName = name

instance HasSort a VarDef where
    getSort _ = sort            -- we decided not to check that sort is defined.
