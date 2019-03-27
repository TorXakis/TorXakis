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
module TorXakis.Var.VarDef
( VarDef
, name
, sort
, mkVarDef
  -- dependencies, yet part of interface
, Name
, Sort
)
where

import           Control.DeepSeq      (NFData)
import           Data.Data            (Data)
import           Data.Hashable        (Hashable(hashWithSalt))
import qualified Data.Set             as Set
import qualified Data.Text            as T
import           GHC.Generics         (Generic)

import           TorXakis.Error
import           TorXakis.Language
import           TorXakis.Name
import           TorXakis.PrettyPrint.TorXakis
import           TorXakis.Sort (Sort, HasSort(getSort), UsedSorts(..))
import           TorXakis.SortContext

-- | Data for a variable definition.
data VarDef = VarDef {   -- | Name
                         name :: Name
                         -- | Sort
                     ,   sort :: Sort 
                     }
         deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | smart constructor for VarDef
-- Error is returned when Sort is not defined within context.
mkVarDef :: SortContext c => c -> Name -> Sort -> Either Error VarDef
mkVarDef ctx n s | memberSort s ctx = Right $ VarDef n s
                 | otherwise        = Left $ Error ("Sort not defined in context " ++ show s)

instance Hashable VarDef where
    s `hashWithSalt` (VarDef nm srt) = s `hashWithSalt` nm
                                         `hashWithSalt` srt

instance HasName VarDef where
    getName = name

instance HasSort c VarDef where
    getSort _ = sort

instance UsedSorts c VarDef where
    usedSorts _ = Set.singleton . sort

instance PrettyPrint c VarDef where
    prettyPrint o c v = TxsString (T.concat [ TorXakis.Name.toText (name v)
                                            , T.pack " :: "
                                            , TorXakis.Language.toText (prettyPrint o c (sort v))
                                            ])
