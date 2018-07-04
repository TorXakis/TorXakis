{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Sort.SortContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for Sort: all defined sorts and necessary other definitions
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module TorXakis.Sort.SortContext
( HasSorts (..)
, SortContext (..)
)
where
import           Control.DeepSeq
import qualified Data.Map        as Map
import           GHC.Generics    (Generic)

import           TorXakis.Sort

-- | A HasSorts instance contains all definitions to work with sort and reference thereof
class HasSorts a where
    sortDefs :: a -> Map.Map SortId SortDef
    cstrDefs :: a -> Map.Map CstrId CstrDef
    
-- | A Sort Context contains all definitions to work with sort and reference thereof
data SortContext = SortContext { sDefs :: Map.Map SortId SortDef
                               , cDefs :: Map.Map CstrId CstrDef
                               }
                   deriving (Eq,Ord,Read,Show, Generic, NFData)

instance HasSorts SortContext
    where sortDefs = sDefs
          cstrDefs = cDefs