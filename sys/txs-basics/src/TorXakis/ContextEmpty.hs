{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.ContextSort
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Instance of Sort Context: ContextSort
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
module TorXakis.ContextEmpty
( -- * Context Empty
  ContextEmpty(..)
)
where
import           Data.Data           (Data)
import           GHC.Generics        (Generic)

import           TorXakis.EmptyContext

-- | An instance of 'TorXakis.EmptyContext'.
data ContextEmpty = ContextEmpty deriving (Eq, Ord, Read, Show, Generic, Data)

instance EmptyContext ContextEmpty
