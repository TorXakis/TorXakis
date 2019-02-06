{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ChanContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Context for Variables.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module TorXakis.ChanContext
( -- * Context
  -- ** Variable Context
  ChanContext (..)
  -- ** Minimal Variable Context
, MinimalChanContext
, fromSortContext
)
where
import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import qualified Data.HashMap    as HashMap
import qualified Data.Text       as T
import           GHC.Generics           (Generic)

import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.Sort (SortContext(..), memberSort)
import           TorXakis.ChanDef

------------------------------------------------------------------------------------------------------------------
-- Context
------------------------------------------------------------------------------------------------------------------

-- | A Variable Context instance contains all definitions to work with sort and channels
class SortContext a => ChanContext a where
    -- | Accessor for defined channels
    chanDefs :: a -> HashMap.Map (RefByName ChanDef) ChanDef

    -- | Add channels to channel context.
    --   A channel context is returned when the following constraints are satisfied:
    --
    --   * The names of the added channels are distinct
    --
    --   * All sorts are known
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    -- Note: added channels might hide previously defined channels.
    -- Note: the order of the channels is not relevant.
    addChanDefs :: a -> [ChanDef] -> Either MinError a

    -- TODO? addVarsDecl function -> don't need to check uniqueness of added channels.

-- | A minimal instance of 'ChanContext'.
data MinimalChanContext a = MinimalChanContext { sortContext :: a
                                               -- channel definitions
                                             , _chanDefs :: HashMap.Map (RefByName ChanDef) ChanDef
                                             } deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Create ChanContext from SortContext
fromSortContext :: a -> MinimalChanContext a
fromSortContext srt = MinimalChanContext srt HashMap.empty

instance SortContext a => SortContext (MinimalChanContext a) where
    empty = MinimalChanContext TorXakis.Sort.empty HashMap.empty
    adtDefs ctx    = adtDefs (sortContext ctx)
    addADTs ctx as = case addADTs (sortContext ctx) as of
                          Left e     -> Left e
                          Right sctx -> Right $ ctx {sortContext = sctx}

instance SortContext a => ChanContext (MinimalChanContext a) where
    chanDefs = _chanDefs
    addChanDefs ctx cs
        | not $ null nuChanDefs              = Left $ MinError (T.pack ("Non unique channel definitions: " ++ show nuChanDefs))
        | not $ null undefinedSorts          = Left $ MinError (T.pack ("List of channel definitions with undefined sorts: " ++ show undefinedSorts))
        | otherwise                          = Right $ ctx { _chanDefs = HashMap.union (toMapByName cs) (chanDefs ctx)}
      where
        nuChanDefs :: [ChanDef]
        nuChanDefs = repeatedByName cs

        undefinedSorts :: [ChanDef]
        undefinedSorts = filter (not . (all (memberSort ctx) . toSorts . chanSort ) ) cs

