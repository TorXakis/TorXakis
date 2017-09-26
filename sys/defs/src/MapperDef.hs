{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module MapperDef where

import           Control.DeepSeq
import qualified Data.Set        as Set
import           GHC.Generics    (Generic)

import           BehExprDefs
import           ChanId

-- | A mapper maps actions to actions.
--
-- For instance, the process:
--
-- > Plus ? x ? y >->  Sin ! "Plus" ! x ! y ||  Min ? x ? y >->  Sin ! "Minus" ! x ! y ||
--
-- defines a mapper rule that maps `Plus` and `Min` channels to `Sin`.
--
-- See `examps/Adder/MAdder.txs` for more details.
data MapperDef = MapperDef
    { inputChs  :: [ChanId]         -- ^ Input channels.
    , outputChs :: [ChanId]         -- ^ Output channels.
    , syncChs   :: [Set.Set ChanId] -- ^ Synchronization channels.
    , bexpr     :: BExpr            -- ^ Behavior expression.
    } deriving (Eq, Ord, Read, Show, Generic, NFData)
