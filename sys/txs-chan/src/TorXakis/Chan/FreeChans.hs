{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  FreeChans
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Free Channels in Behaviour Expression related functionality.
-----------------------------------------------------------------------------
module TorXakis.Chan.FreeChans
( FreeChans (..)
)
where
import qualified Data.Set               as Set

import TorXakis.Name
import TorXakis.Chan.ChanDef

-- | Class for Free Channels
class FreeChans a where
    -- | Determine the free channels
    freeChans :: a -> Set.Set (RefByName ChanDef)
    -- | Is expression closed?
    -- A closed expression has no free channels.
    isClosed :: a -> Bool
    isClosed = Set.null . freeChans