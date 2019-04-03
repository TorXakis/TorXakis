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
, conceptualErrorAddChans
  -- dependencies, yet part of interface
, module TorXakis.SortContext
, ChanDef
)
where
import           TorXakis.Chan
import           TorXakis.Error
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.SortContext

------------------------------------------------------------------------------------------------------------------
-- Context
------------------------------------------------------------------------------------------------------------------

-- | A Channel Context instance contains all definitions to work with sort and channels
class SortContext c => ChanContext c where
    -- | Refers the provided ChanDef name to a ChanDef in the context?
    memberChan :: Name -> c -> Bool
    -- | lookup ChanDef
    lookupChan :: Name -> c -> Maybe ChanDef
    -- | All ChanDef elements in the context
    elemsChan :: c -> [ChanDef]
    -- | Add channels to channel context.
    --   A channel context is returned when the following constraints are satisfied:
    --
    --   * The references of the added channels are distinct
    --
    --   * All sorts are known
    --
    --   Otherwise an error is returned. The error reflects the violations of any of the aforementioned constraints.
    -- Note: added channels might hide previously defined channels.
    -- Note: the order of the channels is not relevant.
    addChans :: [ChanDef] -> c -> Either Error c

-- | Validation function that reports whether a conceptual error will occur
--   when the list of 'ChanDef's would be added to the given context.
--   The conceptual error reflects the violations of any of the following constraints:
--
--   * The 'Name's of the added ChanDef are unique
--
--   * All references are known
conceptualErrorAddChans :: ChanContext c => [ChanDef] -> c -> Maybe Error
conceptualErrorAddChans cs ctx | not $ null nuChanDefs       = Just $ Error ("Non unique variable definitions: " ++ show nuChanDefs)
                               | not $ null undefinedSorts   = Just $ Error ("List of variable definitions with undefined sorts: " ++ show undefinedSorts)
                               | otherwise                   = Nothing
    where
        -- | non unique Channel Definitions (i.e. duplicate names)
        nuChanDefs :: [ChanDef]
        nuChanDefs = repeatedByName cs

        -- | undefined Sorts of Channel Definitions.
        undefinedSorts :: [ChanDef]
        undefinedSorts = filter (not . (all (flip memberSort ctx) . toSorts . chanSort)) cs