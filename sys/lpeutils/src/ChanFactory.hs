{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ChanFactory
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module ChanFactory (
createFreshChan,
createFreshChanFromPrefix,
createFreshChanFromChan,
createFreshChanFromChansAndVars,
createFreshChanFromChansAndSorts
) where

import qualified EnvCore as IOC
import qualified Data.List as List
import qualified Data.Text as Text
import qualified SortId
import qualified SortOf
import qualified ChanId
import qualified VarId
import qualified Id

-- Creates a channel of the specified sorts, using the specified string as part of the name.
createFreshChan :: [SortId.SortId] -> IOC.IOC ChanId.ChanId
createFreshChan = createFreshChanFromPrefix "__FC"

-- Creates a channel of the specified sorts, using the specified string as part of the name.
createFreshChanFromPrefix :: String -> [SortId.SortId] -> IOC.IOC ChanId.ChanId
createFreshChanFromPrefix prefix sorts = do
    chanUnid <- IOC.newUnid
    let idAsInt = Id._id chanUnid
    let absId = if idAsInt >= 0 then idAsInt else -idAsInt
    return ChanId.ChanId { ChanId.name = Text.pack (prefix ++ show absId), ChanId.unid = chanUnid, ChanId.chansorts = sorts }
-- createFreshChanFromPrefix

-- Creates a clone of a given channel.
createFreshChanFromChan :: ChanId.ChanId -> IOC.IOC ChanId.ChanId
createFreshChanFromChan cid = createFreshChanFromPrefix (Text.unpack (ChanId.name cid)) (ChanId.chansorts cid)

-- Creates a channel that is a combination of the specified channels plus the sorts of a number of variables.
createFreshChanFromChansAndVars :: [ChanId.ChanId] -> [VarId.VarId] -> IOC.IOC ChanId.ChanId
createFreshChanFromChansAndVars chans vars = createFreshChanFromChansAndSorts chans (map SortOf.sortOf vars)

-- Creates a channel that is a combination of the specified channels plus a number of a sorts.
createFreshChanFromChansAndSorts :: [ChanId.ChanId] -> [SortId.SortId] -> IOC.IOC ChanId.ChanId
createFreshChanFromChansAndSorts chans extraSorts = do
    let sorts = concatMap ChanId.chansorts chans ++ extraSorts
    createFreshChanFromPrefix (getPrefix chans) sorts
  where
    getPrefix :: [ChanId.ChanId] -> String
    getPrefix [] = "__FC"
    getPrefix xs = List.intercalate "_" (map (Text.unpack . ChanId.name) xs)
-- createFreshChanFromChansAndSorts

