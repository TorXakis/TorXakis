{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  SortFactory
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module SortFactory (
getBoolSort,
getIntSort,
getStdSort
) where

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified SortId
import StdTDefs (stdSortTable)

getBoolSort :: SortId.SortId
getBoolSort = getStdSort "Bool"

getIntSort :: SortId.SortId
getIntSort = getStdSort "Int"

getStdSort :: String -> SortId.SortId
getStdSort sortName = Maybe.fromMaybe (error ("Could not find standard sort " ++ sortName ++ "!")) (Map.lookup (Text.pack sortName) stdSortTable)

