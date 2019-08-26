{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  IntercalateMap
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module IntercalateMap (
intercalateMap
) where

import qualified Data.List as List

intercalateMap :: [b] -> (a -> [b]) -> [a] -> [b]
intercalateMap s f = List.intercalate s . map f

