{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Sigs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- This module introduces the cartesian product of Sigs for the TxsParser.
-----------------------------------------------------------------------------
module Sigs
( Sigs (..)
, Sigs.empty
, combine
)
where

import qualified Data.Map as Map

import ChanId
import ProcId
import SortId

import FuncTable

data Sigs v = Sigs  { chan  :: [ChanId]
                    , func  :: FuncTable v
                    , pro   :: [ProcId]
                    , sort  :: Map.Map String SortId
                    }
    
empty :: Sigs v
empty = Sigs [] FuncTable.empty [] Map.empty

combine :: Sigs v -> Sigs v -> Sigs v
combine l r = Sigs  (chan l ++ chan r)
                    (FuncTable.union (func l) (func r))
                    (pro l ++ pro r)
                    (Map.union (sort l) (sort r))