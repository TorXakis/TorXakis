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
, empty
, combine
)
where

import ChanId
import CstrId
import FuncId
import ProcId
import SortId

data Sigs = Sigs    { chan  :: [ChanId]
                    , cstr  :: [CstrId]
                    , func  :: [FuncId]
                    , pro   :: [ProcId]
                    , sort  :: [SortId]
                    }
    deriving (Eq, Ord, Read, Show)
    
empty :: Sigs
empty = Sigs [] [] [] [] []

combine :: Sigs -> Sigs -> Sigs
combine l r = Sigs  (chan l ++ chan r)
                    (cstr l ++ cstr r)
                    (func l ++ func r)
                    (pro l ++ pro r)
                    (sort l ++ sort r)