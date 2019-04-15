{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  ProcExit
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Kerem Ispirli <kerem.ispirli@tno.nl>
--                Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the data structure for ProcExit.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TorXakis.ProcSignature.ProcExit
( -- ** Process Exit and functions
  ProcExit (..)
, exitSorts
, HasProcExit(..)
, (<<+>>)
, (<<->>)
) where

import           Control.DeepSeq        (NFData)
import           Data.Data              (Data)
import           Data.Hashable          (Hashable(hashWithSalt))
import qualified Data.Set               as Set
import           GHC.Generics           (Generic)

import           TorXakis.Error
import           TorXakis.Sort

-- | Kind of Exit of Process.
-- In the PhD thesis (On the Design of Extended LOTOS: A Specification Language for Open Distributed Systems) of Ed Brinksma 
-- this type is called the /functionality/ of a behaviour expression (see e.g. page 35).
data  ProcExit      =  NoExit
                     | Exit [Sort]      -- TODO: discuss only EXIT without arguments? See issue https://github.com/TorXakis/TorXakis/issues/475
                     | Hit              -- TODO: * Is concept right? PROCDEF and TESTPROCDEF (or GPROCDEF from Generalized / Goal)
                                        --       * better name? Also MISS is allowed... 
     deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance Hashable ProcExit where
    hashWithSalt s NoExit    = s `hashWithSalt` "NoExit"
    hashWithSalt s (Exit xs) = s `hashWithSalt` "Exit" 
                                 `hashWithSalt` xs
    hashWithSalt s Hit       = s `hashWithSalt` "Hit"

instance UsedSorts c ProcExit where
    usedSorts _ (Exit s)    = Set.fromList s
    usedSorts _ _           = Set.empty

-- | Sorts used in Process Exit
exitSorts :: ProcExit -> [Sort]
exitSorts (Exit xs) = xs
exitSorts _         = []

-- | The expression has Process Exit associated to it.
class HasProcExit ctx e where
    getProcExit :: ctx -> e -> ProcExit

-- | Combine Process Exits for Synchronized actions, sequences and choices (max of Process Exits)
(<<+>>) :: ProcExit -> ProcExit -> Either Error ProcExit
NoExit   <<+>> NoExit                   = Right NoExit
NoExit   <<+>> Exit exs                 = Right $ Exit exs
NoExit   <<+>> Hit                      = Right Hit
Exit exs <<+>> NoExit                   = Right $ Exit exs
Exit exs <<+>> Exit exs' | exs == exs'  = Right $ Exit exs
Exit exs <<+>> Exit exs'                = Left $ Error ("ProcExits do not match (max): Exit " ++ show exs ++ " versus Exit " ++ show exs')
Exit exs <<+>> Hit                      = Left $ Error ("ProcExits do not match (max): Exit " ++ show exs ++ " versus Hit")
Hit      <<+>> NoExit                   = Right Hit
Hit      <<+>> Exit exs                 = Left $ Error ("ProcExits do not match (max): Hit versus Exit " ++ show exs)
Hit      <<+>> Hit                      = Right Hit

-- | Combine Process Exits for parallel (min of Process Exits)
(<<->>) :: ProcExit -> ProcExit -> Either Error ProcExit
NoExit   <<->> NoExit                   = Right NoExit
NoExit   <<->> Exit _                   = Right NoExit
NoExit   <<->> Hit                      = Right NoExit
Exit _   <<->> NoExit                   = Right NoExit
Exit exs <<->> Exit exs' | exs == exs'  = Right $ Exit exs
Exit exs <<->> Exit exs'                = Left $ Error ("ProcExits do not match (min): Exit " ++ show exs ++ " versus Exit " ++ show exs')
Exit exs <<->> Hit                      = Left $ Error ("ProcExits do not match (min): Exit " ++ show exs ++ " versus Hit")
Hit      <<->> NoExit                   = Right NoExit      -- TODO: HIT see https://github.com/TorXakis/TorXakis/issues/809
Hit      <<->> Exit exs                 = Left $ Error ("ProcExits do not match (min): Hit versus Exit " ++ show exs)
Hit      <<->> Hit                      = Right Hit