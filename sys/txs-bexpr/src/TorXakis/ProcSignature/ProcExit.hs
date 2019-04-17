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
                     | Hit [Sort]       -- TODO: * Is concept right? PROCDEF and TESTPROCDEF (or GPROCDEF from Generalized / Goal)
                                        --       * better name? Also MISS is allowed... 
                                        -- Can user in case of MISS always provide the necessary data? 
                                        -- Yes, since one can always make an ADT with two constructors: one for HIT and one for MISS!
                                 
     deriving (Eq,Ord,Read,Show, Generic, NFData, Data)

instance Hashable ProcExit where
    hashWithSalt s NoExit    = s `hashWithSalt` "NoExit"
    hashWithSalt s (Exit xs) = s `hashWithSalt` "Exit" 
                                 `hashWithSalt` xs
    hashWithSalt s (Hit xs)  = s `hashWithSalt` "Hit"
                                 `hashWithSalt` xs

instance UsedSorts c ProcExit where
    usedSorts _ (Exit s)    = Set.fromList s
    usedSorts _ (Hit s)     = Set.fromList s
    usedSorts _ _           = Set.empty

-- | Sorts used in Process Exit
exitSorts :: ProcExit -> [Sort]
exitSorts (Exit xs) = xs
exitSorts (Hit xs)  = xs
exitSorts _         = []

-- | The expression has Process Exit associated to it.
class HasProcExit ctx e where
    getProcExit :: ctx -> e -> ProcExit

-- | Combine Process Exits for Synchronized actions, sequences and choices (max of Process Exits)
(<<+>>) :: ProcExit -> ProcExit -> Either Error ProcExit
NoExit  <<+>> NoExit               = Right NoExit
NoExit  <<+>> Exit xs              = Right $ Exit xs
NoExit  <<+>> Hit xs               = Right $ Hit xs
Exit xs <<+>> NoExit               = Right $ Exit xs
Exit xs <<+>> Exit xs' | xs == xs' = Right $ Exit xs
Exit xs <<+>> Exit xs'             = Left $ Error ("ProcExits do not match (max): Exit " ++ show xs ++ " versus Exit " ++ show xs')
Exit xs <<+>> Hit xs'              = Left $ Error ("ProcExits do not match (max): Exit " ++ show xs ++ " versus Hit " ++ show xs')
Hit xs  <<+>> NoExit               = Right $ Hit xs
Hit xs  <<+>> Exit xs'             = Left $ Error ("ProcExits do not match (max): Hit " ++ show xs ++ " versus Exit " ++ show xs')
Hit xs  <<+>> Hit xs' | xs == xs'  = Right $ Hit xs
Hit xs  <<+>> Hit xs'              = Left $ Error ("ProcExits do not match (max): Hit " ++ show xs ++ " versus Hit " ++ show xs')

-- | Combine Process Exits for parallel (min of Process Exits)
(<<->>) :: ProcExit -> ProcExit -> Either Error ProcExit
NoExit  <<->> NoExit                = Right NoExit
NoExit  <<->> Exit _                = Right NoExit
NoExit  <<->> Hit xs                = Right $ Hit xs
Exit _  <<->> NoExit                = Right NoExit
Exit xs <<->> Exit xs' | xs == xs'  = Right $ Exit xs
Exit xs <<->> Exit xs'              = Left $ Error ("ProcExits do not match (min): Exit " ++ show xs ++ " versus Exit " ++ show xs')
Exit xs <<->> Hit xs'               = Left $ Error ("ProcExits do not match (min): Exit " ++ show xs ++ " versus Hit " ++ show xs')
Hit xs  <<->> NoExit                = Right $ Hit xs
Hit xs  <<->> Exit xs'              = Left $ Error ("ProcExits do not match (min): Hit " ++ show xs ++ " versus Exit " ++ show xs') -- TODO: Should it be HIT since termination of a parallel process doesn't `hamper` HITting test purpose.
Hit xs  <<->> Hit xs' | xs == xs'   = Right $ Hit xs
Hit xs  <<->> Hit xs'               = Left $ Error ("ProcExits do not match (min): Hit " ++ show xs ++ " versus Hit " ++ show xs')