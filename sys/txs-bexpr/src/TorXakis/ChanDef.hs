{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  VarDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
-- 
-- Maintainer  :  pierre.vandelaar@tno.nl (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Variable Definition
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
module TorXakis.ChanDef
( ChanDef (..)
, ChanSort (..)
, chanExit
, chanIstep
, chanQstep
, chanHit
, chanMiss
)
where

import           Control.DeepSeq     (NFData)
import           Data.Data           (Data)
import           Data.Hashable       (Hashable(hashWithSalt))
import qualified Data.Text           as T
import           GHC.Generics        (Generic)

import TorXakis.Name
import TorXakis.Sort

newtype ChanSort = ChanSort [Sort]
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance Hashable ChanSort where
    hashWithSalt s (ChanSort xs)    = s `hashWithSalt` xs

-- | Data structure of Channel Definition.
-- TODO: should we have separated constructors for the predefined channels (EXIT, ISTEP, QSTEP, HIT, MISS)?
data ChanDef = ChanDef { -- | Name
                         chanName :: Name
                         -- | ChanSort
                       , chanSort :: ChanSort
                       }
                       deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName ChanDef where
    getName = chanName

-- TODO: should we have separated constructors for the predefined channels (EXIT, ISTEP, QSTEP, HIT, MISS)?
-- | Predefined EXIT channel
chanExit  :: ChanDef
chanExit  = ChanDef exitName  (ChanSort [])     -- EXIT channel can also have other kinds of chanSort
    where exitName = case mkName (T.pack "EXIT") of
                        Right v -> v
                        Left e  -> error ("EXIT should be legal name, yet " ++ show e)
-- | Predefined ISTEP channel
chanIstep :: ChanDef
chanIstep = ChanDef istepName (ChanSort [])
    where istepName = case mkName (T.pack "ISTEP") of
                        Right v -> v
                        Left e  -> error ("ISTEP should be legal name, yet " ++ show e)
-- | Predefined QSTEP channel
chanQstep :: ChanDef
chanQstep = ChanDef qstepName (ChanSort [])
    where qstepName = case mkName (T.pack "QSTEP") of
                        Right v -> v
                        Left e  -> error ("QSTEP should be legal name, yet " ++ show e)
-- | Predefined HIT channel
chanHit   :: ChanDef
chanHit   = ChanDef hitName   (ChanSort [])
    where hitName = case mkName (T.pack "HIT") of
                        Right v -> v
                        Left e  -> error ("HIT should be legal name, yet " ++ show e)
-- | Predefined MISS channel
chanMiss  :: ChanDef
chanMiss  = ChanDef missName  (ChanSort [])
    where missName = case mkName (T.pack "MISS") of
                        Right v -> v
                        Left e  -> error ("MISS should be legal name, yet " ++ show e)