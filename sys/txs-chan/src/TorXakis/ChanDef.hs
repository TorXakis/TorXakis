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
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
module TorXakis.ChanDef
( ChanSort
, toSorts
, mkChanSort
, ChanDef (..)
, chanExit
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

import TorXakis.Error
import TorXakis.Name
import TorXakis.Sort

-- | Data structure for Channel Sort: a list of 'TorXakis.Sort's.
newtype ChanSort = ChanSort { toSorts :: [Sort] }
     deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

-- | Smart constructor for ChanSort
mkChanSort :: SortContext a => a -> [Sort] -> Either MinError ChanSort
mkChanSort ctx l | not $ null undefinedSorts = Left $ MinError (T.pack ("Channel has undefined sorts " ++ show undefinedSorts))
                 | otherwise                 = Right $ ChanSort l
    where
        undefinedSorts :: [Sort]
        undefinedSorts = filter (not . elemSort ctx) l

instance Hashable ChanSort where
    hashWithSalt s (ChanSort xs)    = s `hashWithSalt` xs

-- | Data structure of Channel Definition.
-- TODO: should we have separated constructors for the predefined channels (EXIT, QSTEP, HIT, MISS)?
data ChanDef = ChanDef { -- | Name
                         chanName :: Name
                         -- | ChanSort
                       , chanSort :: ChanSort
                       }
                       deriving (Eq, Ord, Read, Show, Generic, NFData, Data)

instance HasName ChanDef where
    getName = chanName

-- TODO: should we have separated constructors for the predefined channels (EXIT, QSTEP, HIT, MISS)?
--       Additional advantage, we can decouple ChanSort from ChanDef!
-- | Predefined EXIT channel
chanExit  :: ChanDef
chanExit  = ChanDef exitName (ChanSort [])    -- EXIT channel can also have other kinds of chanSort
    where exitName = case mkName (T.pack "EXIT") of
                        Right v -> v
                        Left e  -> error ("EXIT should be legal name, yet " ++ show e)

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