{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ConnectionDefs
-- Copyright   :  (c) Jan Tretmans
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Damian Nadales <damian.nadales@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the definitions needed for connecting TorXakis to the
-- external world, and translating the messages to and from this external
-- world.
-----------------------------------------------------------------------------

module ConnectionDefs where

import           Control.DeepSeq
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

--  Local imports.
import           ChanId
import           VarId
import           VarEnv

-- | Description of how TorXakis connects and interacts with the outside-world
-- (mostly the system-under-test). It connections includes information about:
--
--   * How to start and stop the SUT.
--
--   * Type of connection.
--
--   * mapping from abstract TorXakis channels to the concrete outside-world
--     connections.
data CnectDef = CnectDef
    { eworldCfg     :: EWorldCfg
    , cnectType     :: CnectType
    , cnectMappings :: [ConnDef]
    } deriving (Eq, Ord, Read, Show, Generic, NFData)

-- | Configuration for interacting with the external world.
newtype EWorldCfg = EWorldCfg {
    -- | Command used to start the SUT. It might include the executable name,
    -- along with command line arguments for this executable.
    startSUTCmd :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Generic, NFData)

-- | Type of connection.
data CnectType = ClientSocket -- ^ TorXakis is the client side. This is the
                              -- type of connection typically used for
                              -- connecting TorXakis to an SUT.
               | ServerSocket -- ^ TorXakis is the server side. This type of
                              -- connection allows TorXakis to serve as a
                              -- simulator.
  deriving (Eq, Ord, Read, Show, Generic, NFData)

-- | Definition of the mapping to and from the external world.
--
data  ConnDef = ConnDtoW  { chan     :: ChanId
                          , hostname :: Text
                          , portnr   :: Integer
                          , vars     :: [VarId]  -- ^ Encoding domain
                          , vexpr    :: VExpr    -- ^ Encoding range of String
                          }
              | ConnDfroW { chan     :: ChanId
                          , hostname :: Text
                          , portnr   :: Integer
                          , var      :: VarId   -- ^ Decoding domain of String
                          , vexprs   :: [VExpr] -- ^ Decoding range
                          }
  deriving (Eq, Ord, Read, Show, Generic, NFData)

