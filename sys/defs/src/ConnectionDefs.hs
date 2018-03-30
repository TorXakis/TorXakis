{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module ConnectionDefs
where

import           Control.DeepSeq
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

import           ChanId
import           VarEnv
import           VarId

-- | Connection Definitions
data CnectType =  ClientSocket
               | ServerSocket
  deriving (Eq, Ord, Read, Show, Generic, NFData)

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
              | HttpDtoW  { chan     :: ChanId
                          , endpoint :: EndPoint
                          , method   :: Method
                          , var      :: VarId   -- ^ Decoding domain of String
                          , vexprs   :: [VExpr] -- ^ Decoding range
                          }
              | HttpDfroW { chan     :: ChanId
                          , endpoint :: EndPoint
                          , method   :: Method
                          , var      :: VarId   -- ^ Decoding domain of String
                          , vexprs   :: [VExpr] -- ^ Decoding range
                          }
  deriving (Eq, Ord, Read, Show, Generic, NFData)

data Method = Get | Post -- | Put | Delete | Options -- ... See https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods
  deriving (Eq, Ord, Read, Show, Generic, NFData)

newtype EndPoint = EndPoint { toText :: Text}
  deriving (Eq, Ord, Read, Show, Generic, NFData)
