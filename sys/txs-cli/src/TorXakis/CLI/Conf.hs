{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
-- | TorXakis CLI configuration.
module TorXakis.CLI.Conf
    (defaultConf, prompt, rPrompt)
where

import           Data.Data     (Data)
import           Lens.Micro.TH (makeLenses)

data Conf = Conf
    { -- | Preferred prompt
      _prompt  :: String
      -- | Prefix for the server replies.
    , _rPrompt :: String
    } deriving (Show, Eq, Ord, Data)

makeLenses ''Conf

defaultConf :: Conf
defaultConf = Conf
    { _prompt = ">> "
    , _rPrompt = "<< "
    }
