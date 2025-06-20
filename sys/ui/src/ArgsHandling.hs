{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- | Argument handling functionality for TorXakis.
module ArgsHandling where

import           Network.Socket
import           System.Environment
import           Text.Read

-- | Arguments that can be passed to the UI.
data UIArgs = UIArgs
    { -- | Address where the server can be reached, or 'Nothing' if the user
      -- wants the UI to start it.
      txsServerAddress :: TxsServerAddress
      -- | Input files that will be passed to the server for parsing.
    , inputFiles       :: [String]
    }

-- | Address of the TorXakis server.
data TxsServerAddress = TxsServerAddress
    { -- | Host name of the TorXakis server.
      hostName :: HostName
      -- | Port identifier to connect to the TorXakis server.
    , portId   :: Maybe PortNumber
    }

type Error = String

-- | Read the command line arguments, and parse them. A 'Left errMsg' value is
-- returned if the arguments could not be parsed.
getTxsUIArgs :: IO (Either Error UIArgs)
getTxsUIArgs = do
    args <- getArgs
    let
      uiArgs = UIArgs { txsServerAddress = serverAddress, inputFiles = removePort mPort args }
      serverAddress = TxsServerAddress
          { hostName = "localhost" -- For now only "localhost" connections are supported.
          , portId = mPort
          }
      mPort = getPortId args
    return $ Right uiArgs
    where
      getPortId :: [String] -> Maybe PortNumber
      getPortId []    = Nothing
      getPortId (x:_) = readMaybe x
      
      removePort :: Maybe PortNumber -> [String] -> [String]
      removePort Nothing xs = xs
      removePort (Just _) (_:xs) = xs
      removePort _ _ = error $  "This shouldn't happen: pattern 'Just x' "
                             ++ "indicates that the arguments list contains at "
                             ++ "least one argument."

