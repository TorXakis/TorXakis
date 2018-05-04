-- | Logging functionality for the TorXakis CLI.
module TorXakis.CLI.Log
    (initLogger, info, warn)
where


import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (fileHandler)
-- import           System.Log.Handler.Syslog
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           System.Log.Logger         (Priority (INFO), addHandler, infoM,
                                            setLevel, updateGlobalLogger,
                                            warningM)

appName :: String
appName = "txs-cli"

initLogger :: MonadIO m => m ()
initLogger = liftIO $ do
    lh <- fileHandler "txs-cli.log" INFO
    let h = setFormatter lh
           (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger appName
        (setLevel INFO)
    updateGlobalLogger appName (addHandler h)

info :: MonadIO m => String -> m ()
info = liftIO . infoM appName

warn :: MonadIO m => String -> m ()
warn = liftIO . warningM appName

