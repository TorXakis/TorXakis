-- | Logging functionality for the TorXakis CLI.
module TorXakis.CLI.Log
    (initL, info, warn)
where


import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Time                 (defaultTimeLocale, formatTime,
                                            getCurrentTime)
import           System.Log.Formatter      (tfLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (fileHandler)
import           System.Log.Logger         (Priority (INFO), addHandler, infoM,
                                            removeAllHandlers, setLevel,
                                            updateGlobalLogger, warningM)

appName :: String
appName = "txs-cli"

initL :: MonadIO m => m ()
initL = liftIO $ do
    removeAllHandlers
    time <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y%m%d%H%M%S%3q" time
    lh <- fileHandler ("txs-cli_" ++ timeStr ++ ".log") INFO
    let h = setFormatter lh
           (tfLogFormatter "%Y%m%d_%H%M%S.%3q" "[$time : $loggername : $prio] $msg")
    updateGlobalLogger appName
        (setLevel INFO)
    updateGlobalLogger appName (addHandler h)

info :: MonadIO m => String -> m ()
info = liftIO . infoM appName

warn :: MonadIO m => String -> m ()
warn s = liftIO $ do
    warningM appName s
    putStrLn s

