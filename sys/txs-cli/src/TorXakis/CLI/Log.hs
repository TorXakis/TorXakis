-- | Logging functionality for the TorXakis CLI.
module TorXakis.CLI.Log
    (initL, info, warn)
where

import           Control.Exception         (catch, throwIO)
import           Control.Monad             (when)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Time                 (defaultTimeLocale, formatTime,
                                            getCurrentTime)
import           Data.Time.LocalTime       (getZonedTime)
import           System.Directory          (createDirectoryIfMissing,
                                            doesFileExist, getHomeDirectory,
                                            removeFile)
import           System.FilePath           ((</>))
import           System.IO.Error           (isDoesNotExistError)
import           System.Log.Formatter      (tfLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (fileHandler)
import           System.Log.Logger         (Priority (INFO), addHandler, infoM,
                                            removeAllHandlers, setLevel,
                                            updateGlobalLogger, warningM)

appName :: String
appName = "txs-cli"

-- | Initialize the home directory for TorXakis by creating it if it doesn't
-- exist. Returns the path to the home directory.
initTxsHomeDir :: IO FilePath
initTxsHomeDir = do
    home <- getHomeDirectory
    let txsDir = home </> ".torxakis"
    createDirectoryIfMissing True txsDir
    return txsDir

initL :: MonadIO m => m ()
initL = liftIO $ do
    removeAllHandlers
    time <- getZonedTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d-%H_%M_%S_%3q" time
    txsHome <- initTxsHomeDir
    lh <- fileHandler (txsHome </> "txs-cli_" ++ timeStr ++ ".log") INFO
    -- | The last logs can be always found at the same location. This is useful
    -- for visualizing the logs using `tail` without having to switch files.
    let latest = txsHome </> "txs-cli-latest.log"
    doesFileExist latest >>= (`when` writeFile latest "")
    lh' <- fileHandler latest INFO
    let formatter = tfLogFormatter "%Y%m%d_%H%M%S.%3q" "[$time : $loggername : $prio] $msg"
        h = setFormatter lh formatter
        h' = setFormatter lh' formatter
    updateGlobalLogger appName (setLevel INFO)
    updateGlobalLogger appName (addHandler h)
    updateGlobalLogger appName (addHandler h')

info :: MonadIO m => String -> m ()
info = liftIO . infoM appName

warn :: MonadIO m => String -> m ()
warn s = liftIO $ do
    warningM appName s
    putStrLn s

