{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module CLI
(
  startCLI
)
where

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (async, cancel)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (ask, asks)
import           Control.Monad.Trans        (lift)
import           Data.Aeson                 (FromJSON, decode)
import           Data.Aeson.Types           (Object, Parser, parseMaybe, (.:))
import           Data.Char                  (toLower)
import           Data.List                  (intercalate)
-- import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)
import           Lens.Micro                 ((^.))
import           System.FilePath            (takeFileName)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text                  as T
import           Network.Wreq
import           System.Console.Haskeline

import           Env

data UploadResult = UploadResult
                            { fileName :: Text
                            , loaded   :: Bool
                            }
    deriving (Generic)
instance FromJSON UploadResult
instance Show UploadResult where
    show r = concat [ show $ fileName r
                    , "\t-> "
                    , if loaded r
                        then "LOADED"
                        else "FAILED" ]

startCLI :: EnvM ()
startCLI = runInputT defaultSettings cli
  where
    cli :: InputT EnvM ()
    cli = do
        printer <- getExternalPrint
        sid <- lift $ asks sessionId
        liftIO $ printer $ "SessionId: " ++ show sid
        liftIO $ printer "Starting printer async..."
        pa <- liftIO $ async $ timer 0 printer
        handleInterrupt (return ())
                        $ withInterrupt loop
        liftIO $ cancel pa
    loop :: InputT EnvM ()
    loop = do
        minput <- getInputLine "% "
        case minput of
            Nothing -> return ()
            Just "" -> loop
            Just "quit" -> return ()
            Just input -> do outputStrLn $ "Input was: " ++ input
                             dispatch input
                             loop
    timer :: Int -> (String -> IO ()) -> IO ()
    timer n p = do
        -- TODO: Connect and wait for SSEs, print whatever comes
        --       use foldGet as in Stepper test in sys\txs-webserver\test\Spec.hs
        --       or in testTorXakisWithEcho example in sys\txs-lib\src\TorXakis\Lib\Examples.hs
        p $ "Printer waiting: " ++ show n
        threadDelay (10 ^ (7 :: Int))
        timer (n+1) p
    dispatch :: String -> InputT EnvM ()
    dispatch inputLine = do
        let tokens = words inputLine
            cmd = head tokens
        case map toLower cmd of
            "info" -> getTxsInfo
            "load" -> loadFile $ tail tokens
            _      -> outputStrLn $ "Unknown command: " ++ cmd
          where
            getTxsInfo :: InputT EnvM () -- todo: split to modules?
            getTxsInfo = do
                host <- lift $ asks txsHost
                let url = host ++ "info"
                r <- (lift . lift) $ get url
                let st = r ^. responseStatus . statusCode
                case st of
                    200 -> -- OK
                        let
                            parseInfo :: Object -> Parser (String, String)
                            parseInfo o = do
                                v  <- o .: "version" :: Parser String
                                bt <- o .: "buildTime"   :: Parser String
                                return (v,bt)
                            info = do infoObj <- decode $ r ^. responseBody
                                      parseMaybe parseInfo infoObj
                        in case info of
                            Nothing     -> throwIO $ TorXakisServerException
                                                (BSL.unpack $ r ^. responseBody)
                                                st url "JSON - Version and BuildTime"
                            Just (v,bt) -> do
                                            outputStrLn $ concat
                                                [ "Torxakis version: ", v
                                                , "\nBuild time: ", bt]
                                            return ()
                    _   -> throwIO $ TorXakisServerException
                                    (BSL.unpack $ r ^. responseBody)
                                    st url "HTTP Status 200"
            loadFile :: [FilePath] -> InputT EnvM ()
            loadFile files = do
                env  <- lift ask
                let fns = map takeFileName files
                    pfs  = zipWith partFile (map T.pack fns) files
                    host = txsHost env
                    sid  = sessionId env
                    url  = concat [host, "sessions/", show sid, "/model"]
                outputStrLn $ show pfs
                r <- (lift . lift) $ put url pfs
                let st = r ^. responseStatus . statusCode
                    body = r ^. responseBody
                case st of
                    202 -> -- Accepted
                        let mResultObjs = decode body :: Maybe [UploadResult]
                        in  case mResultObjs of
                                Nothing  ->
                                    throwIO $ TorXakisServerException
                                        (BSL.unpack body)
                                        st url $
                                        concat ("JSON - States of files to be loaded: " : fns)
                                Just rs -> do
                                    outputStrLn $ intercalate "\n" $ map show rs
                                    return ()
                    _   -> throwIO $ TorXakisServerException
                                    (BSL.unpack body)
                                    st url "HTTP Status 202"
