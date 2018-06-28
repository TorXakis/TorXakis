{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module TorXakis.CLI
    ( startCLI
    , module TorXakis.CLI.Env
    , runCli
    )
where

import           Control.Arrow                    ((|||))
import           Control.Concurrent               (newChan, readChan,
                                                   threadDelay)
import           Control.Concurrent.Async         (async, cancel)
import           Control.Concurrent.STM.TVar      (readTVarIO, writeTVar)
import           Control.Monad                    (forever, unless, void, when)
import           Control.Monad.Except             (runExceptT)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader, ReaderT, ask,
                                                   asks, runReaderT)
import           Control.Monad.STM                (atomically)
import           Control.Monad.Trans              (lift)
import           Data.Aeson                       (eitherDecodeStrict)
import qualified Data.ByteString.Char8            as BS
import           Data.Char                        (toLower)
import           Data.Either                      (isLeft)
import           Data.Either.Utils                (maybeToEither)
import           Data.Foldable                    (traverse_)
import           Data.List.Split                  (splitOn)
import           Data.Maybe                       (fromMaybe)
import           Data.String.Utils                (strip)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Lens.Micro                       ((^.))
import           System.Console.Haskeline
import           System.Console.Haskeline.History (addHistoryRemovingAllDupes)
import           System.Directory                 (doesFileExist,
                                                   getHomeDirectory)
import           System.FilePath                  ((</>))
import           System.IO                        (BufferMode (NoBuffering),
                                                   Handle,
                                                   IOMode (AppendMode, WriteMode),
                                                   hClose, hFlush, hPutStrLn,
                                                   hSetBuffering, openFile)
import           Text.Read                        (readMaybe)

import           EnvData                          (Msg)
import           TxsShow                          (pshow)

import           TorXakis.CLI.Conf
import           TorXakis.CLI.Env
import           TorXakis.CLI.Help
import qualified TorXakis.CLI.Log                 as Log
import           TorXakis.CLI.WebClient

-- | Client monad
newtype CLIM a = CLIM { innerM :: ReaderT Env IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader Env
             , MonadException -- needed for getExternalPrint
             )

runCli :: Env -> CLIM a -> IO a
runCli e clim = runReaderT (innerM clim) e

startCLI :: [FilePath] -> CLIM ()
startCLI modelFiles = do
    home <- liftIO getHomeDirectory
    runInputT (haskelineSettings home) cli
  where
    haskelineSettings home = defaultSettings
        { historyFile = Just $ home </> ".torxakis-hist.txt"
        -- We add entries to the history ourselves, by using
        -- 'addHistoryRemovingAllDupes'.
        , autoAddHistory = False
        }
    cli :: InputT CLIM ()
    cli = do
        Log.info "Starting the main loop..."
        outputStrLn "Welcome to TorXakis!"
        withMessages $ withModelFiles modelFiles $
            withInterrupt $
            handleInterrupt (output Nothing ["Ctrl+C: quitting"]) loop
    loop :: InputT CLIM ()
    loop = do
        minput <- fmap strip <$> getInputLine (defaultConf ^. prompt)
        Log.info $ "Processing input line: " ++ show (fromMaybe "<no input>" minput)
        case minput of
            Nothing -> return ()
            Just "" -> loop
            Just "q" -> return ()
            Just "quit" -> return ()
            Just "exit" -> return ()
            Just "x" -> return ()
            Just "?" -> showHelp
            Just "h" -> showHelp
            Just "help" -> showHelp
            Just input ->
                let strippedInput = strip input
                    (cmdAndArgs, redir) = span (/= '$') strippedInput
                in do
                    mhT <- lift $ asks fOutH
                    mh <- liftIO $ readTVarIO mhT
                    case mh of
                        Nothing -> return ()
                        Just h -> liftIO $ do hClose h
                                              atomically $ writeTVar mhT Nothing
                    (argsFromFile, mToFileH) <- liftIO $ parseRedirs redir
                    liftIO $ atomically $ writeTVar mhT mToFileH
                    modifyHistory $ addHistoryRemovingAllDupes strippedInput
                    dispatch $ cmdAndArgs ++ argsFromFile
                    loop
    parseRedirs :: String -> IO (String, Maybe Handle)
    parseRedirs redir = do
        Log.info $ "Parsing redir: " ++ redir
        let redirs = splitOn "$" redir
            (mArgsFn,mOutIOmode,mOutFn) = foldr parseRedir (Nothing, Nothing, Nothing) redirs
        Log.info $ "Parsed redir: " ++ show (mArgsFn,mOutIOmode,mOutFn)
        args <- case mArgsFn of
            Nothing  -> return ""
            Just fin -> readFile fin
        mh <- case mOutFn of
            Nothing -> return Nothing
            Just fn -> case mOutIOmode of
                Nothing     -> error "Impossible to have an out file and no IOMode set!"
                Just ioMode -> do
                    fh <- openFile fn ioMode -- TODO: Handle other errors
                    hSetBuffering fh NoBuffering
                    return $ Just fh
        return (args, mh)
          where
            parseRedir :: String
                       -> (Maybe String, Maybe IOMode, Maybe String)
                       -> (Maybe String, Maybe IOMode, Maybe String)
            parseRedir ('>':'>':fn) (a,_m,_o) = (              a, Just AppendMode, Just $ strip fn)
            parseRedir ('>':fn)     (a,_m,_o) = (              a, Just  WriteMode, Just $ strip fn)
            parseRedir ('<':fn)     (_a,m, o) = (Just $ strip fn,               m,               o)
            parseRedir _            t         = t
    showHelp :: InputT CLIM ()
    showHelp = do
        outputStrLn helpText
        loop
    dispatch :: String -> InputT CLIM ()
    dispatch inputLine = do
        Log.info $ "Dispatching input: " ++ inputLine
        mhT <- lift $ asks fOutH
        mh <- liftIO $ readTVarIO mhT
        lift (getOutputableResult inputLine) >>= output mh
          where
            getOutputableResult :: String -> CLIM [String]
            getOutputableResult inp =
                let tokens = words inp
                    cmd  = head tokens
                    rest = tail tokens
                in case map toLower cmd of
                    "#"         -> return []
                    "echo"      -> return rest
                    "delay"     -> pretty <$> waitFor rest
                    "i"         -> pretty <$> runExceptT info
                    "info"      -> pretty <$> runExceptT info
                    "l"         -> pretty <$> load rest
                    "load"      -> pretty <$> load rest -- TODO: this will break if the file names contain a space.
                    "param"     -> pretty <$> param rest
                    "run"       -> pretty <$> run rest
                    "simulator" -> pretty <$> simulator rest
                    "sim"       -> pretty <$> sim rest
                    "stepper"   -> pretty <$> subStepper rest
                    "step"      -> pretty <$> subStep rest
                    "tester"    -> pretty <$> tester rest
                    "test"      -> pretty <$> test rest
                    "stop"      -> pretty <$> stopTxs
                    "time"      -> pretty <$> runExceptT getTime
                    "timer"     -> pretty <$> timer rest
                    "val"       -> pretty <$> val rest
                    "var"       -> pretty <$> var rest
                    "eval"      -> pretty <$> eval rest
                    "solve"     -> pretty <$> callSolver "sol" rest
                    "unisolve"  -> pretty <$> callSolver "uni" rest
                    "ransolve"  -> pretty <$> callSolver "ran" rest
                    "lpe"       -> pretty <$> callLpe rest
                    "ncomp"     -> pretty <$> callNComp rest
                    "show"      -> pretty <$> runExceptT (showTxs rest)
                    "menu"      -> pretty <$> menu rest
                    "seed"      -> pretty <$> seed rest
                    "goto"      -> pretty <$> goto rest
                    "back"      -> pretty <$> back rest
                    "path"      -> pretty <$> runExceptT getPath
                    "trace"     -> pretty <$> trace rest
                    _           -> return ["Can't dispatch command: " ++ cmd]
            waitFor :: [String] -> CLIM String
            waitFor [n] = case readMaybe n :: Maybe Int of
                            Nothing -> return $ "Error: " ++ show n ++ " doesn't seem to be an integer."
                            Just s  -> do liftIO $ threadDelay (s * 10 ^ (6 :: Int))
                                          return ""
            waitFor _ = return "Usage: delay <seconds>"
            param :: [String] -> CLIM (Either String String)
            param []    = runExceptT getAllParams
            param [p]   = runExceptT $ getParam p
            param [p,v] = runExceptT $ setParam p v
            param _     = return $ Left "Usage: param [ <parameter> [<value>] ]"
            run :: [String] -> CLIM [String]
            run [filePath] = do
                exists <- liftIO $ doesFileExist filePath
                if exists
                    then do fileContents <- liftIO $ readFile filePath
                            let script = lines fileContents
                            concat <$> mapM getOutputableResult script
                    else return ["File " ++ filePath ++ " does not exist."]
            run _ = return ["Usage: run <file path>"]
            simulator :: [String] -> CLIM (Either String ())
            simulator names
                | length names < 2 || length names > 3 = return $ Left "Usage: simulator <model> [<mapper>] <cnect>"
                | otherwise = startSimulator names
            sim :: [String] -> CLIM (Either String ())
            sim []  = simStep "-1"
            sim [n] = simStep n
            sim _   = return $ Left "Usage: sim [<step count>]"
            -- | Sub-command stepper.
            subStepper :: [String] -> CLIM (Either String ())
            subStepper [mName] = stepper mName
            subStepper _       = return $ Left "This command is not supported yet."
            -- | Sub-command step.
            subStep = step . concat
            tester :: [String] -> CLIM (Either String ())
            tester names
                | length names < 2 || length names > 4 = return $ Left "Usage: tester <model> [<purpose>] [<mapper>] <cnect>"
                | otherwise = startTester names
            test :: [String] -> CLIM (Either String ())
            test = testStep . concat
            timer :: [String] -> CLIM (Either String Text)
            timer [nm] = runExceptT $ callTimer nm
            timer _    = return $ Left "Usage: timer <timer name>"
            val :: [String] -> CLIM (Either String String)
            val [] = runExceptT getVals
            val t  = runExceptT $ createVal $ unwords t
            var :: [String] -> CLIM (Either String String)
            var [] = runExceptT getVars
            var t  = runExceptT $ createVar $ unwords t
            eval :: [String] -> CLIM (Either String String)
            eval [] = return $ Left "Usage: eval <value expression>"
            eval t  = runExceptT $ evaluate $ unwords t
            callSolver :: String -> [String] -> CLIM (Either String String)
            callSolver _   [] = return $ Left "Usage: [uni|ran]solve <value expression>"
            callSolver kind t = runExceptT $ solve kind $ unwords t
            callLpe :: [String] -> CLIM (Either String ())
            callLpe [] = return $ Left "Usage: lpe <model|process>"
            callLpe t  = runExceptT $ lpe $ unwords t
            callNComp :: [String] -> CLIM (Either String ())
            callNComp [] = return $ Left "Usage: ncomp <model>"
            callNComp t  = runExceptT $ ncomp $ unwords t
            menu :: [String] -> CLIM (Either String String)
            menu t = runExceptT $ getMenu $ unwords t
            seed :: [String] -> CLIM (Either String ())
            seed [s] = runExceptT $ setSeed s
            seed _   = return $ Left "Usage: seed <n>"
            goto :: [String] -> CLIM (Either String String)
            goto [st] = case readMaybe st of
                Nothing   -> return $ Left "Usage: goto <state>"
                Just stNr -> runExceptT $ gotoState stNr
            goto _    = return $ Left "Usage: goto <state>"
            back :: [String] -> CLIM (Either String String)
            back []   = runExceptT $ backState 1
            back [st] = case readMaybe st of
                Nothing   -> return $ Left "Usage: back [<count>]"
                Just stNr -> runExceptT $ backState stNr
            back _    = return $ Left "Usage: back [<count>]"
            trace :: [String] -> CLIM (Either String String)
            trace []    = runExceptT $ getTrace ""
            trace [fmt] = runExceptT $ getTrace fmt
            trace _     = return $ Left "Usage: trace [<format>]"
    withMessages :: InputT CLIM () -> InputT CLIM ()
    withMessages action = do
        Log.info "Starting printer async..."
        printer <- getExternalPrint
        ch <- liftIO newChan
        env <- lift ask
        sId <- lift $ asks sessionId
        Log.info $ "Enabling messages for session " ++ show sId ++ "..."
        res <- lift openMessages
        when (isLeft res) (error $ show res)
        producer <- liftIO $ async $
            sseSubscribe env ch $ concat ["sessions/", show sId, "/messages"]
        mhT <- lift $ asks fOutH
        consumer <- liftIO $ async $ forever $ do
            Log.info "Waiting for message..."
            msg <- readChan ch
            Log.info $ "Printing message: " ++ show msg
            mh <- readTVarIO mhT
            traverse_ (outputAndPrint mh printer . ("<< " ++)) $ pretty (asTxsMsg msg)
        Log.info "Triggering action..."
        action `finally` do
            Log.info "Closing messages..."
            _ <- lift closeMessages
            liftIO $ do
                cancel producer
                cancel consumer
          where
            outputAndPrint :: Maybe Handle -> (String -> IO ()) -> String -> IO ()
            outputAndPrint mh prntr s = do
                Log.info $ "Showing message on screen: " ++ s
                prntr s
                -- Log.info $ "Maybe file handle: " ++ show mh
                case mh of
                    Just h  -> do hPutStrLn h s
                                  hFlush h
                    Nothing -> return ()
            asTxsMsg :: BS.ByteString -> Either String Msg
            asTxsMsg msg = do
                msgData <- maybeToEither dataErr $
                    BS.stripPrefix (BS.pack "data:") msg
                eitherDecodeStrict msgData
                    where
                    dataErr = "The message from TorXakis did not contain a \"data:\" field: "
                            ++ show msg
    withModelFiles :: [FilePath] -> InputT CLIM () -> InputT CLIM ()
    withModelFiles mfs action = do
        unless (null mfs) $ void $ lift $
            do Log.info $ "Loading model files: " ++ show mfs
               load mfs
        action

-- | Perform an output action in the @InputT@ monad.
output :: Maybe Handle -> [String] -> InputT CLIM ()
output mh = traverse_ logAndOutput
    where
    logAndOutput s = do
        Log.info $ "Showing output: " ++ s
        case mh of
            Just h  -> liftIO $ do mapM_ (hPutStrLn h) $ lines s -- skip last ending newline
                                   hFlush h
            Nothing -> return ()
        outputStrLn s

-- | Values that can be output in the command line.
class Outputable v where
    -- | Format the value as list of strings, to be printed line by line in the
    -- command line.
    pretty :: v -> [String]

instance Outputable () where
    pretty _ = []

instance Outputable String where
    pretty = pure

instance Outputable [String] where
    pretty = id

instance Outputable Text where
    pretty = pure . T.unpack

instance Outputable Info where
    pretty i = [ "Version: " ++ T.unpack (i ^. version)
               , "Build time: "++ T.unpack (i ^. buildTime)
               ]

instance (Outputable a, Outputable b) => Outputable (Either a b) where
    pretty = pretty ||| pretty

instance Outputable Msg where
    pretty = pure . pshow
