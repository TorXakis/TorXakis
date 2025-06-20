{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-- ----------------------------------------------------------------------------------------- --

module Main

-- ----------------------------------------------------------------------------------------- --
--
--   Main Module TorXakis Line-Oriented User Interface Client
--
-- ----------------------------------------------------------------------------------------- --

where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception                hiding (handle)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Foldable
import           Data.List.Utils
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.String.Utils
import           Data.Time
import           GHC.IO.Handle
import           Network.Socket
import           Network.TextViaSockets
import           System.Console.Haskeline
import           System.Console.Haskeline.History
import           Control.Monad.Catch              (handle)
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process

import           ArgsHandling
import           TxsHelp
import           UIenv
import           UIif


-- ----------------------------------------------------------------------------------------- --
-- torxakis ui main

main :: IO ()
main  =  withSocketsDo $ do

    hSetBuffering stdin  NoBuffering     -- alt: LineBuffering
    hSetBuffering stdout NoBuffering     -- alt: LineBuffering
    hSetBuffering stderr NoBuffering     -- alt: LineBuffering

    hSetEncoding stdin latin1
    hSetEncoding stdout latin1
    hSetEncoding stderr latin1

    eUIArgs <- getTxsUIArgs
    either error startUI eUIArgs

    where
      startUI uiArgs = do
          putStrLn "\nTXS >>  TorXakis :: Model-Based Testing\n"
          now <- getCurrentTime
          bracket (initializeServer uiArgs) cleanup $ \(hc, _) -> do
              home <- getHomeDirectory
              _ <- runStateT ( runInputT (txsHaskelineSettings home) $
                               do { doCmd "START" ""
                                  ; doCmd "INIT" $ show (inputFiles uiArgs)
                                  ; cmdsIntprSafe
                                  }
                             )
                            UIenv { uiservh   = hc
                                  , uihins    = [stdin]
                                  , uihout    = stdout
                                  , uisystems = Map.empty
                                  , uitimers  = Map.singleton "global" now
                                  , uiparams  = Map.empty
                                  }
              putStrLn "\nTXS >>  TorXakis :: Model-Based Testing  << End\n"

      initializeServer :: UIArgs -> IO (Handle, Maybe TxsServerInfo)
      initializeServer uiArgs = do
          home <- getHomeDirectory
          logDir <- mkLogDir (home </> ".torxakis")
          connectToServer logDir (txsServerAddress uiArgs)

      mkLogDir :: FilePath -> IO FilePath
      mkLogDir prefix = do
          currTimeDir <- replace ":" "-"  . replace " " "_" . show <$> getZonedTime
          -- Make the directory.
          let logDir = prefix </> "txsui-logs" </> currTimeDir
          createDirectoryIfMissing True logDir
          return logDir

      -- | Shutdown the TorXakis UI properly.
      cleanup :: (Handle, Maybe TxsServerInfo) -> IO ()
      cleanup (h, mTsi) = do
          hClose h
          traverse_ cleanupServer mTsi

      -- | Cleanup the resources associated to the 'txsserver' process started
      -- by the UI.
      cleanupServer :: TxsServerInfo -> IO ()
      cleanupServer tsi = do
          hClose (errHandle tsi)
          cancel (monitorAsync tsi)


-- | Connect to the server. If the port number of the `TxsServerAddress`
-- argument is 'Nothing` then a new 'txsserver' process is started on
-- "localhost". The port to which UI and server connect is determined by
-- looking at some random available port.
--
-- This function returns the handle for reading from the 'txsserver' socket,
-- and maybe a process handle if the server was started in this function.
--
connectToServer :: FilePath -- ^ Log directory.
                -> TxsServerAddress
                -> IO (Handle, Maybe TxsServerInfo)
connectToServer logDir sAddr = do
    (p, mTsi) <- findTxsServer logDir (portId sAddr)
    threadDelay 1000000    -- 1 sec delay on trying to connect
    sock <- connectToSocket (hostName sAddr) (show p)
    hc <- socketToHandle sock ReadWriteMode
    hSetBuffering hc NoBuffering
    hSetEncoding hc latin1
    return (hc, mTsi)

-- | Information associated to the TorXakis server process started by the UI.
data TxsServerInfo = TxsServerInfo
    { -- ^ Server process handle.
      procHandle   :: ProcessHandle
      -- ^ Handle to the standard error file.
    , errHandle    :: Handle
    -- ^ Asynchronous action associated with the server monitor process.
    , monitorAsync :: Async ()
    }

-- | If the argument is Nothing, this function will start the TorXakis server,
-- and return the port that will be used for communication with it. If the
-- argument contains a port number this port number is returned and no process
-- is started (the TorXakis server is assumed to be running).
findTxsServer :: FilePath -> Maybe PortNumber -> IO (PortNumber, Maybe TxsServerInfo)
findTxsServer logDir Nothing = do
    errh <- openFile (logDir </> "txsserver-err.log") ReadWriteMode
    hSetBuffering errh LineBuffering
    outFileH <- openFile (logDir </> "txsserver-out.log") ReadWriteMode
    hSetBuffering outFileH LineBuffering
    (_, Just outh, _, ph) <- createProcess $
        (proc "txsserver" []) { std_out = CreatePipe
                              , std_err = UseHandle errh
                              , delegate_ctlc = False -- The UI handles the Ctrl-C command.
                              }
    -- The TorXakis server will announce its port via the standard input, so we
    -- need to read its answer.
    line <- hGetLine outh
    a <- async $ monitor outh outFileH
    return ( (read line :: PortNumber)
           , Just TxsServerInfo { procHandle = ph
                                 , errHandle = errh
                                 , monitorAsync = a
                                 }
           )
    where
      monitor :: Handle -> Handle -> IO ()
      monitor from to = do
          eLine <- try $ hGetLine from :: IO (Either IOException String)
          case eLine of
              Left _ ->
                  -- The monitor stops monitoring if there is an exception
                  -- thrown.
                  return ()
              Right line -> do
                  putStrLn line
                  hPutStrLn to line
                  monitor from to
findTxsServer _ (Just p) = return (p, Nothing)

-- | TorXakis prompt. For now this is not configurable.
txsPrompt :: String
txsPrompt = "TXS >> "

-- | Construct the Haskeline settings. Currently only the location of the
-- configuration file is determined here.
txsHaskelineSettings :: MonadIO m
                     => FilePath -- ^ Home directory for `TorXakis`.
                     -> Settings m
txsHaskelineSettings txsHome =
    defaultSettings { historyFile = Just $ txsHome </> ".torxakis-hist.txt"
                    -- We add entries to the history ourselves, by using
                    -- 'addHistoryRemovingAllDupes'.
                    , autoAddHistory = False
                    }

-- | Like 'cmdsIntpr' but handling the 'Ctrl-C' key-press. Currently it does
-- nothing but it is left as place-holder in case a more sophisticated handler
-- is needed.
--
cmdsIntprSafe :: UIO ()
cmdsIntprSafe = handle handleCtrlC (withInterrupt cmdsIntpr)
    where
      handleCtrlC :: Interrupt -> UIO ()
      handleCtrlC _ =
          outputStrLn "Received `Ctrl-C`: quitting."


-- | TorXakis UI commands processing.
cmdsIntpr :: UIO ()
cmdsIntpr  =  do
     x <- lift $ gets uihins
     case x of
        (cmdhin:_cmdhins) -> do
                         line <- if cmdhin /= stdin
                                 then liftIO $ hGetLine cmdhin
                                 else filter (/= '\r') . fromMaybe "" <$> getInputLine txsPrompt
                         unless (cmdhin /= stdin || null line) $
                             -- Add the line to the history, removing the duplicates, and trimming
                             -- leading and trailing white-spaces.
                             modifyHistory $ addHistoryRemovingAllDupes (strip line)
                         let (cmd,args1)   = span (/= ' ') (dropWhile (== ' ') line)
                         let (args,redir1) = span (/= '$') (dropWhile (== ' ') args1)
                         let redir         = replace "$<"  " $< "
                                             $ replace "$>"  " $> "
                                             $ replace "$>>" " $= " redir1
                         case words redir of
                               []                      -> do setOut "" WriteMode
                                                             cmdIntpr cmd args
                               ["$<",fin]              -> do setOut "" WriteMode
                                                             args' <- liftIO $ readFile fin
                                                             cmdIntpr cmd $ replace "\n" " " args'
                               ["$>",fout]             -> do setOut fout WriteMode
                                                             cmdIntpr cmd args
                               ["$=",fapp]             -> do setOut fapp AppendMode
                                                             cmdIntpr cmd args
                               ["$<",fin,"$>",fout]    -> do setOut fout WriteMode
                                                             args' <- liftIO $ readFile fin
                                                             cmdIntpr cmd $ replace "\n" " " args'
                               ["$<",fin,"$=",fapp]    -> do setOut fapp AppendMode
                                                             args' <- liftIO $ readFile fin
                                                             cmdIntpr cmd $ replace "\n" " " args'
                               ["$>",fout,"$<",fin]    -> do setOut fout WriteMode
                                                             args' <- liftIO $ readFile fin
                                                             cmdIntpr cmd $ replace "\n" " " args'
                               ["$=",fapp,"$<",fin]    -> do setOut fapp AppendMode
                                                             args' <- liftIO $ readFile fin
                                                             cmdIntpr cmd $ replace "\n" " " args'
                               _                       -> do putErr "wrong IO-redirection in command"
                                                             cmdsIntpr
        _                    -> error "cmdsIntpr failed: error in internal structure"



-- ----------------------------------------------------------------------------------------- --
-- TorXakis individual command processing

cmdIntpr :: String -> String -> UIO ()
cmdIntpr cmdname args  =
     case cmdname of
-- ----------------------------------------------------------------------------------- modus --
         { "quit"          -> cmdQuit       args
         ; "q"             -> cmdQuit       args
         ; "exit"          -> cmdExit       args
         ; "x"             -> cmdExit       args
-- -------------------------------------------------------------------------------- settings --
         ; "help"          -> cmdHelp       args
         ; "h"             -> cmdHelp       args
         ; "?"             -> cmdHelp       args
         ; "info"          -> cmdInfo       args
         ; "i"             -> cmdInfo       args
         ; "param"         -> cmdParam      args
         ; "echo"          -> cmdEcho       args
         ; "#"             -> cmdComment    args
         ; "seed"          -> cmdSeed       args
         ; ""              -> cmdsIntpr
-- ---------------------------------------------------------------------------- run and time --
         ; "delay"         -> cmdDelay      args
         ; "time"          -> cmdTime       args
         ; "timer"         -> cmdTimer      args
         ; "run"           -> cmdRun        args
-- ------------------------------------------------------------------------------------ data --
         ; "var"           -> cmdVar        args
         ; "val"           -> cmdVal        args
         ; "eval"          -> cmdEval       args
         ; "solve"         -> cmdSolve      args
         ; "unisolve"      -> cmdUniSolve   args
-- ----- --------------------------------- ---------------------------------------------modus --
         ; "tester"        -> cmdTester     args
         ; "simulator"     -> cmdSimulator  args
         ; "stepper"       -> cmdStepper    args
         ; "stop"          -> cmdStop       args
-- -------------------------------------------------------------------- test, simulate, step --
         ; "test"          -> cmdTest       args
         ; "sim"           -> cmdSim        args
         ; "step"          -> cmdStep       args
-- --------------------------------------------------------------------------- btree state --
         ; "show"          -> cmdShow       args
         ; "state"         -> cmdState      args
         ; "btree"         -> cmdModel      args
         ; "goto"          -> cmdGoTo       args
         ; "back"          -> cmdBack       args
         ; "path"          -> cmdPath       args
         ; "trace"         -> cmdTrace      args
         ; "menu"          -> cmdMenu       args
         ; "map"           -> cmdMap        args
         ; "ncomp"         -> cmdNComp      args
         ; "lpe"           -> cmdLPE        args
         ; "lpeop"         -> cmdLPEOp      args
         ; "lpeq"          -> cmdLPEQ       args
         ; "merge"         -> cmdMerge      args
-- ---------------------------------------------------------------------------------- system --
         ; "systart"       -> cmdSyStart    args
         ; "systop"        -> cmdSyStop     args
-- ---------------------------------------------------------------------------- unrecognized --
         ; _               -> do putErr $ "unrecognized command `" ++ cmdname ++  "` (enter 'help' for help)"
                                 x <- lift $ gets uihins
                                 case x of
                                    (cmdhin:cmdhins) -> if  cmdhin == stdin
                                                            then    cmdsIntpr
                                                            else do lift $ modify ( \e -> e { uihins = cmdhins } )
                                                                    cmdsIntpr
                                    _                -> error "unrecognized command handling failed"
         }

-- ----------------------------------------------------------------------------------------- --
-- torxakis ui server individual command processing

-- ----------------------------------------------------------------------------------------- --

cmdQuit :: String -> UIO ()
cmdQuit _  =  do
     systems  <- lift $ gets uisystems
     runprocs <- liftIO $ filterM ( fmap isNothing . getProcessExitCode )
                                  ( Map.elems systems )
     _ <- ($) liftIO $ mapM terminateProcess runprocs
     doCmd "QUIT" ""
     return ()

-- ----------------------------------------------------------------------------------------- --

cmdExit :: String -> UIO ()
cmdExit _args  =  do
     x <- lift $ gets uihins
     case x of
        (cmdhin:cmdhins) -> if  cmdhin == stdin
                               then do doCmd "QUIT" ""
                                       return ()
                               else do lift $ modify ( \e -> e { uihins = cmdhins } )
                                       cmdsIntpr
        _                -> error "cmdExit failed"

-- ----------------------------------------------------------------------------------------- --

cmdHelp :: String -> UIO ()
cmdHelp _args  =  do
     putOut helptxt
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdInfo :: String -> UIO ()
cmdInfo _args  =  do
    doCmd "INFO" ""
    cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdParam :: String -> UIO ()
cmdParam args  =  do
     doCmd "PARAM" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdDelay :: String -> UIO ()
cmdDelay args  =
     case words args of
         { [val] -> do let sec = read val ::Int
                       liftIO $ threadDelay $ 1000000 * sec      -- sec seconds delay
                       cmdsIntpr
         ; _     -> do putErr "unknown delay"
                       cmdsIntpr
         }

-- ----------------------------------------------------------------------------------------- --

cmdTime :: String -> UIO ()
cmdTime _args  =  do
     now <- showTime
     putOut now
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTimer :: String -> UIO ()
cmdTimer args  =  do
     let timername = head (words args)
     timertext <- doTimer timername
     putOut timertext
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdEcho :: String -> UIO ()
cmdEcho args  =  do
     putOut args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdComment :: String -> UIO ()
cmdComment _args  =
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSeed :: String -> UIO ()
cmdSeed args  =  do
     doCmd "SEED" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdRun :: String -> UIO ()
cmdRun args  =  do
     let (fin,_rest) = span (/= ' ') (dropWhile (== ' ') args)
     hin        <- liftIO $ openFile fin ReadMode
     liftIO $ hSetBuffering hin NoBuffering
     cmdhins <- lift $ gets uihins
     lift $ modify ( \e -> e { uihins = hin:cmdhins } )
     cmdsIntpr
     liftIO $ hClose hin

-- ----------------------------------------------------------------------------------------- --

cmdShow :: String -> UIO ()
cmdShow args  =  do
     doCmd "SHOW" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdVar :: String -> UIO ()
cmdVar args  =  do
     doCmd "VAR" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdVal :: String -> UIO ()
cmdVal args  =  do
     doCmd "VAL" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdEval :: String -> UIO ()
cmdEval args  =  do
     doCmd "EVAL" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSolve :: String -> UIO ()
cmdSolve args  =  do
     doCmd "SOLVE" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdUniSolve :: String -> UIO ()
cmdUniSolve args  =  do
     doCmd "UNISOLVE" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTester :: String -> UIO ()
cmdTester args  =  do
     doCmd "TESTER" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSimulator :: String -> UIO ()
cmdSimulator args  =  do
     doCmd "SIMULATOR" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStepper :: String -> UIO ()
cmdStepper args  =  do
     doCmd "STEPPER" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStop :: String -> UIO ()
cmdStop args  =  do
     doCmd "STOP" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTest :: String -> UIO ()
cmdTest args  =  do
     doCmd "TEST" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSim :: String -> UIO ()
cmdSim args  =  do
     doCmd "SIM" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStep :: String -> UIO ()
cmdStep args  =  do
     doCmd "STEP" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdState :: String -> UIO ()
cmdState _args  =  do
     doCmd "SHOW" "state"
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdModel :: String -> UIO ()
cmdModel args  =  do
     doCmd "SHOW" $ "model" ++ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdGoTo :: String -> UIO ()
cmdGoTo args  =  do
     doCmd "GOTO" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdBack :: String -> UIO ()
cmdBack args  =  do
     doCmd "GOTO" $ "back" ++ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdPath :: String -> UIO ()
cmdPath args  =  do
     doCmd "PATH" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTrace :: String -> UIO ()
cmdTrace args  =  do
     doCmd "TRACE" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdMenu :: String -> UIO ()
cmdMenu args  =  do
     doCmd "MENU" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdMap :: String -> UIO ()
cmdMap args  =  do
     doCmd "MAP" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdNComp :: String -> UIO ()
cmdNComp args  =  do
     doCmd "NCOMP" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdLPE :: String -> UIO ()
cmdLPE args  =  do
     doCmd "LPE" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdLPEOp :: String -> UIO ()
cmdLPEOp args  =  do
     doCmd "LPEOP" args
     cmdsIntpr
-- ----------------------------------------------------------------------------------------- --

cmdLPEQ :: String -> UIO ()
cmdLPEQ args  =  do
     doCmd "LPEQ" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdMerge :: String -> UIO ()
cmdMerge args  =  do
     doCmd "MERGE" args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSyStart :: String -> UIO ()
cmdSyStart args  =  do
     systems <- lift $ gets uisystems
     case words args of
         { (nm:cmd:args')
             -> if Map.member nm systems
                 then do putErr "system name in use"
                         cmdsIntpr
                 else do x <- liftIO $ createProcess (proc cmd args')
                                                     { std_out = CreatePipe
                                                     , std_in  = CreatePipe
                                                     , std_err = CreatePipe
                                                     }
                         case x of
                            (Just hin, Just hout, Just herr, ph) -> do
                                 liftIO $ hSetBuffering hin  NoBuffering
                                 liftIO $ hSetBuffering hout NoBuffering
                                 liftIO $ hSetBuffering herr NoBuffering
                                 lift $ modify ( \e -> e { uisystems = Map.insert nm ph systems } )
                                 cmdsIntpr
                            _                                    -> error "createProcess failed"
         ; _ -> do putErr "no system to start"
                   cmdsIntpr
         }

-- ----------------------------------------------------------------------------------------- --

cmdSyStop :: String -> UIO ()
cmdSyStop args  =  do
     systems <- lift $ gets uisystems
     case words args of
         { [nm] -> case Map.lookup nm systems of
                   { Nothing -> do putErr "no system to stop"
                                   cmdsIntpr
                   ; Just ph -> do lift $ modify ( \e -> e { uisystems = Map.delete nm systems } )
                                   ec <- liftIO $ getProcessExitCode ph
                                   if isNothing ec
                                     then do liftIO $ terminateProcess ph
                                             cmdsIntpr
                                     else do putErr "system already stopped"
                                             cmdsIntpr
                   }
         ; _    -> do putErr "no system name to stop"
                      cmdsIntpr
         }


-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
