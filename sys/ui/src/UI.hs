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
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.String.Utils
import           Data.Time
import           Network
import           System.Console.Haskeline
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import           System.Process

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

     args <- getArgs
     if  length args < 1
       then error "Usage: txsui <portnumber> [input file(s)]"
       else do threadDelay 1000000    -- 1 sec delay on trying to connect
               let hostname = "localhost"
               let portnr = read (head args) :: Integer
               let portid = PortNumber (fromInteger portnr)
               hc       <- connectTo hostname portid
               hSetBuffering hc NoBuffering
               hSetEncoding hc latin1

               hPutStrLn stderr "\nTXS >>  TorXakis :: Model-Based Testing\n"

               now <- getCurrentTime
               home <- getHomeDirectory
               _ <- runStateT ( runInputT (txsHaskelineSettings home) $
                                  do { doCmd "START" ""
                                     ; doCmd "INIT"  $ unwords (tail args)
                                     ; cmdsIntpr
                                     }
                              )
                              UIenv { uiservh   = hc
                                    , uihins    = [stdin]
                                    , uihout    = stdout
                                    , uisystems = Map.empty
                                    , uitimers  = Map.singleton "global" now
                                    , uiparams  = Map.empty
                                    }
               hClose hc
               hPutStrLn stderr "\nTXS >>  TorXakis :: Model-Based Testing  << End\n"

-- | TorXakis prompt. For now this is not configurable.
txsPrompt :: String
txsPrompt = "TXS >> "

-- | Construct the Haskeline settings.
txsHaskelineSettings :: MonadIO m
                     => FilePath -- ^ Home directory for `TorXakis`.
                     -> Settings m
txsHaskelineSettings txsHome =
    defaultSettings { historyFile = Just $ txsHome </> ".torxakis-hist.txt" }

-- | TorXakis UI commands processing.
cmdsIntpr :: UIO ()
cmdsIntpr  =  do
     (cmdhin:_cmdhins) <- lift $ gets uihins
     line <- if cmdhin == stdin
             then (filter (/= '\r') . fromMaybe "")
                  <$> getInputLine txsPrompt
             else liftIO $ hGetLine cmdhin
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
-- ---------------------------------------------------------------------------------- system --
         ; "systart"       -> cmdSyStart    args
         ; "systop"        -> cmdSyStop     args
-- ---------------------------------------------------------------------------- unrecognized --
         ; _               -> do putErr $ "unrecognized command `" ++ cmdname ++  "` (enter 'help' for help)"
                                 (cmdhin:cmdhins) <- lift $ gets uihins
                                 if  cmdhin == stdin
                                    then    cmdsIntpr
                                    else do lift $ modify ( \e -> e { uihins = cmdhins } )
                                            cmdsIntpr
         }

-- ----------------------------------------------------------------------------------------- --
-- torxakis ui server individual command processing

-- ----------------------------------------------------------------------------------------- --

cmdQuit :: String -> UIO ()
cmdQuit _args  =  do
     systems  <- lift $ gets uisystems
     runprocs <- liftIO $ filterM ( \ph -> do { ec <- getProcessExitCode ph
                                            ; return (isNothing ec)
                                            }
                                ) ( Map.elems systems )
     _ <- ($) liftIO $ mapM terminateProcess runprocs
     doCmd "QUIT" ""
     return ()

-- ----------------------------------------------------------------------------------------- --

cmdExit :: String -> UIO ()
cmdExit _args  =  do
     (cmdhin:cmdhins) <- lift $ gets uihins
     if  cmdhin == stdin
       then do doCmd "QUIT" ""
               return ()
       else do lift $ modify ( \e -> e { uihins = cmdhins } )
               cmdsIntpr

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

cmdSyStart :: String -> UIO ()
cmdSyStart args  =  do
     systems <- lift $ gets uisystems
     case words args of
         { (nm:cmd:args')
             -> if Map.member nm systems
                 then do putErr "system name in use"
                         cmdsIntpr
                 else do (Just hin, Just hout, Just herr, ph)
                             <- liftIO $ createProcess (proc cmd args')
                                                     { std_out = CreatePipe
                                                     , std_in  = CreatePipe
                                                     , std_err = CreatePipe
                                                     }
                         liftIO $ hSetBuffering hin  NoBuffering
                         liftIO $ hSetBuffering hout NoBuffering
                         liftIO $ hSetBuffering herr NoBuffering
                         lift $ modify ( \e -> e { uisystems = Map.insert nm ph systems } )
                         cmdsIntpr
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
