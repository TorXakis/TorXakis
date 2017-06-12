{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Main

-- ----------------------------------------------------------------------------------------- --
--
--   Main Module TorXakis Line-Oriented User Interface Client
--
-- ----------------------------------------------------------------------------------------- --

where

import System.IO
import System.Environment
import System.Process
import Network
import Control.Monad.State
import Data.Time

import System.Process
import Control.Concurrent
import Data.String.Utils

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified Data.Text as Text

import TxsHelp
import UIenv
import UIif


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
               hostname <- return $ "localhost"
               portnr   <- return $ ((read (args!!0))::Integer)
               portid   <- return $ PortNumber (fromInteger portnr)
               hc       <- connectTo hostname portid
               hSetBuffering hc NoBuffering
               hSetEncoding hc latin1

               hPutStrLn stderr $ "\nTXS >>  TorXakis :: Model-Based Testing\n"

               now <- getCurrentTime
               runStateT ( do { doCmd "START" $ ""
                              ; doCmd "INIT"  $ unwords (tail args)
                              ; cmdsIntpr
                              }
                         )
                         ( UIenv { uiservh   = hc
                                 , uihins    = [stdin]
                                 , uihout    = stdout
                                 , uisystems = Map.empty
                                 , uitimers  = Map.singleton "global" now 
                                 , uiparams  = Map.empty
                                 }
                         )
               hClose hc
               hPutStrLn stderr $ "\nTXS >>  TorXakis :: Model-Based Testing  << End\n"


-- ----------------------------------------------------------------------------------------- --
-- torxakis ui commands processing

cmdsIntpr :: UIO ()
cmdsIntpr  =  do
     (cmdhin:cmdhins) <- gets uihins
     lift $ hPutStr stderr $ if cmdhin == stdin then "TXS <<  " else ""
     line             <- lift $ hGetLine cmdhin
     (cmd,args1)      <- return $ span (/= ' ') (dropWhile (== ' ') line)
     (args,redir1)    <- return $ span (/= '$') (dropWhile (== ' ') args1)
     redir            <- return $ replace "$<"  " $< "
                                $ replace "$>"  " $> "
                                $ replace "$>>" " $= " $ redir1
     case words redir of
     { []                      -> do setOut "" WriteMode
                                     cmdIntpr cmd args
     ; ("$<":fin:[])           -> do setOut "" WriteMode
                                     args' <- lift $ readFile fin
                                     cmdIntpr cmd $ replace "\n" " " args'
     ; ("$>":fout:[])          -> do setOut fout WriteMode
                                     cmdIntpr cmd args
     ; ("$=":fapp:[])          -> do setOut fapp AppendMode
                                     cmdIntpr cmd args
     ; ("$<":fin:"$>":fout:[]) -> do setOut fout WriteMode
                                     args' <- lift $ readFile fin
                                     cmdIntpr cmd $ replace "\n" " " args'
     ; ("$<":fin:"$=":fapp:[]) -> do setOut fapp AppendMode
                                     args' <- lift $ readFile fin
                                     cmdIntpr cmd $ replace "\n" " " args'
     ; ("$>":fout:"$<":fin:[]) -> do setOut fout WriteMode
                                     args' <- lift $ readFile fin
                                     cmdIntpr cmd $ replace "\n" " " args'
     ; ("$=":fapp:"$<":fin:[]) -> do setOut fapp AppendMode
                                     args' <- lift $ readFile fin
                                     cmdIntpr cmd $ replace "\n" " " args'
     ; _                       -> do putErr $ "wrong IO-redirection in command"
                                     cmdsIntpr
     }


-- ----------------------------------------------------------------------------------------- --
-- TorXakis individual command processing

cmdIntpr :: String -> String -> UIO ()
cmdIntpr cmdname args  =  do
     case cmdname of
-- ----------------------------------------------------------------------------------- modus --
     { "quit"          ->  do cmdQuit       args 
     ; "q"             ->  do cmdQuit       args 
     ; "exit"          ->  do cmdExit       args 
     ; "x"             ->  do cmdExit       args
-- -------------------------------------------------------------------------------- settings --
     ; "help"          ->  do cmdHelp       args
     ; "h"             ->  do cmdHelp       args
     ; "?"             ->  do cmdHelp       args
     ; "info"          ->  do cmdInfo       args
     ; "i"             ->  do cmdInfo       args
     ; "param"         ->  do cmdParam      args
     ; "echo"          ->  do cmdEcho       args
     ; "#"             ->  do cmdComment    args
     ; "seed"          ->  do cmdSeed       args
     ; ""              ->  do cmdsIntpr
-- ---------------------------------------------------------------------------- run and time --
     ; "delay"         ->  do cmdDelay      args
     ; "time"          ->  do cmdTime       args
     ; "timer"         ->  do cmdTimer      args
     ; "run"           ->  do cmdRun        args
-- ------------------------------------------------------------------------------------ data --
     ; "var"           ->  do cmdVar        args
     ; "val"           ->  do cmdVal        args
     ; "eval"          ->  do cmdEval       args
     ; "solve"         ->  do cmdSolve      args
     ; "unisolve"      ->  do cmdUniSolve   args
-- ----- --------------------------------- ---------------------------------------------modus --
     ; "tester"        ->  do cmdTester     args
     ; "simulator"     ->  do cmdSimulator  args
     ; "stepper"       ->  do cmdStepper    args
     ; "stop"          ->  do cmdStop       args
-- -------------------------------------------------------------------- test, simulate, step --
     ; "test"          ->  do cmdTest       args
     ; "sim"           ->  do cmdSim        args
     ; "step"          ->  do cmdStep       args
-- ----------------------------------------------------------------------------- btree state --
     ; "show"          ->  do cmdShow       args
     ; "state"         ->  do cmdState      args
     ; "btree"         ->  do cmdModel      args
     ; "goto"          ->  do cmdGoTo       args
     ; "back"          ->  do cmdBack       args
     ; "path"          ->  do cmdPath       args
     ; "trace"         ->  do cmdTrace      args
     ; "menu"          ->  do cmdMenu       args
     ; "map"           ->  do cmdMap        args
-- ---------------------------------------------------------------------------------- system --
     ; "systart"       ->  do cmdSyStart    args
     ; "systop"        ->  do cmdSyStop     args
-- ---------------------------------------------------------------------------- unrecognized --
     ; otherwise       ->  do putErr $ "???  ('help' for help)"
                              (cmdhin:cmdhins) <- gets uihins
                              if  cmdhin == stdin
                                then do cmdsIntpr
                                else do modify ( \env -> env { uihins = cmdhins } )
                                        cmdsIntpr
     }

-- ----------------------------------------------------------------------------------------- --
-- torxakis ui server individual command processing

-- ----------------------------------------------------------------------------------------- --

cmdQuit :: String -> UIO ()
cmdQuit args  =  do
     systems  <- gets uisystems
     runprocs <- lift $ filterM ( \ph -> do { ec <- getProcessExitCode ph
                                            ; return (ec == Nothing)
                                            }
                                ) ( Map.elems systems )
     lift $ mapM terminateProcess runprocs
     doCmd "QUIT" $ ""
     return $ ()

-- ----------------------------------------------------------------------------------------- --

cmdExit :: String -> UIO ()
cmdExit args  =  do
     (cmdhin:cmdhins) <- gets uihins
     if  cmdhin == stdin
       then do doCmd "QUIT" $ ""
               return $ ()
       else do modify ( \env -> env { uihins = cmdhins } )
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdHelp :: String -> UIO ()
cmdHelp args  =  do
     putOut $ helptxt
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdInfo :: String -> UIO ()
cmdInfo args  =  do
    doCmd "INFO" $ ""
    cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdParam :: String -> UIO ()
cmdParam args  =  do
     doCmd "PARAM" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdDelay :: String -> UIO ()
cmdDelay args  =  do
     case words args of
     { [val] -> do sec <- return $ ( (read val)::Int )
                   lift $ threadDelay $ 1000000 * sec      -- sec seconds delay
                   cmdsIntpr
     ; _     -> do putErr $ "unknown delay"
                   cmdsIntpr
     }

-- ----------------------------------------------------------------------------------------- --

cmdTime :: String -> UIO ()
cmdTime args  =  do
     now <- showTime
     putOut $ now
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTimer :: String -> UIO ()
cmdTimer args  =  do
     timername <- return $ (words args)!!0
     timertext <- doTimer timername
     putOut $ timertext
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdEcho :: String -> UIO ()
cmdEcho args  =  do
     s <- return $ args
     putOut $ s
     cmdsIntpr
              
-- ----------------------------------------------------------------------------------------- --

cmdComment :: String -> UIO ()
cmdComment args  =  do
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSeed :: String -> UIO ()
cmdSeed args  =  do
     doCmd "SEED" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdRun :: String -> UIO ()
cmdRun args  =  do
     (fin,rest) <- return $ span (/= ' ') (dropWhile (== ' ') args)
     hin        <- lift $ openFile fin ReadMode
     lift $ hSetBuffering hin NoBuffering
     cmdhins <- gets uihins
     modify ( \env -> env { uihins = hin:cmdhins } )
     cmdsIntpr
     lift $ hClose hin

-- ----------------------------------------------------------------------------------------- --

cmdShow :: String -> UIO ()
cmdShow args  =  do
     doCmd "SHOW" $ args
     cmdsIntpr
  
-- ----------------------------------------------------------------------------------------- --

cmdVar :: String -> UIO ()
cmdVar args  =  do
     doCmd "VAR" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdVal :: String -> UIO ()
cmdVal args  =  do
     doCmd "VAL" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdEval :: String -> UIO ()
cmdEval args  =  do
     doCmd "EVAL" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSolve :: String -> UIO ()
cmdSolve args  =  do
     doCmd "SOLVE" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdUniSolve :: String -> UIO ()
cmdUniSolve args  =  do
     doCmd "UNISOLVE" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTester :: String -> UIO ()
cmdTester args  =  do
     doCmd "TESTER" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSimulator :: String -> UIO ()
cmdSimulator args  =  do
     doCmd "SIMULATOR" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStepper :: String -> UIO ()
cmdStepper args  =  do
     doCmd "STEPPER" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStop :: String -> UIO ()
cmdStop args  =  do
     doCmd "STOP" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTest :: String -> UIO ()
cmdTest args  =  do
     doCmd "TEST" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSim :: String -> UIO ()
cmdSim args  =  do
     doCmd "SIM" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStep :: String -> UIO ()
cmdStep args  =  do
     doCmd "STEP" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdState :: String -> UIO ()
cmdState args  =  do
     doCmd "SHOW" $ "state"
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdModel :: String -> UIO ()
cmdModel args  =  do
     doCmd "SHOW" $ "model" ++ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdGoTo :: String -> UIO ()
cmdGoTo args  =  do
     doCmd "GOTO" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdBack :: String -> UIO ()
cmdBack args  =  do
     doCmd "GOTO" $ "back" ++ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdPath :: String -> UIO ()
cmdPath args  =  do 
     doCmd "PATH" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTrace :: String -> UIO ()
cmdTrace args  =  do
     doCmd "TRACE" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdMenu :: String -> UIO ()
cmdMenu args  =  do
     doCmd "MENU" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdMap :: String -> UIO ()
cmdMap args  =  do
     doCmd "MAP" $ args
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSyStart :: String -> UIO ()
cmdSyStart args  =  do
     systems <- gets uisystems
     case words args of
     { (nm:cmd:args')
         -> do if Map.member nm systems
                 then do putErr $ "system name in use"
                         cmdsIntpr
                 else do (Just hin, Just hout, Just herr, ph)
                             <- lift $ createProcess (proc cmd args')
                                                     { std_out = CreatePipe
                                                     , std_in  = CreatePipe
                                                     , std_err = CreatePipe
                                                     }
                         lift $ hSetBuffering hin  NoBuffering
                         lift $ hSetBuffering hout NoBuffering
                         lift $ hSetBuffering herr NoBuffering
                         modify ( \env -> env { uisystems = Map.insert nm ph systems } )
                         cmdsIntpr
     ; _ -> do putErr $ "no system to start"
               cmdsIntpr
     }

-- ----------------------------------------------------------------------------------------- --

cmdSyStop :: String -> UIO ()
cmdSyStop args  =  do
     systems <- gets uisystems
     case words args of
     { [nm] -> case Map.lookup nm systems of
               { Nothing -> do putErr $ "no system to stop"
                               cmdsIntpr
               ; Just ph -> do modify ( \env -> env { uisystems = Map.delete nm systems } )
                               ec <- lift $ getProcessExitCode ph
                               if ec == Nothing
                                 then do lift $ terminateProcess ph
                                         cmdsIntpr
                                 else do putErr $ "system already stopped"
                                         cmdsIntpr
               }
     ; _    -> do putErr $ "no system name to stop"
                  cmdsIntpr
     }


-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

