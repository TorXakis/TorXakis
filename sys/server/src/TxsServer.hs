{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Main

-- ----------------------------------------------------------------------------------------- --
--
--   Main Module TorXakis as a Socket Service
--
-- ----------------------------------------------------------------------------------------- --

where

import System.IO
import System.Environment
import Network
import Control.Monad.State
import System.Random
import Control.Concurrent
import Control.Exception
import Control.DeepSeq

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map
import qualified Data.Text as Text
 
import BuildInfo
import VersionInfo

import TxsDefs
import TxsDDefs
import TxsUtils
import StdTDefs
import TxsAlex
import TxsHappy
import TxsShow 

import TxsEnv

import CTree
import CTShow
import Primer
import Eval
import Cnect
import FreeVar

import ToProcdef

import Utils
import Test
import Sim
import Step

import SolveDefs
import SMT
import SMTData(SmtEnv(..))
import Solve

import ServerIf

import VarId


-- ----------------------------------------------------------------------------------------- --
-- torxakis server main

main :: IO ()
main  =  withSocketsDo $ do

     hSetBuffering stderr NoBuffering     -- alt: LineBuffering
     hSetEncoding stderr latin1
     
     args <- getArgs
     if  length args /= 1
       then do error "Usage: txsserver <portnumber>"
       else do portnr         <- return $ ((read (args!!0))::Integer)
               portid         <- return $ PortNumber (fromInteger portnr)
               servsock       <- listenOn portid
               (hs,host,port) <- accept servsock
               hSetBuffering hs LineBuffering
               hSetEncoding hs latin1
               
               hPutStrLn stderr $ "\nTXSSERVER >>  Starting  ..... \n"
  
               runStateT cmdsIntpr ( envNone { envhost   = host
                                             , envportnr = portnr
                                             , envservhs = hs
                                             }
                                   )

               threadDelay 1000000    -- 1 sec delay on closing
               sClose servsock
               hPutStrLn stderr $ "\nTXSSERVER >>  Closing  ..... \n"


-- ----------------------------------------------------------------------------------------- --
-- torxakis server commands processing

cmdsIntpr :: IOE ()
cmdsIntpr  =  do
     modus      <- gets envmodus
     (cmd,args) <- getCmd
     case cmd of
-- ----------------------------------------------------------------------------------- modus --
     { "START"     |       isNoning     modus         ->  do cmdStart     args
     ; "START"     | not $ isNoning     modus         ->  do cmdNoop      cmd
     ; "QUIT"      |       True                       ->  do cmdQuit      args  
     ; "INIT"      |       isIdling     modus         ->  do cmdInit      args  
     ; "INIT"      | not $ isIdling     modus         ->  do cmdNoop      cmd
     ; "TERMIT"    |       isNotIdling  modus         ->  do cmdTermit    args
     ; "TERMIT"    | not $ isNotIdling  modus         ->  do cmdNoop      cmd
-- -------------------------------------------------------------------------------- settings --
     ; "INFO"      |       isNotNoning  modus         ->  do cmdInfo      args
     ; "INFO"      | not $ isNotNoning  modus         ->  do cmdNoop      cmd
     ; "PARAM"     |       isNotNoning  modus         ->  do cmdParam     args
     ; "PARAM"     | not $ isNotNoning  modus         ->  do cmdNoop      cmd
     ; "SEED"      |       isNotNoning  modus         ->  do cmdSeed      args
     ; "SEED"      | not $ isNotNoning  modus         ->  do cmdNoop      cmd
-- ------------------------------------------------------------------------------------ data --
     ; "VAR"       |       isNotIdling  modus         ->  do cmdVar       args
     ; "VAR"       | not $ isNotIdling  modus         ->  do cmdNoop      cmd
     ; "VAL"       |       isNotIdling  modus         ->  do cmdVal       args
     ; "VAL"       | not $ isNotIdling  modus         ->  do cmdNoop      cmd
     ; "EVAL"      |       isNotIdling  modus         ->  do cmdEval      args
     ; "EVAL"      | not $ isNotIdling  modus         ->  do cmdNoop      cmd 
     ; "SOLVE"     |       isNotIdling  modus         ->  do cmdSolve     args "sol"
     ; "SOLVE"     | not $ isNotIdling  modus         ->  do cmdNoop      cmd
     ; "UNISOLVE"  |       isNotIdling  modus         ->  do cmdSolve     args "uni"
     ; "UNISOLVE"  | not $ isNotIdling  modus         ->  do cmdNoop      cmd
-- ----- ------------------------------------------------------------------------------ exec --
     ; "TESTER"    |       isIniting    modus         ->  do cmdTester    args
     ; "TESTER"    | not $ isIniting    modus         ->  do cmdNoop      cmd
     ; "SIMULATOR" |       isIniting    modus         ->  do cmdSimulator args
     ; "SIMULATOR" | not $ isIniting    modus         ->  do cmdNoop      cmd
     ; "STEPPER"   |       isIniting    modus         ->  do cmdStepper   args
     ; "STEPPER"   | not $ isIniting    modus         ->  do cmdNoop      cmd
     ; "STOP"      |       isNotIniting modus         ->  do cmdStop      args
     ; "STOP"      | not $ isNotIniting modus         ->  do cmdNoop      cmd
-- -------------------------------------------------------------------- test, simulate, step --
     ; "TEST"      |       isTesting    modus         ->  do cmdTest      args
     ; "TEST"      | not $ isTesting    modus         ->  do cmdNoop      cmd
     ; "SIM"       |       isSimuling   modus         ->  do cmdSim       args
     ; "SIM"       | not $ isSimuling   modus         ->  do cmdNoop      cmd
     ; "STEP"      |       isStepping   modus         ->  do cmdStep      args
     ; "STEP"      | not $ isStepping   modus         ->  do cmdNoop      cmd
-- ----------------------------------------------------------------------------- btree state --
     ; "SHOW"      |       isNotIdling  modus         ->  do cmdShow      args
     ; "SHOW"      | not $ isNotIdling  modus         ->  do cmdNoop      cmd
     ; "GOTO"      |       isNotIniting modus         ->  do cmdGoTo      args
     ; "GOTO"      | not $ isNotIniting modus         ->  do cmdNoop      cmd
     ; "PATH"      |       isNotIniting modus         ->  do cmdPath      args
     ; "PATH"      | not $ isNotIniting modus         ->  do cmdNoop      cmd
     ; "TRACE"     |       isNotIniting modus         ->  do cmdTrace     args
     ; "TRACE"     | not $ isNotIniting modus         ->  do cmdNoop      cmd
     ; "MENU"      |       isNotIniting modus         ->  do cmdMenu      args
     ; "MENU"      | not $ isNotIniting modus         ->  do cmdNoop      cmd
-- --------------------------------------------------------------------------------- unknown --
     ; otherwise                                      ->  do cmdUnknown   cmd
     }


-- ----------------------------------------------------------------------------------------- --
-- torxakis server individual command processing

-- ----------------------------------------------------------------------------------------- --

cmdNoop :: String -> IOE ()                          
cmdNoop cmd  =  do
     fack cmd $ "no action"
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdUnknown :: String -> IOE ()
cmdUnknown cmd  =  do
     nack cmd $ "unknown"
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStart :: String -> IOE ()
cmdStart args  =  do
     modify $ \env -> env { envmodus = Idling }
     host <- gets envhost
     port <- gets envportnr 
     fack "START" $ "txsserver starting:  " ++ (show host) ++ " : " ++ (show port)
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdQuit :: String -> IOE ()
cmdQuit args  =  do
     fack "QUIT" $ "txsserver closing"
     return $ ()

-- ----------------------------------------------------------------------------------------- --

cmdInit :: String -> IOE ()
cmdInit args  =  do
     srctxts            <- lift $ mapM readFile (words args)
     srctxt             <- return $ List.intercalate "\n\n" srctxts
     unid               <- gets envuid
     tdefs              <- gets envtdefs
     params             <- gets envparams
     ((unid',tdefs'),e) <- lift $ catch ( let parsing = txsParser (txsLexer srctxt)
                                           in return $! (show parsing) `deepseq` (parsing,"")
                                        )
                                        ( \e -> return $ ((unid,tdefs),show (e::ErrorCall))
                                        )
     if e /= ""
       then do nack "INIT" $ e
               cmdsIntpr
       else do modify $ \env -> env { envmodus  = Initing
                                    , envuid    = unid'
                                    , envtdefs  = tdefs'
                                    }
               pack "INIT" $ "input files parsed: " ++ (List.intercalate " " (words args))
               smtEnv <- lift $ createSMTEnv cmdZ3 False tdefs' params
               (info,smtEnv') <- lift $ runStateT SMT.openSolver smtEnv
               pack "INIT" $ "smt solver initialized: " ++ info
               (_,smtEnv'') <- lift $ runStateT (SMT.addDefinitions tdefs') smtEnv'
               putSMT "current" smtEnv''
               fack "INIT" $ "txsserver initialized"
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTermit :: String -> IOE ()
cmdTermit args  =  do
     smtEnv <- getSMT "current"
     lift $ runStateT SMT.close smtEnv
     -- TODO: PvdL issue: should not ALL smts be closed!
     pack "TERMIT" $ "smt solver closed"
     env <- get
     modify $ \env -> envIdle { envservhs = envservhs env
                              , envuid    = envuid    env
                              , envvar    = envvar    env
                              , envval    = envval    env
                              , envparams = envparams env
                              }
     fack "TERMIT" $ "model closed"
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdInfo :: String -> IOE ()
cmdInfo args  =  do
     pack "INFO" $ "TorXakis version    : " ++ version
     fack "INFO" $ "Build time          : " ++ BuildInfo.buildTime
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdParam :: String -> IOE ()
cmdParam args  =  do
     case words args of
     { []        -> do params <- gets envparams
                       mapM ( pack "PARAM" ) $ map ( \(nm,(val,_)) -> (nm++" = "++val) )
                                             $ Map.toList params
                       fack "PARAM" $ "all parameters end"
                       cmdsIntpr
     ; [prm]     -> do val <- getParam prm
                       fack "PARAM" $ prm++" = "++val
                       cmdsIntpr
     ; [prm,val] -> do setParam prm val
                       fack "PARAM" $ prm++" = "++val
                       
                       -- update params in SmtSolver as well
                       newParams <- gets envparams
                       smtEnv <- getSMT "current"
                       let smtEnv' = smtEnv { params = newParams }
                       putSMT "current" smtEnv'
                          
                       cmdsIntpr
     ; _         -> do nack "PARAM" $ "unknown param"
                       cmdsIntpr
     }

-- ----------------------------------------------------------------------------------------- --

cmdSeed :: String -> IOE ()
cmdSeed args  =  do
     case words args of
     { [val] -> do seed <- return $ read val
                   lift $ setStdGen(mkStdGen seed)
                   fack "SEED" $ "seed = "++(show seed)
                   cmdsIntpr
     ; _     -> do nack "SEED" $ "unknown seed"
                   cmdsIntpr
     }

-- ----------------------------------------------------------------------------------------- --

cmdVar :: String -> IOE ()
cmdVar args  =  do
     env              <- get
     uid              <- return $ envuid env
     tdefs            <- return $ envtdefs env
     ((uid',vars'),e) <- lift $ catch ( let p = vardeclsParser (  ( Ctdefs $ tdefs )
                                                                : ( Cunid  $ uid + 1 )
                                                                : ( txsLexer args )
                                                               )
                                         in return $! (show p) `deepseq` (p,"")
                                      )
                                      ( \e -> return $ ((uid,[]),(show (e::ErrorCall)))
                                      )
     if  e /= ""
       then do nack "VAR" $ e
               cmdsIntpr
       else do if let newnames = (map VarId.name vars')
                   in    ( newnames `List.intersect` (map VarId.name (envvar env))            == [] )
                      && ( newnames `List.intersect` (map VarId.name (Map.keys (envval env))) == [] )
                 then do modify $ \env -> env { envvar = (envvar env) ++ vars'
                                              , envuid = uid'
                                              }
                         fack "VAR" $ fshow vars'
                         cmdsIntpr
                 else do modify $ \env -> env { envuid = uid' }
                         nack "VAR" $ "double variables"
                         cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdVal :: String -> IOE ()
cmdVal args  =  do
     env              <- get
     uid              <- return $ envuid env
     tdefs            <- return $ envtdefs env
     ((uid',venv'),e) <- lift $ catch ( let p = valdefsParser (  ( Ctdefs  $ tdefs )
                                                               : ( Cvarenv $ [] )
                                                               : ( Cunid   $ uid + 1 )
                                                               : ( txsLexer args )
                                                              )
                                         in return $! (show p) `deepseq` (p,"")
                                      )
                                      ( \e -> return $ ((uid,Map.empty),(show (e::ErrorCall)))
                                      )
     if  e /= ""
       then do nack "VAL" $ e
               cmdsIntpr
       else do if let newnames = map VarId.name (Map.keys venv')
                   in    ( newnames `List.intersect` (map VarId.name (envvar env))            == [] )
                      && ( newnames `List.intersect` (map VarId.name (Map.keys (envval env))) == [] )
                 then do modify $ \env -> env { envval = (envval env) `Map.union`  venv'
                                              , envuid = uid'
                                              }
                         fack "VAL" $ fshow venv'
                         cmdsIntpr
                 else do modify $ \env -> env { envuid = uid' }
                         nack "VAL" $ "double value names"
                         cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdEval :: String -> IOE ()
cmdEval args  =  do
     env              <- get
     uid              <- return $ envuid env
     tdefs            <- return $ envtdefs env
     ((uid',vexp'),e) <- lift $ catch ( let p = vexprParser (  ( Ctdefs  $ tdefs )
                                                             : ( Cvarenv $ Map.keys(envval env))
                                                             : ( Cunid   $ uid + 1 )
                                                             : ( txsLexer args )
                                                            )
                                         in return $! (show p) `deepseq` (p,"")
                                      )
                                      ( \e -> return $ ((uid,cstrError ""),(show (e::ErrorCall)))
                                      )
     if  e /= ""
       then do nack "EVAL" $ e
               cmdsIntpr
       else let vexp'' = cstrEnv (envval env) vexp'
             in if isClosed vexp''
                  then do svexp <- eval vexp''
                          modify $ \env -> env { envuid = uid' }
                          fack "EVAL" $ fshow svexp
                          cmdsIntpr
                  else do modify $ \env -> env { envuid = uid' }
                          nack "EVAL" $ "value expression not closed"
                          cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSolve :: String -> String -> IOE ()
cmdSolve args kind  =  do
     cmd              <- return $ if kind=="uni" then "UNISOLVE" else "SOLVE"
     env              <- get
     uid              <- return $ envuid env
     tdefs            <- return $ envtdefs env
     ((uid',vexp'),e) <- lift $ catch ( let p = vexprParser (  ( Ctdefs $ tdefs )
                                                             : ( Cvarenv $ (Map.keys(envval env)
                                                                             ) ++ (envvar env))
                                                             : ( Cunid $ uid + 1 )
                                                             : ( txsLexer args )
                                                            )
                                         in return $! (show p) `deepseq` (p,"")
                                      )
                                      ( \e -> return $ ((uid,cstrError ""),(show (e::ErrorCall)))
                                      )
     if  e /= ""
       then do nack cmd $ e
               cmdsIntpr
       else let vexp'' = cstrEnv (envval env) vexp'
             in if  sortOf vexp'' == sortId_Bool
                  then do frees  <- return $ freeVars vexp''
                          solver <- return $ case kind of
                                             { "sol" -> solve
                                             ; "uni" -> uniSolve
                                             }
                          assertions <- return $ add vexp'' Solve.empty
                          smtEnv <- getSMT "current"
                          (sat,smtEnv') <- lift $ runStateT (solver frees assertions) smtEnv
                          putSMT "current" smtEnv'
                          case sat of
                          { Solved sol    -> do modify $ \env -> env { envuid = uid' }
                                                pack cmd $ "sat"
                                                pack cmd $ fshow sol
                                                fack cmd $ "solved"
                                                cmdsIntpr
                          ; Unsolvable    -> do modify $ \env -> env { envuid = uid' }
                                                fack cmd $ "unsat"
                                                cmdsIntpr
                          ; UnableToSolve -> do modify $ \env -> env { envuid = uid' }
                                                fack cmd $ "unknown"
                                                cmdsIntpr
                          }
                  else do modify $ \env -> env { envuid = uid' }
                          nack cmd $ "invalid value expression for (uni)solve"
                          cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTester :: String -> IOE ()
cmdTester args  =  do
     tdefs  <- gets envtdefs
     let tdefs' = TxsDefs.toList tdefs
     case words args of
     { [m,c]     -> do mdefs <- return $ [ mdef
                                         | mdef@(IdModel (ModelId nm uid), modeldef) <- tdefs'
                                         , nm == m
                                         ]
                       cdefs <- return $ [ cdef
                                         | cdef@(IdCnect (CnectId nm uid), cnectdef) <- tdefs'
                                         , nm == c
                                         ]
                       case (mdefs,cdefs) of
                       { ([mdef],[cdef]) -> cmdTesterMC mdef cdef
                       ; _               -> do nack "TESTER" $ "wrong parameters : [" ++ m ++ " , " ++ c ++ "]"
                                               cmdsIntpr
                       }
     ; [m,p,c]   -> do mdefs <- return $ [ mdef
                                         | mdef@(IdModel (ModelId nm uid), modeldef) <- tdefs'
                                         , nm == m
                                         ]
                       pdefs <- return $ [ pdef
                                         | pdef@(IdPurp (PurpId nm uid), purpdef) <- tdefs'
                                         , nm == p
                                         ]
                       adefs <- return $ [ adef
                                         | adef@(IdMapper (MapperId nm uid), mapperdef) <- tdefs'
                                         , nm == p
                                         ]
                       cdefs <- return $ [ cdef
                                         | cdef@(IdCnect (CnectId nm uid), cnectdef) <- tdefs'
                                         , nm == c
                                         ]
                       case (mdefs,pdefs,adefs,cdefs) of
                       { ([mdef],[pdef],[],[cdef]) -> cmdTesterMPC mdef pdef cdef
                       ; ([mdef],[],[adef],[cdef]) -> cmdTesterMAC mdef adef cdef
                       ; _                         -> do nack "TESTER" $ "wrong parameters : [" ++ m ++ " , " ++ p ++ " , " ++ c ++ "]"
                                                         cmdsIntpr
                       }
     ; [m,p,a,c] -> do mdefs <- return $ [ mdef
                                         | mdef@(IdModel (ModelId nm uid), modeldef) <- tdefs'
                                         , nm == m
                                         ]
                       pdefs <- return $ [ pdef
                                         | pdef@(IdPurp (PurpId nm uid), purpdef) <- tdefs'
                                         , nm == p
                                         ]
                       adefs <- return $ [ adef
                                         | adef@(IdMapper (MapperId nm uid), mapperdef) <- tdefs'
                                         , nm == a
                                         ]
                       cdefs <- return $ [ cdef
                                         | cdef@(IdCnect (CnectId nm uid), cnectdef) <- tdefs'
                                         , nm == c
                                         ]
                       case (mdefs,pdefs,adefs,cdefs) of
                       { ([mdef],[pdef],[adef],[cdef]) -> cmdTesterMPAC mdef pdef adef cdef
                       ; _                             -> do nack "TESTER" $ "wrong parameters  : [" ++ m ++ " , " ++ p ++ " , " ++ a ++ " , " ++ c ++ "]"
                                                             cmdsIntpr
                       }
     ; _         -> do nack "TESTER" $ "wrong parameters : " ++ args
                       cmdsIntpr
     }

    
cmdTesterMC :: (Ident,TxsDef) -> (Ident,TxsDef) -> IOE ()
cmdTesterMC (IdModel modelid,modeldef) (IdCnect cnectid,cnectdef)  =  do
     if  consistentTester modeldef cnectdef
       then do modify $ \env -> env { envmodus  = Testing modelid cnectid }
               openCnect
               initBTree
               fack "TESTER" $ "tester started"
               cmdsIntpr
       else do nack "TESTER" $ "model and cnect not consistent"
               cmdsIntpr


cmdTesterMPC :: (Ident,TxsDef) -> (Ident,TxsDef) -> (Ident,TxsDef) -> IOE ()
cmdTesterMPC mdef pdef cdef  =  do
     nack "TESTER" $ "test purposes not implemented yet"
     cmdsIntpr


cmdTesterMAC :: (Ident,TxsDef) -> (Ident,TxsDef) -> (Ident,TxsDef) -> IOE ()
cmdTesterMAC mdef adef cdef  =  do
     nack "TESTER" $ "mappers not implemented yet"
     cmdsIntpr


cmdTesterMPAC :: (Ident,TxsDef) -> (Ident,TxsDef) -> (Ident,TxsDef) -> (Ident,TxsDef) -> IOE ()
cmdTesterMPAC mdef pdef adef cdef  =  do
     nack "TESTER" $ "test purposes with mappers not implemented yet"
     cmdsIntpr


-- ----------------------------------------------------------------------------------------- --

cmdSimulator :: String -> IOE ()
cmdSimulator args  =  do
     tdefs  <- gets envtdefs
     let tdefs' = TxsDefs.toList tdefs
     case words args of
     { [m,c]     -> do mdefs <- return $ [ mdef
                                         | mdef@(IdModel (ModelId nm uid), modeldef) <- tdefs'
                                         , nm == m
                                         ]
                       cdefs <- return $ [ cdef
                                         | cdef@(IdCnect (CnectId nm uid), cnectdef) <- tdefs'
                                         , nm == c
                                         ]
                       case (mdefs,cdefs) of
                       { ([mdef],[cdef]) -> cmdSimulatorMC mdef cdef
                       ; _               -> do nack "SIMULATOR" $ "wrong parameters"
                                               cmdsIntpr
                       }
     ; [m,a,c]   -> do mdefs <- return $ [ mdef
                                         | mdef@(IdModel (ModelId nm uid), modeldef) <- tdefs'
                                         , nm == m
                                         ]
                       adefs <- return $ [ adef
                                         | adef@(IdMapper (MapperId nm uid), mapperdef) <- tdefs'
                                         , nm == a
                                         ]
                       cdefs <- return $ [ cdef
                                         | cdef@(IdCnect (CnectId nm uid), cnectdef) <- tdefs'
                                         , nm == c
                                         ]
                       case (mdefs,adefs,cdefs) of
                       { ([mdef],[adef],[cdef]) -> cmdSimulatorMAC mdef adef cdef
                       ; _                      -> do nack "SIMULATOR" $ "wrong parameters"
                                                      cmdsIntpr
                       }
     ; _         -> do nack "SIMULATOR" $ "wrong parameters"
                       cmdsIntpr
     }

    
cmdSimulatorMC :: (Ident,TxsDef) -> (Ident,TxsDef) -> IOE ()
cmdSimulatorMC (IdModel modelid,modeldef) (IdCnect cnectid,cnectdef)  =  do
     if  consistentSimulator modeldef cnectdef
       then do sutdeltatime <- getParam "param_Sut_deltaTime"
               simdeltatime <- getParam "param_Sim_deltaTime"
               setParam "param_Sut_deltaTime" simdeltatime
               setParam "param_Sim_deltaTime" sutdeltatime
               modify $ \env -> env { envmodus  = Simuling modelid cnectid }
               openCnect
               initBTree
               fack "SIMULATOR" $ "simulator started"
               cmdsIntpr
       else do nack "SIMULATOR" $ "model and cnect not consistent"
               cmdsIntpr


cmdSimulatorMAC :: (Ident,TxsDef) -> (Ident,TxsDef) -> (Ident,TxsDef) -> IOE ()
cmdSimulatorMAC mdef adef cdef  =  do
     nack "SIMULATOR" $ "mappers not implemented yet"
     cmdsIntpr


-- ----------------------------------------------------------------------------------------- --

cmdStepper :: String -> IOE ()
cmdStepper args  =  do
     tdefs  <- gets envtdefs
     let tdefs' = TxsDefs.toList tdefs
     case words args of
     { [m]       -> do mdefs <- return $ [ mdef
                                         | mdef@(IdModel (ModelId nm uid), modeldef) <- tdefs'
                                         , nm == m
                                         ]
                       case (mdefs) of
                       { ([mdef]) -> cmdStepperM mdef 
                       ; _        -> do nack "STEPPER" $ "wrong parameters"
                                        cmdsIntpr
                       }
     ; _         -> do nack "STEPPER" $ "wrong parameters"
                       cmdsIntpr
     }


cmdStepperM :: (Ident,TxsDef) -> IOE ()
cmdStepperM (IdModel modelid,modeldef)  =  do
     if  consistentStepper modeldef
       then do modify $ \env -> env { envmodus  = Stepping modelid }
               initBTree
               fack "STEPPER" $ "stepper started"
               cmdsIntpr
       else do nack "STEPPER" $ "model not consistent"
               cmdsIntpr
     
-- ----------------------------------------------------------------------------------------- --

cmdStop :: String -> IOE ()
cmdStop args  =  do
     closeCnect
     modus <- gets envmodus
     if  isSimuling modus 
       then do sutdeltatime <- getParam "param_Sut_deltaTime"
               simdeltatime <- getParam "param_Sim_deltaTime"
               setParam "param_Sut_deltaTime" simdeltatime
               setParam "param_Sim_deltaTime" sutdeltatime
               modify $ \env -> env { envmodus   = Initing
                                    , envinit    = (-1)
                                    , envnrst    = 0
                                    , envcurs    = (-1)
                                    , envtrie    = []
                                    , envs2bt    = Map.empty
                                    , envmapper  = []
                                    , envtow     = ( Nothing, Nothing, [] )
                                    , envfrow    = ( Nothing, [],      [] )
                                    }
               fack "STOP" $ "simulation/testing/stepping stopped"
               cmdsIntpr
       else do modify $ \env -> env { envmodus   = Initing
                                    , envinit    = (-1)
                                    , envnrst    = 0
                                    , envcurs    = (-1)
                                    , envtrie    = []
                                    , envs2bt    = Map.empty
                                    , envmapper  = []
                                    , envtow     = ( Nothing, Nothing, [] )
                                    , envfrow    = ( Nothing, [],      [] )
                                    }
               fack "STOP" $ "simulation/testing/stepping stopped"
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTest :: String -> IOE ()
cmdTest args  =  do
     case words args of
     { []  -> do verdict <- testOut 1                                -- no arg: observe output
                 fack "TEST" $ fshow verdict
                 cmdsIntpr
     ; [d] | and (map Char.isDigit d)
           -> do verdict <- testN (read d)  1                        -- d::int test steps
                 fack "TEST" $ fshow verdict
                 cmdsIntpr
     ; _   -> do Testing modid _ <- gets envmodus                    -- action arg: give input
                 tdefs <- gets envtdefs
                 chids <- case Map.lookup modid (modelDefs tdefs) of
                          { Just (ModelDef chinsets choutsets _ _)
                              -> do return $ Set.toList $ Set.unions $ chinsets ++ choutsets
                          ; _ -> do mack "TEST" $ "no model"
                                    return $ []
                          }
                 act <- readAction chids args
                 case act of
                 { Left s           -> do nack "TEST" $ s
                                          cmdsIntpr
                 ; Right (Act acts) -> do verdict <- testIn (Act acts) 1   -- try to give input
                                          fack "TEST" $ fshow verdict
                                          cmdsIntpr
                 }
     }

-- ----------------------------------------------------------------------------------------- --

cmdSim :: String -> IOE ()
cmdSim args  =  do
     case words args of
     { []  -> do verdict <- simN (-1) 1                              -- no arg: infinite sim
                 fack "SIM" $ fshow verdict
                 cmdsIntpr
     ; [d] | and (map Char.isDigit d)                                -- d::int sim steps
           -> do verdict <- simN (read d) 1
                 fack "SIM" $ fshow verdict
                 cmdsIntpr
     ; _   -> do nack "SIM" "wrong parameter"
                 cmdsIntpr
     }

-- ----------------------------------------------------------------------------------------- --

cmdStep :: String -> IOE ()
cmdStep args  =  do
     case words args of
     { []  -> do verdict <- stepN 1 1                                -- no arg: one step
                 fack "STEP" $ fshow verdict
                 cmdsIntpr
     ; [d] | and (map Char.isDigit d)                                -- d::int steps
           -> do verdict <- stepN (read d) 1
                 fack "STEP" $ fshow verdict
                 cmdsIntpr
     ; _   -> do Stepping modid <- gets envmodus                     -- action arg: step action
                 tdefs    <- gets envtdefs
                 chids    <- case Map.lookup modid (modelDefs tdefs) of
                             { Just (ModelDef chinsets choutsets _ _)
                                 -> do return $ Set.toList $ Set.unions $ chinsets ++ choutsets
                             ; _ -> do mack "STEP" $ "no model"
                                       return $ []
                             }
                 act <- readAction chids args
                 case act of
                 { Left s    -> do nack "STEP" $ s
                                   cmdsIntpr
                 ; Right act -> do done <- doAfter act
                                   if  done
                                     then do fack "STEP" $ fshow act
                                             cmdsIntpr
                                     else do nack "STEP" $ "impossible step"
                                             cmdsIntpr
                 }
     }

-- ----------------------------------------------------------------------------------------- --

cmdShow :: String -> IOE ()
cmdShow args  =  do
     env <- get
     txt <- return $ case words args of
                     { []         -> Just $ show (envtdefs  env)
                     ; ["tdefs"]  -> Just $ show (envtdefs  env)
                     ; ["state"]  -> Just $ fshow (envcurs   env)
                     ; ["model"]  -> do statenr <- return $ envcurs env
                                        case Map.lookup statenr (envs2bt env) of 
                                        { Nothing -> Nothing
                                        ; Just bt -> Just $ fshow bt 
                                        }
                     ; ["model",d] | and (map Char.isDigit d)
                                  -> do statenr <- return $ read d
                                        case Map.lookup statenr (envs2bt env) of 
                                        { Nothing -> Nothing
                                        ; Just bt -> Just $ fshow bt             
                                        }
                     ; ["purp"]   -> Just $ fshow (envpurp   env)
                     ; ["mapper"] -> Just $ fshow (envmapper env)
                     ; ["cnect"]  -> Just $ let (_, _, towhdls ) = envtow env
                                                (_, _, frowhdls) = envfrow env
                                             in fshow (towhdls ++ frowhdls)
                     ; ["var"]    -> Just $ fshow (envvar   env)
                     ; ["val"]    -> Just $ fshow (envval   env)
                     ; _          -> Nothing
                     }
     case txt of
     { Nothing -> do nack "SHOW" $ "no show"
                     cmdsIntpr
     ; Just s  -> do pack "SHOW" $ s
                     fack "SHOW" $ ""
                     cmdsIntpr
     }
  
-- ----------------------------------------------------------------------------------------- --

cmdGoTo :: String -> IOE ()
cmdGoTo args  =  do
     state <- case words args of
              { []        -> do gets envcurs
              ; ["back"]  -> do return $ -1
              ; ["back",d] | and (map Char.isDigit d)
                          -> do steps <- return $ read d
                                if  steps == 0
                                  then gets envcurs
                                  else return $ -steps
              ; [d] | and (map Char.isDigit d)
                          -> do return $ read d
              ; _         -> do mack "GOTO" $ "unknown state"
                                gets envcurs
              }
     if  state >= 0
       then do s2bt <- gets envs2bt
               case Map.lookup state s2bt of
               { Nothing -> do nack "GOTO" $ "unknown state"
                               cmdsIntpr
               ; Just ms -> do modify $ \env -> env { envcurs = state }
                               fack "GOTO" $ show state
                               cmdsIntpr
               }
       else do ltsBackN (-state)
               newstate <- gets envcurs
               fack "GOTO" $ show newstate
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdPath :: String -> IOE ()
cmdPath args  =  do
     initstate <- gets envinit
     curstate  <- gets envcurs
     path      <- path initstate curstate
     txt       <- return $ concat $ [ (showN n 5) ++ ": " ++
                                      (fshow s1) ++ " -> " ++ (unwords $ lines (fshow a)) ++
                                      " -> " ++ (fshow s2) ++ "\n"
                                    | (n,(s1,a,s2)) <- zip [1 ..] path
                                    ]
     fack "PATH" $ txt
     cmdsIntpr
     
-- ----------------------------------------------------------------------------------------- --

cmdTrace :: String -> IOE ()
cmdTrace args  =  do
     initstate <- gets envinit
     curstate  <- gets envcurs
     path      <- path initstate curstate
     trace     <- return $ [ a | (s,a,s') <- path ]
     arg       <- return $ if  null args
                             then "txs"                       -- no arg: Default Torxakis
                             else (Text.unpack (Text.strip (Text.pack args)))
     txt       <- if arg == "txs"
                    then return $ concat $ [ (showN n 5) ++ ":  " ++ (fshow a)
                                           | (n,a) <- zip [1..] trace
                                           ] 
                    else do spec <- getSpec
                            toProcdef trace spec
     fack "TRACE" $ txt
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdMenu :: String -> IOE ()
cmdMenu args  =  do
     curstnr   <- gets envcurs
     s2bt      <- gets envs2bt
     (io,stnr) <- return $ case words args of
                           { []        -> ("all", curstnr)
                           ; ["in"]    -> ("in", curstnr)
                           ; ["in",s]  -> if  and (map Char.isDigit s)
                                            then ("in", (read s)::StatNr)
                                            else ("in", -11)
                           ; ["out"]   -> ("out", curstnr)
                           ; ["out",s] -> if  and (map Char.isDigit s)
                                            then ("out", (read s)::StatNr)
                                            else ("out", -11)
                           ; [s]       -> if  and (map Char.isDigit s)
                                            then ("all", (read s)::StatNr)
                                            else ("all", -11)
                           ; _         -> ("all", -11)
                           }
     mnu <- case (io,Map.lookup stnr s2bt) of
            { (_,Nothing)     -> do mack "MENU" $ "no state"
                                    return $ []
            ; ("all",Just bt) -> do return $ menu bt
            ; ("in",Just bt)  -> do filterM (isInCTOffers.frst) (menu bt)
            ; ("out",Just bt) -> do filterM (isOutCTOffers.frst) (menu bt)
            }
     fack "MENU" $ fshow mnu
     cmdsIntpr


-- ----------------------------------------------------------------------------------------- --
--
-- Helper Functions
--
-- ----------------------------------------------------------------------------------------- --
-- consistentX :  check consistent configurations for testing, Simulation, Stepping

{-

consistentTester :: TxsDef -> TxsDef -> TxsDef -> Bool
consistentTester modeldef mapperdef cnectdef
  =  case (modeldef,mapperdef,cnectdef) of
     { ( ModelDef specinsets specoutsets _ specbexp
       , ModelDef mapinsets  mapoutsets  _ mapbexp
       , CnectDef cnecttype conndefs
       ) -> let { sins   = Set.fromList $ map sig $ Set.toList $ Set.unions specinsets
                ; souts  = Set.fromList $ map sig $ Set.toList $ Set.unions specoutsets
                ; mins   = Set.fromList $ map sig $ Set.toList $ Set.unions mapinsets
                ; mouts  = Set.fromList $ map sig $ Set.toList $ Set.unions mapoutsets
                ; ctows  = Set.fromList $ map sig [ chan | ConnDtoW  chan _ _ _ _ <- conndefs ]
                ; cfrows = Set.fromList $ map sig [ chan | ConnDfroW chan _ _ _ _ <- conndefs ]
                }
            in  sins   `Set.isSubsetOf` mins
             && cfrows `Set.isSubsetOf` mins
             && mouts  `Set.isSubsetOf` ( ctows `Set.union` souts )
     ; ( _, _, _
       ) -> error $ "TXS consistentTester: Wrong call\n"
     }

-}


consistentTester :: TxsDef -> TxsDef -> Bool
consistentTester modeldef cnectdef
  =  case (modeldef,cnectdef) of
     { ( DefModel (ModelDef specinsets specoutsets _ specbexp)
       , DefCnect (CnectDef cnecttype conndefs)
       ) -> let { sins   = Set.fromList $ map (sig . IdChan) $ Set.toList $ Set.unions specinsets
                ; souts  = Set.fromList $ map (sig . IdChan) $ Set.toList $ Set.unions specoutsets
                ; ctows  = Set.fromList $ map (sig . IdChan) [ chan | ConnDtoW  chan _ _ _ _ <- conndefs ]
                ; cfrows = Set.fromList $ map (sig . IdChan) [ chan | ConnDfroW chan _ _ _ _ <- conndefs ]
                }
            in  sins   `Set.isSubsetOf` ctows
             && cfrows `Set.isSubsetOf` souts
     ; ( _, _
       ) -> error $ "TXS consistentTester: Wrong call\n"
     }


consistentSimulator :: TxsDef -> TxsDef -> Bool
consistentSimulator modeldef cnectdef
  =  case (modeldef,cnectdef) of
     { ( DefModel (ModelDef specinsets specoutsets _ specbexp)
       , DefCnect (CnectDef cnecttype conndefs)
       ) -> let { sins   = Set.fromList $ map (sig . IdChan) $ Set.toList $ Set.unions specinsets
                ; souts  = Set.fromList $ map (sig . IdChan) $ Set.toList $ Set.unions specoutsets
                ; ctows  = Set.fromList $ map (sig . IdChan) [ chan | ConnDtoW  chan _ _ _ _ <- conndefs ]
                ; cfrows = Set.fromList $ map (sig . IdChan) [ chan | ConnDfroW chan _ _ _ _ <- conndefs ]
                }
            in  souts  `Set.isSubsetOf` ctows 
             && cfrows `Set.isSubsetOf` sins
     ; ( _, _
       ) -> error $ "TXS consistentSimulator: Wrong call\n"
     }


consistentStepper :: TxsDef -> Bool
consistentStepper modeldef
  =  case modeldef of
     { ( DefModel (ModelDef specinsets specoutsets _ specbexp)
       ) -> let { sins   = Set.fromList $ map (sig . IdChan) $ Set.toList $ Set.unions specinsets
                ; souts  = Set.fromList $ map (sig . IdChan) $ Set.toList $ Set.unions specoutsets
                }
            in  True
     ; ( _
       ) -> error $ "TXS consistentStepper: Wrong call\n"
     }


-- ----------------------------------------------------------------------------------------- --
-- readAction  :  read Action from String

readAction :: [ChanId] -> String -> IOE (Either String Action)
readAction chids args  =  do
     env              <- get
     uid              <- return $ envuid env
     tdefs            <- return $ envtdefs env
     ((uid',offs'),e) <- lift $ catch ( let p = prefoffsParser ( ( Ctdefs   $ tdefs )
                                                                : ( Cchanenv $ chids )
                                                                : ( Cvarenv  $ Map.keys
                                                                               (envval env) )
                                                                : ( Cunid    $ uid + 1 )
                                                                : ( txsLexer args)
                                                               )
                                         in return $! (show p) `deepseq` (p,"")
                                      )
                                      ( \e -> return $ ((uid,Set.empty),(show (e::ErrorCall)))
                                      )
     if  e /= ""
       then do return $ Left e
       else do
         qstnoffs <- return [ q | q@(Quest vid) <- concat $ map chanoffers (Set.toList offs') ]
         if  not $ null qstnoffs
           then do
             modify $ \env -> env { envuid = uid' }
             return $ Left "no question mark offer allowed ain action"
           else do
             modify $ \env -> env { envuid = uid' }
             venv <- gets envval
             acts <- sequence $ [ liftP2 (chid, sequence [ eval vexp | Exclam vexp <- choffs ])
                                | Offer chid choffs <- Set.toList offs'
                                ]
             return $ Right ( Act (Set.fromList acts) )


-- ----------------------------------------------------------------------------------------- --


ltsBackN :: Int -> IOE ()
ltsBackN backsteps
   | backsteps <= 0  =  do
       return $ ()
   | backsteps > 0   =  do
       init     <- gets envinit
       curstate <- gets envcurs
       trie     <- gets envtrie
       case [ (s,s') | (s,a,s') <- trie, s' == curstate ] of
       { [(prev,cur)] -> do modify $ \env -> env { envcurs = prev }
                            if  prev == init
                              then do return $ ()
                              else do ltsBackN (backsteps-1)
       ; _            -> do mack "GOTO" $ "something wrong with Trie"
                            do return $ ()
       }


path :: StatNr -> StatNr -> IOE [(StatNr,Action,StatNr)]
path from to
   | from >= to  =  do
       return $ []
   | from < to   =  do
       init     <- gets envinit
       trie     <- gets envtrie
       case [ (s1,a,s2) | (s1,a,s2) <- trie, s2 == to ] of
       { [(s1,a,s2)] -> do if (s1 == from) || (s1 == init)
                             then do return $ [(s1,a,s2)]
                             else do pp <- path from s1
                                     return $ pp ++ [(s1,a,s2)]
       ; _            -> do mack "BACK" $ "something wrong with Trie"
                            return $ []
       }


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

