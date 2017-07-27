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
-- export

(
  main      -- main :: IO ()
            -- torxakis server main
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import Data.Either -- TODO: use Validation instead
import System.IO
import System.Environment
import Network
import Control.Monad.State
import Control.Concurrent
import Control.Exception
import Control.DeepSeq

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map

-- import from local
import ToProcdef
import CmdLineParser

-- import from serverenv
import qualified EnvServer    as IOS
import qualified IfServer     as IFS
import qualified ParamServer  as ParamServer

-- import from core
import qualified Config       as CoreConfig
import qualified TxsCore      as TxsCore
import qualified BuildInfo    as BuildInfo
import qualified VersionInfo  as VersionInfo

-- import from defs
import qualified TxsDefs      as TxsDefs
import qualified VarId        as VarId
import qualified TxsDDefs     as TxsDDefs
import qualified TxsShow      as TxsShow 
import qualified TxsUtils     as TxsUtils
import qualified Utils        as Utils

-- import from front
import qualified TxsHappy     as TxsHappy
import qualified TxsAlex      as TxsAlex

-- import from cnect
import qualified SocketWorld as World

-- import from value
import qualified Eval         as Eval

main :: IO ()
main  =  withSocketsDo $ do
  hSetBuffering stderr NoBuffering     -- alt: LineBuffering

  uConfig <- loadConfig
     
  case interpretConfig uConfig of
    Left xs -> do
      hPutStrLn stderr $
        "Errors found while loading the configuration"
      hPutStrLn stderr (show xs)
    Right config -> do
      let portNr = portNumber config
      servsock       <- listenOn (PortNumber portNr)
      (hs, host, port) <- accept servsock
      hSetBuffering hs LineBuffering
      hSetEncoding hs latin1
      hPutStrLn stderr "\nTXSSERVER >>  Starting  ..... \n"
      let initS = IOS.envsNone
            { IOS.host   = host
            , IOS.portNr = portNr
            , IOS.servhs = hs
            }
          coreConfig = CoreConfig.Config
            { CoreConfig.smtSolver = (smtSolver config)
            , CoreConfig.smtLog = (smtLog config)
            }
      TxsCore.runTxsCore coreConfig cmdsIntpr initS
      threadDelay 1000000    -- 1 sec delay on closing
      sClose servsock
      hPutStrLn stderr "\nTXSSERVER >>  Closing  ..... \n"

-- * TorXakis server commands processing

cmdsIntpr :: IOS.IOS ()
cmdsIntpr  =  do
     modus      <- gets IOS.modus
     (cmd,args) <- IFS.getCmd
     case cmd of
-- ----------------------------------------------------------------------------------- modus --
       "START"     |       IOS.isNoned    modus  ->  cmdStart     args
       "START"     | not $ IOS.isNoned    modus  ->  cmdNoop      cmd
       "QUIT"                                    ->  cmdQuit      args  
       "INIT"      |       IOS.isIdled    modus  ->  cmdInit      args  
       "INIT"      | not $ IOS.isIdled    modus  ->  cmdNoop      cmd
       "TERMIT"    |       IOS.isGtIdled  modus  ->  cmdTermit    args
       "TERMIT"    | not $ IOS.isGtIdled  modus  ->  cmdNoop      cmd
       "STOP"      |       IOS.isGtInited modus  ->  cmdStop      args
       "STOP"      | not $ IOS.isGtInited modus  ->  cmdNoop      cmd
-- -------------------------------------------------------------------------------- settings --
       "INFO"      |       IOS.isGtNoned  modus  ->  cmdInfo      args
       "INFO"      | not $ IOS.isGtNoned  modus  ->  cmdNoop      cmd
       "PARAM"     |       IOS.isGtNoned  modus  ->  cmdParam     args
       "PARAM"     | not $ IOS.isGtNoned  modus  ->  cmdNoop      cmd
       "SEED"      |       IOS.isGtNoned  modus  ->  cmdSeed      args
       "SEED"      | not $ IOS.isGtNoned  modus  ->  cmdNoop      cmd
-- ------------------------------------------------------------------------------------ data --
       "VAR"       |       IOS.isGtIdled  modus  ->  cmdVar       args
       "VAR"       | not $ IOS.isGtIdled  modus  ->  cmdNoop      cmd
       "VAL"       |       IOS.isGtIdled  modus  ->  cmdVal       args
       "VAL"       | not $ IOS.isGtIdled  modus  ->  cmdNoop      cmd
       "EVAL"      |       IOS.isGtIdled  modus  ->  cmdEval      args
       "EVAL"      | not $ IOS.isGtIdled  modus  ->  cmdNoop      cmd 
       "SOLVE"     |       IOS.isGtIdled  modus  ->  cmdSolve     args "sol"
       "SOLVE"     | not $ IOS.isGtIdled  modus  ->  cmdNoop      cmd
       "UNISOLVE"  |       IOS.isGtIdled  modus  ->  cmdSolve     args "uni"
       "UNISOLVE"  | not $ IOS.isGtIdled  modus  ->  cmdNoop      cmd
       "RANSOLVE"  |       IOS.isGtIdled  modus  ->  cmdSolve     args "ran"
       "RANSOLVE"  | not $ IOS.isGtIdled  modus  ->  cmdNoop      cmd
-- ----- ------------------------------------------------------------------------------ exec --
       "TESTER"    |       IOS.isInited   modus  ->  cmdTester    args
       "TESTER"    | not $ IOS.isInited   modus  ->  cmdNoop      cmd
       "SIMULATOR" |       IOS.isInited   modus  ->  cmdSimulator args
       "SIMULATOR" | not $ IOS.isInited   modus  ->  cmdNoop      cmd
       "STEPPER"   |       IOS.isInited   modus  ->  cmdStepper   args
       "STEPPER"   | not $ IOS.isInited   modus  ->  cmdNoop      cmd
-- -------------------------------------------------------------------- test, simulate, step --
       "TEST"      |       IOS.isTested   modus  ->  cmdTest      args
       "TEST"      | not $ IOS.isTested   modus  ->  cmdNoop      cmd
       "SIM"       |       IOS.isSimuled  modus  ->  cmdSim       args
       "SIM"       | not $ IOS.isSimuled  modus  ->  cmdNoop      cmd
       "STEP"      |       IOS.isStepped  modus  ->  cmdStep      args
       "STEP"      | not $ IOS.isStepped  modus  ->  cmdNoop      cmd
-- ----------------------------------------------------------------------------- btree state --
       "SHOW"      |       IOS.isGtIdled  modus  ->  cmdShow      args
       "SHOW"      | not $ IOS.isGtIdled  modus  ->  cmdNoop      cmd
       "GOTO"      |       IOS.isGtInited modus  ->  cmdGoTo      args
       "GOTO"      | not $ IOS.isGtInited modus  ->  cmdNoop      cmd
       "PATH"      |       IOS.isGtInited modus  ->  cmdPath      args
       "PATH"      | not $ IOS.isGtInited modus  ->  cmdNoop      cmd
       "TRACE"     |       IOS.isGtInited modus  ->  cmdTrace     args
       "TRACE"     | not $ IOS.isGtInited modus  ->  cmdNoop      cmd
       "MENU"      |       IOS.isGtInited modus  ->  cmdMenu      args
       "MENU"      | not $ IOS.isGtInited modus  ->  cmdNoop      cmd
       "MAP"       |       IOS.isTested   modus  ->  cmdMap       args
       "MAP"       |       IOS.isSimuled  modus  ->  cmdMap       args
       "MAP"       |       IOS.isStepped  modus  ->  cmdNoop      cmd
       "MAP"       | not $ IOS.isGtInited modus  ->  cmdNoop      cmd
       "NCOMP"     |       IOS.isInited   modus  ->  cmdNComp     args
       "NCOMP"     | not $ IOS.isInited   modus  ->  cmdNoop      cmd
       _                                         ->  cmdUnknown   cmd


-- ----------------------------------------------------------------------------------------- --
-- torxakis server individual command processing

-- ----------------------------------------------------------------------------------------- --

cmdNoop :: String -> IOS.IOS ()
cmdNoop cmd  =  do
     IFS.nack cmd [ "NoOp : No action"]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdUnknown :: String -> IOS.IOS ()
cmdUnknown cmd  =  do
     IFS.nack "ERROR" [ "Unknown command : " ++ cmd ]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStart :: String -> IOS.IOS ()
cmdStart _  =  do
     modify $ \env -> env { IOS.modus = IOS.Idled }
     host <- gets IOS.host
     port <- gets IOS.portNr
     IFS.pack "START" ["txsserver starting:  " ++ show host ++ " : " ++ show port]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdQuit :: String -> IOS.IOS ()
cmdQuit _  =  do
     modify $ \env -> env { IOS.modus = IOS.Noned }
     host <- gets IOS.host
     port <- gets IOS.portNr
     IFS.pack "QUIT" ["txsserver closing  " ++ show host ++ " : " ++ show port]
     return ()

-- ----------------------------------------------------------------------------------------- --

cmdInit :: String -> IOS.IOS ()
cmdInit args  =  do
     servhs             <- gets IOS.servhs
     unid               <- gets IOS.uid
     tdefs              <- gets IOS.tdefs
     sigs               <- gets IOS.sigs
     srctxts            <- lift $ lift $ mapM readFile (words args)
     srctxt             <- return $ List.intercalate "\n\n" srctxts
     ((unid',tdefs', sigs'),e) <- lift $ lift $ catch
                             ( let parsing = TxsHappy.txsParser (TxsAlex.txsLexer srctxt)
                                in return $!! (parsing, "")
                             )
                             ( \e -> return $ ((unid, tdefs, sigs), show (e::ErrorCall))
                             )
     if e /= ""
       then do IFS.nack "INIT" [e]
               cmdsIntpr
       else do modify $ \env -> env { IOS.modus  = IOS.Inited
                                    , IOS.uid    = unid'
                                    , IOS.tdefs  = tdefs'
                                    , IOS.sigs   = sigs'
                                    }
               lift $ TxsCore.txsInit tdefs' sigs' ( (IFS.hmack servhs) . (map TxsShow.pshow) )
               IFS.pack "INIT" ["input files parsed:", List.intercalate " " (words args)]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTermit :: String -> IOS.IOS ()
cmdTermit _  =  do
     modify $ \env -> env { IOS.modus  = IOS.Idled
                          , IOS.tdefs  = TxsDefs.empty
                          , IOS.tow    = ( Nothing, Nothing, [] )
                          , IOS.frow   = ( Nothing, [],      [] )
                          }
     lift $ TxsCore.txsTermit
     IFS.pack "TERMIT" []
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStop :: String -> IOS.IOS ()
cmdStop _  =  do
     World.closeSockets
     modus <- gets IOS.modus
     if  IOS.isSimuled modus
       then do [(nmsut,valsut)] <- IOS.getParams ["param_Sut_deltaTime"]
               [(nmsim,valsim)] <- IOS.getParams ["param_Sim_deltaTime"]
               -- IOS.setParams [("param_Sut_deltaTime",valsut)]
               -- IOS.setParams [("param_Sim_deltaTime",valsim)]
               modify $ \env -> env { IOS.modus   = IOS.Inited
                                    , IOS.tow     = ( Nothing, Nothing, [] )
                                    , IOS.frow    = ( Nothing, [],      [] )
                                    }
               lift $ TxsCore.txsStop
               IFS.pack "STOP" []
               cmdsIntpr
       else do modify $ \env -> env { IOS.modus   = IOS.Inited
                                    , IOS.tow     = ( Nothing, Nothing, [] )
                                    , IOS.frow    = ( Nothing, [],      [] )
                                    }
               lift $ TxsCore.txsStop
               IFS.pack "STOP" []
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdInfo :: String -> IOS.IOS ()
cmdInfo _  =  do
     IFS.pack "INFO" [ "TorXakis version    : " ++ VersionInfo.version
                     , "Build time          : " ++ BuildInfo.buildTime
                     ]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdParam :: String -> IOS.IOS ()
cmdParam args  =  do
     case words args of
       []        -> do params1 <- lift $ TxsCore.txsGetParams
                       params2 <- IOS.getParams []
                       mapM IFS.mack [ [ nm ++ " = " ++ val ]
                                     | (nm,val) <- params1 ++ params2
                                     ]
                       IFS.pack "PARAM" []
                       cmdsIntpr
       [prm]     -> do params1 <- lift $ TxsCore.txsGetParam prm
                       params2 <- IOS.getParams [prm]
                       case params1++params2 of
                         []         -> IFS.nack "PARAM" [ "No parameter: " ++ prm ]
                         [(nm,val)] -> IFS.pack "PARAM" [ prm++" = "++val ]
                         _          -> IFS.nack "PARAM" [ "More parameters: " ++ prm ]
                       cmdsIntpr
       [prm,val] -> do params1 <- lift $ TxsCore.txsSetParam prm val
                       params2 <- IOS.setParams [(prm,val)]
                       case params1++params2 of
                         []         -> IFS.nack "PARAM" [ "No parameter: " ++ prm ]
                         [(nm,val)] -> IFS.pack "PARAM" [ prm++" = "++val ]
                         _          -> IFS.nack "PARAM" [ "More parameters: " ++ prm ]
                       cmdsIntpr
       _         -> do IFS.nack "PARAM" [ "Unknown parameter action" ]
                       cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSeed :: String -> IOS.IOS ()
cmdSeed args  =  do
     case words args of
       [val] -> do seed <- return $ read val
                   lift $ TxsCore.txsSetSeed seed
                   IFS.pack "SEED" []
                   cmdsIntpr
       _     -> do IFS.nack "SEED" [ "Incorrect seed" ]
                   cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdVar :: String -> IOS.IOS ()
cmdVar args  =  do
     env              <- get
     uid              <- return $ IOS.uid env
     tdefs            <- return $ IOS.tdefs env
     sigs             <- return $ IOS.sigs env
     vars             <- return $ IOS.locvars env
     vals             <- return $ IOS.locvals env
     if  args == ""
       then do
         IFS.pack "VAR" [ TxsShow.fshow vars ]
         cmdsIntpr
       else do
         ((uid',vars'),e) <- lift $ lift $ catch
                               ( let p = TxsHappy.vardeclsParser
                                           (  ( TxsAlex.Ctdefs $ tdefs )
                                            : ( TxsAlex.Csigs $ sigs )
                                            : ( TxsAlex.Cunid $ uid + 1 )
                                            : ( TxsAlex.txsLexer args )
                                           )
                                  in return $!! (p,"")
                               )
                               ( \e -> return $ ((uid,[]),(show (e::ErrorCall)))
                               )
         if  e /= ""
           then do
             modify $ \env -> env { IOS.uid = uid' }
             IFS.nack "VAR" [ e ]
             cmdsIntpr
           else do
             if  let newnames = (map VarId.name vars')
                  in ( newnames `List.intersect` (map VarId.name vars) == [] ) &&
                     ( newnames `List.intersect` (map VarId.name (Map.keys vals)) == [] )
               then do
                 modify $ \env -> env { IOS.locvars = vars ++ vars'
                                      , IOS.uid  = uid'
                                      }
                 IFS.pack "VAR" [ TxsShow.fshow vars' ]
                 cmdsIntpr
               else do
                 modify $ \env -> env { IOS.uid = uid' }
                 IFS.nack "VAR" [ "double variable names: " ++ (TxsShow.fshow vars') ]
                 cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdVal :: String -> IOS.IOS ()
cmdVal args  =  do
     env              <- get
     uid              <- return $ IOS.uid env
     tdefs            <- return $ IOS.tdefs env
     sigs             <- return $ IOS.sigs env
     vars             <- return $ IOS.locvars env
     vals             <- return $ IOS.locvals env
     if  args == ""
       then do
         IFS.pack "VAL" [ TxsShow.fshow vals ]
         cmdsIntpr
       else do
         ((uid',venv'),e) <- lift $ lift $ catch
                               ( let p = TxsHappy.valdefsParser
                                           (  ( TxsAlex.Ctdefs  $ tdefs )
                                            : ( TxsAlex.Csigs  $ sigs )
                                            : ( TxsAlex.Cvarenv $ [] )
                                            : ( TxsAlex.Cunid $ uid + 1 )
                                            : ( TxsAlex.txsLexer args )
                                           )
                                  in return $!! (p,"")
                               )
                               ( \e -> return $ ((uid,Map.empty),(show (e::ErrorCall)))
                               )
         if  e /= ""
           then do
             modify $ \env -> env { IOS.uid = uid' }
             IFS.nack "VAL" [ e ]
             cmdsIntpr
           else do
             if  let newnames = map VarId.name (Map.keys venv')
                  in ( newnames `List.intersect` (map VarId.name vars) == [] ) &&
                     ( newnames `List.intersect` (map VarId.name (Map.keys vals)) == [] )
               then do
                 modify $ \env -> env { IOS.locvals = vals `Map.union` venv'
                                      , IOS.uid     = uid'
                                      }
                 IFS.pack "VAL" [ TxsShow.fshow venv' ]
                 cmdsIntpr
               else do
                 modify $ \env -> env { IOS.uid = uid' }
                 IFS.nack "VAR" [ "double value names: " ++ (TxsShow.fshow venv') ]
                 cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdEval :: String -> IOS.IOS ()
cmdEval args  =  do
     env              <- get
     uid              <- return $ IOS.uid env
     tdefs            <- return $ IOS.tdefs env
     sigs             <- return $ IOS.sigs env     
     vals             <- return $ IOS.locvals env
     ((uid',vexp'),e) <- lift $ lift $ catch
                           ( let p = TxsHappy.vexprParser
                                        (  ( TxsAlex.Ctdefs  $ tdefs )
                                         : ( TxsAlex.Csigs   $ sigs )
                                         : ( TxsAlex.Cvarenv $ Map.keys vals )
                                         : ( TxsAlex.Cunid   $ uid + 1 )
                                         : ( TxsAlex.txsLexer args )
                                        )
                              in return $!! (p,"")
                           )
                           ( \e -> return $ ((uid,TxsDefs.cstrError ""),(show (e::ErrorCall)))
                           )
     if  e /= ""
       then do modify $ \env -> env { IOS.uid = uid' }
               IFS.nack "EVAL" [ e ]
               cmdsIntpr
       else do modify $ \env -> env { IOS.uid = uid' }
               walue <- lift $ TxsCore.txsEval (TxsDefs.cstrEnv vals vexp')
               IFS.pack "EVAL" [ TxsShow.fshow walue ]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSolve :: String -> String -> IOS.IOS ()
cmdSolve args kind  =  do
     (cmd,solver)     <- return $ case kind of
                                    "sol" -> ( "SOLVE"   , TxsCore.txsSolve    )
                                    "uni" -> ( "UNISOLVE", TxsCore.txsUniSolve )
                                    "ran" -> ( "RANSOLVE", TxsCore.txsRanSolve )
     env              <- get
     uid              <- return $ IOS.uid env
     tdefs            <- return $ IOS.tdefs env
     sigs             <- return $ IOS.sigs env     
     vars             <- return $ IOS.locvars env
     vals             <- return $ IOS.locvals env
     ((uid',vexp'),e) <- lift $ lift $ catch
                           ( let p = TxsHappy.vexprParser
                                       (  ( TxsAlex.Ctdefs $ tdefs )
                                        : ( TxsAlex.Csigs $ sigs )
                                        : ( TxsAlex.Cvarenv $ (Map.keys vals) ++ vars )
                                        : ( TxsAlex.Cunid $ uid + 1 )
                                        : ( TxsAlex.txsLexer args )
                                       )
                              in return $!! (p,"")
                           )
                           ( \e -> return $ ((uid,TxsDefs.cstrError ""),(show (e::ErrorCall)))
                           )
     if  e /= ""
       then do modify $ \env -> env { IOS.uid = uid' }
               IFS.nack cmd [ e ]
               cmdsIntpr
       else do modify $ \env -> env { IOS.uid = uid' }
               sols  <- lift $ solver (TxsDefs.cstrEnv vals vexp')
               IFS.pack cmd [ TxsShow.fshow sols ]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTester :: String -> IOS.IOS ()
cmdTester args  =  do
     tdefs  <- gets IOS.tdefs
     case words args of
     { [m,c] -> do
            mdefs <- return $ [ mdef
                              | (TxsDefs.ModelId nm uid, mdef) <- Map.toList (TxsDefs.modelDefs tdefs)
                              , nm == m
                              ]
            cdefs <- return $ [ cdef
                              | (TxsDefs.CnectId nm uid, cdef) <- Map.toList (TxsDefs.cnectDefs tdefs)
                              , nm == c
                              ]
            case (mdefs,cdefs) of
            { ([modeldef],[cnectdef])
                         | isConsistentTester modeldef Nothing Nothing cnectdef
                -> do modify $ \env -> env { IOS.modus = IOS.Tested cnectdef }
                      World.openSockets
                      envs  <- get
                      lift $ TxsCore.txsSetTest (World.putSocket envs) (World.getSocket envs)
                                                modeldef Nothing Nothing
                      IFS.pack "TESTER" []
                      cmdsIntpr
            ; _ -> do IFS.nack "TESTER" [ "Wrong or inconsistent parameters" ]
                      cmdsIntpr
            }
     ; [m,x,c] -> do
            mdefs <- return $ [ mdef
                              | (TxsDefs.ModelId  nm uid, mdef) <- Map.toList (TxsDefs.modelDefs tdefs)
                              , nm == m
                              ]
            adefs <- return $ [ adef
                              | (TxsDefs.MapperId nm uid, adef) <- Map.toList (TxsDefs.mapperDefs tdefs)
                              , nm == x
                              ]
            pdefs <- return $ [ pdef
                              | (TxsDefs.PurpId   nm uid, pdef) <- Map.toList (TxsDefs.purpDefs tdefs)
                              , nm == x
                              ]
            cdefs <- return $ [ cdef
                              | (TxsDefs.CnectId  nm uid, cdef) <- Map.toList (TxsDefs.cnectDefs tdefs)
                              , nm == c
                              ]
            case (mdefs,adefs,pdefs,cdefs) of
            { ([modeldef],[mapperdef],[],[cnectdef])
                         | isConsistentTester modeldef (Just mapperdef) Nothing cnectdef
                -> do modify $ \env -> env { IOS.modus  = IOS.Tested cnectdef }
                      World.openSockets
                      envs  <- get
                      lift $ TxsCore.txsSetTest (World.putSocket envs) (World.getSocket envs)
                                                modeldef (Just mapperdef) Nothing
                      IFS.pack "TESTER" []
                      cmdsIntpr
            ; ([modeldef],[],[purpdef],[cnectdef])
                         | isConsistentTester modeldef Nothing (Just purpdef) cnectdef
                -> do modify $ \env -> env { IOS.modus  = IOS.Tested cnectdef }
                      World.openSockets
                      envs  <- get
                      lift $ TxsCore.txsSetTest (World.putSocket envs) (World.getSocket envs)
                                                modeldef Nothing (Just purpdef)
                      IFS.pack "TESTER" [ ]
                      cmdsIntpr
            ; _ -> do IFS.nack "TESTER" [ "Wrong or inconsistent parameters" ]
                      cmdsIntpr
            }
     ; [m,x,y,c] -> do
            mdefs <- return $ [ mdef
                              | (TxsDefs.ModelId  nm uid, mdef) <- Map.toList (TxsDefs.modelDefs tdefs)
                              , nm == m
                              ]
            adefs <- return $ [ adef
                              | (TxsDefs.MapperId nm uid, adef) <- Map.toList (TxsDefs.mapperDefs tdefs)
                              , nm == x || nm == y
                              ]
            pdefs <- return $ [ pdef
                              | (TxsDefs.PurpId   nm uid, pdef) <- Map.toList (TxsDefs.purpDefs tdefs)
                              , nm == x || nm == y
                              ]
            cdefs <- return $ [ cdef
                              | (TxsDefs.CnectId  nm uid, cdef) <- Map.toList (TxsDefs.cnectDefs tdefs)
                              , nm == c
                              ]
            case (mdefs,adefs,pdefs,cdefs) of
            { ([modeldef],[mapperdef],[purpdef],[cnectdef])
                         | isConsistentTester modeldef (Just mapperdef) (Just purpdef) cnectdef
                -> do modify $ \env -> env { IOS.modus  = IOS.Tested cnectdef }
                      World.openSockets
                      envs  <- get
                      lift $ TxsCore.txsSetTest (World.putSocket envs) (World.getSocket envs)
                                                modeldef (Just mapperdef) (Just purpdef)
                      IFS.pack "TESTER" [ ]
                      cmdsIntpr
            ; _ -> do IFS.nack "TESTER" [ "Wrong or inconsistent parameters" ]
                      cmdsIntpr
            }
     ; _ -> do
            IFS.nack "TESTER" [ "Wrong parameters" ]
            cmdsIntpr
     }


isConsistentTester :: TxsDefs.ModelDef ->
                      Maybe TxsDefs.MapperDef ->
                      Maybe TxsDefs.PurpDef ->
                      TxsDefs.CnectDef ->
                      Bool

isConsistentTester (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
                   Nothing
                   _
                   (TxsDefs.CnectDef cnecttype conndefs)
  =  let { mins   = Set.fromList minsyncs
         ; mouts  = Set.fromList moutsyncs
         ; ctows  = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDtoW  chan _ _ _ _ <- conndefs ]
         ; cfrows = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDfroW chan _ _ _ _ <- conndefs ]
         } 
      in    mins   == ctows
         && cfrows == mouts

isConsistentTester (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
                   (Just (TxsDefs.MapperDef achins achouts asyncsets abexp))
                   _
                   (TxsDefs.CnectDef cnecttype conndefs)
  =  let { ctows  = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDtoW  chan _ _ _ _ <- conndefs ]
         ; cfrows = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDfroW chan _ _ _ _ <- conndefs ]
         ; ains   = Set.fromList $ filter (not . Set.null)
                        [ sync `Set.intersection` (Set.fromList achins)  | sync <- asyncsets ]
         ; aouts  = Set.fromList $ filter (not . Set.null)
                        [ sync `Set.intersection` (Set.fromList achouts) | sync <- asyncsets ]
         } 
      in    cfrows `Set.isSubsetOf` ains
         && ctows  `Set.isSubsetOf` aouts


-- ----------------------------------------------------------------------------------------- --

cmdSimulator :: String -> IOS.IOS ()
cmdSimulator args  =  do
     tdefs  <- gets IOS.tdefs
     case words args of
     { [m,c] -> do
            mdefs <- return $ [ mdef
                              | (TxsDefs.ModelId nm uid, mdef) <- Map.toList (TxsDefs.modelDefs tdefs)
                              , nm == m
                              ]
            cdefs <- return $ [ cdef
                              | (TxsDefs.CnectId nm uid, cdef) <- Map.toList (TxsDefs.cnectDefs tdefs)
                              , nm == c
                              ]
            case (mdefs,cdefs) of
            { ([modeldef],[cnectdef])
                         | isConsistentSimulator modeldef Nothing cnectdef
                -> do modify $ \env -> env { IOS.modus = IOS.Simuled cnectdef }
                      World.openSockets
                      envs  <- get
                      lift $ TxsCore.txsSetSim (World.putSocket envs) (World.getSocket envs)
                                               modeldef Nothing
                      IFS.pack "SIMULATOR" []
                      cmdsIntpr
            ; _ -> do IFS.nack "SIMULATOR" [ "Wrong or inconsistent parameters" ]
                      cmdsIntpr
            }
     ; [m,a,c] -> do
            mdefs <- return $ [ mdef
                              | (TxsDefs.ModelId nm uid, mdef) <- Map.toList (TxsDefs.modelDefs tdefs)
                              , nm == m
                              ]
            adefs <- return $ [ adef
                              | (TxsDefs.MapperId nm uid, adef) <- Map.toList (TxsDefs.mapperDefs tdefs)
                              , nm == a
                              ]
            cdefs <- return $ [ cdef
                               | (TxsDefs.CnectId nm uid, cdef) <- Map.toList (TxsDefs.cnectDefs tdefs)
                              , nm == c
                              ]
            case (mdefs,adefs,cdefs) of
            { ([modeldef],[mapperdef],[cnectdef])
                         | isConsistentSimulator modeldef (Just mapperdef) cnectdef
                -> do modify $ \env -> env { IOS.modus = IOS.Simuled cnectdef }
                      World.openSockets
                      envs  <- get
                      lift $ TxsCore.txsSetSim (World.putSocket envs) (World.getSocket envs)
                                               modeldef (Just mapperdef)
                      IFS.pack "SIMULATOR" []
                      cmdsIntpr
            ; _ -> do IFS.nack "SIMULATOR" [ "Wrong or inconsistent parameters" ]
                      cmdsIntpr
            }
     }


isConsistentSimulator :: TxsDefs.ModelDef -> Maybe TxsDefs.MapperDef -> TxsDefs.CnectDef -> Bool

isConsistentSimulator (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
                      Nothing
                      (TxsDefs.CnectDef cnecttype conndefs)
  =  let { mins   = Set.fromList minsyncs
         ; mouts  = Set.fromList moutsyncs
         ; ctows  = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDtoW  chan _ _ _ _ <- conndefs ]
         ; cfrows = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDfroW chan _ _ _ _ <- conndefs ]
         }
      in    mins  == cfrows 
         && mouts == ctows 
  
isConsistentSimulator (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
                      (Just (TxsDefs.MapperDef achins achouts asyncsets abexp))
                      (TxsDefs.CnectDef cnecttype conndefs)
  =  let { ctows  = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDtoW  chan _ _ _ _ <- conndefs ]
         ; cfrows = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDfroW chan _ _ _ _ <- conndefs ]
         ; ains   = Set.fromList $ filter (not . Set.null)
                        [ sync `Set.intersection` (Set.fromList achins)  | sync <- asyncsets ]
         ; aouts  = Set.fromList $ filter (not . Set.null)
                        [ sync `Set.intersection` (Set.fromList achouts) | sync <- asyncsets ]
         }
      in    cfrows `Set.isSubsetOf` ains
         && ctows  `Set.isSubsetOf` aouts


-- ----------------------------------------------------------------------------------------- --

cmdStepper :: String -> IOS.IOS ()
cmdStepper args  =  do
     tdefs  <- gets IOS.tdefs
     mdefs  <- return $ TxsDefs.modelDefs tdefs
     case words args of
     { [m] -> do
          mdefs <- return $ [ mdef
                            | (TxsDefs.ModelId nm uid, mdef) <- Map.toList mdefs
                            , nm == m
                            ]
          case mdefs of
          { [modeldef] -> do modify $ \env -> env { IOS.modus = IOS.Stepped }
                             lift $ TxsCore.txsSetStep modeldef
                             IFS.pack "STEPPER" []
                             cmdsIntpr
          ; _          -> do IFS.nack "STEPPER" [ "Wrong or inconsistent parameters" ]
                             cmdsIntpr
          }
     }

-- ----------------------------------------------------------------------------------------- --

cmdTest :: String -> IOS.IOS ()
cmdTest args  =  do
     case words args of
     { []                                                             -- observe one output --
          -> do verdict <-lift $ TxsCore.txsTestOut
                IFS.pack "TEST" [TxsShow.fshow verdict]
                cmdsIntpr
     ; [d] | and (map Char.isDigit d)                              -- d::int i/o test steps --
          -> do verdict <- lift $ TxsCore.txsTestN (read d)
                IFS.pack "TEST" [TxsShow.fshow verdict]
                cmdsIntpr
     ; _  -> do                                                 -- do given action as input --
                IOS.Tested (TxsDefs.CnectDef _ conndefs) <- gets IOS.modus
                ctows <- return [ chan | TxsDefs.ConnDtoW  chan _ _ _ _ <- conndefs ]
                act <- readAction ctows args
                if  act == TxsDDefs.ActQui
                  then do cmdsIntpr
                  else do verdict <- lift $ TxsCore.txsTestIn act
                          IFS.pack "TEST" [TxsShow.fshow verdict]
                          cmdsIntpr
     }

-- ----------------------------------------------------------------------------------------- --

cmdSim :: String -> IOS.IOS ()
cmdSim args  =  do
     case words args of
     { []                                                           -- no arg: infinite sim --
         -> do verdict <- lift $ TxsCore.txsSimN (-1)
               IFS.pack "SIM" [TxsShow.fshow verdict]
               cmdsIntpr
     ; [d] | and (map Char.isDigit d)                                   -- d::int sim steps --
         -> do verdict <- lift $ TxsCore.txsSimN (read d)
               IFS.pack "SIM" [TxsShow.fshow verdict]
               cmdsIntpr
     ; _                                                                -- not a valid call --
         -> do IFS.nack "SIM" ["wrong parameter"]
               cmdsIntpr
     }

-- ----------------------------------------------------------------------------------------- --

cmdStep :: String -> IOS.IOS ()
cmdStep args  =  do
     case words args of
     { []                                                                -- no arg: one step --
         -> do verdict <- lift $ TxsCore.txsStepN 1
               IFS.pack "STEP" [TxsShow.fshow verdict]
               cmdsIntpr
     ; [d] | and (map Char.isDigit d)                                        -- d::int steps --
         -> do verdict <- lift $ TxsCore.txsStepN (read d)
               IFS.pack "STEP" [TxsShow.fshow verdict]
               cmdsIntpr
     ; _                                                          -- action arg: step action --
         -> do tdefs  <- gets IOS.tdefs
               mdefs  <- return $ TxsDefs.modelDefs tdefs
               chids  <- return $ Set.toList $ Set.unions
                            [ Set.unions (chins ++ chouts ++ spls)
                            | (_, TxsDefs.ModelDef chins chouts spls _) <- Map.toList mdefs
                            ]
               act <- readAction chids args
               if  act == TxsDDefs.ActQui
                 then do cmdsIntpr
                 else do verdict <- lift $ TxsCore.txsStepA act
                         IFS.pack "STEP" [TxsShow.fshow verdict]
                         cmdsIntpr
     }

-- ----------------------------------------------------------------------------------------- --

cmdShow :: String -> IOS.IOS ()
cmdShow args  =  do
     envs <- get
     txt  <- case words args of
             {
             ; ["tdefs"]  -> lift $ TxsCore.txsShow "tdefs"
             ; ["state"]  -> lift $ TxsCore.txsShow "state"
             ; ["model"]  -> lift $ TxsCore.txsShow "model"
             ; ["mapper"] -> lift $ TxsCore.txsShow "mapper"
             ; ["purp"]   -> lift $ TxsCore.txsShow "purp"
             ; ["cnect"]  -> return $ let (_, _, towhdls ) = IOS.tow envs
                                          (_, _, frowhdls) = IOS.frow envs
                                       in TxsShow.fshow (towhdls ++ frowhdls)
             ; ["var"]    -> return $ TxsShow.fshow (IOS.locvars envs)
             ; ["val"]    -> return $ TxsShow.fshow (IOS.locvals envs)
             ; _          -> return $ ""
             }
     case txt of
     { "" -> do IFS.nack "SHOW" ["nothing to be shown"]
                cmdsIntpr
     ; s  -> do IFS.mack [s]
                IFS.pack "SHOW" ["\n"]
                cmdsIntpr
     }
  
-- ----------------------------------------------------------------------------------------- --

cmdGoTo :: String -> IOS.IOS ()
cmdGoTo args  =  do
     case words args of
     { []        -> do IFS.pack "GOTO" ["gone to current state"]
                       cmdsIntpr
     ; ["back"]  -> do lift $ TxsCore.txsGoTo (-1)
                       IFS.pack "GOTO" ["gone to previous state"]
                       cmdsIntpr
     ; ["back",d] | and (map Char.isDigit d)
                 -> do steps <- return $ read d
                       if  steps == 0
                         then do IFS.pack "GOTO" ["gone to current state"]
                                 cmdsIntpr
                         else do lift $ TxsCore.txsGoTo (-steps)
                                 IFS.pack "GOTO" ["gone back " ++ (show steps) ++ "states"]
                                 cmdsIntpr 
     ; [d] | and (map Char.isDigit d)
                 -> do lift $ TxsCore.txsGoTo (read d)
                       IFS.pack "GOTO" ["gone to state " ++ d]
                       cmdsIntpr 
     ; _         -> do IFS.nack "GOTO" ["unknown state"]
                       cmdsIntpr
     }

-- ----------------------------------------------------------------------------------------- --

cmdPath :: String -> IOS.IOS ()
cmdPath args  =  do
     path <- lift $ TxsCore.txsPath
     IFS.mack [ (TxsShow.showN n 6) ++ ": " ++ (TxsShow.fshow s1) ++ " -> " ++
                (unwords $ lines (TxsShow.fshow a)) ++ " -> " ++ (TxsShow.fshow s2)
              | (n,(s1,a,s2)) <- zip [1 ..] path
              ]
     IFS.pack "PATH" ["\n"]
     cmdsIntpr
     
-- ----------------------------------------------------------------------------------------- --

cmdTrace :: String -> IOS.IOS ()
cmdTrace args  =  do
     path  <- lift $ TxsCore.txsPath
     trace <- return $ [ a | (s,a,s') <- path ]
     case words args of
     { []      -> do IFS.mack [ (TxsShow.showN n 6) ++ ":  " ++ (TxsShow.fshow a)
                              | (n,(s1,a,s2)) <- zip [1..] path
                              ]
                     IFS.pack "TRACE" ["\n"]
                     cmdsIntpr
     ; ["txs"] -> do IFS.mack [toProcdef trace]
                     IFS.pack "TRACE" ["\n"]
                     cmdsIntpr
     ; _       -> do IFS.nack "TRACE" [ "No such trace format" ]
                     cmdsIntpr
     }

-- ----------------------------------------------------------------------------------------- --

cmdMenu :: String -> IOS.IOS ()
cmdMenu args  =  do
     (kind,what,stnr) <- return
          $ case words args of
            { []            -> ( "mod", "all", (-1) )
            ; ["in"]        -> ( "mod", "in", (-1)  )
            ; ["in",s]      -> if  and (map Char.isDigit s)
                                   then ( "mod", "in", (read s)::Int )
                                 else ( "mod", "in", (-11)             )
            ; ["out"]       -> ( "mod", "out", (-1) )
            ; ["out",s]     -> if  and (map Char.isDigit s)
                                 then ( "mod", "out", (read s)::Int )
                                 else ( "mod", "out", (-11) )
            ; [s]           -> if  and (map Char.isDigit s)
                                 then ( "mod", "all", (read s)::Int )
                                 else ( "mod", "all", (-11) )
            ; ["purp",nm]   -> ( "purp", nm , (-1))
            ; ["purp",gl,s] -> if  and (map Char.isDigit s)
                                 then ( "purp", gl, (read s)::Int )
                                 else ( "purp", gl, (-11))
            ; _             -> ( "mod", "all", (-11) )
            }
     menu <- lift $ TxsCore.txsMenu kind what stnr
     IFS.mack [ TxsShow.fshow menu ]
     IFS.pack "MENU" [ "\n" ]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdMap :: String -> IOS.IOS ()
cmdMap args  =  do
     tdefs   <- gets IOS.tdefs
     mdefs   <- return $ TxsDefs.mapperDefs tdefs
     inchids <- return $ concat [ chins
                                | ( TxsDefs.MapperId nm uid
                                  , TxsDefs.MapperDef chins chouts syncs bexp
                                  ) <- Map.toList mdefs
                                ]
     if  null inchids
       then do IFS.nack "MAP" [ "No mapper(s) defined" ]
               cmdsIntpr
       else do act  <- readAction inchids args
               if  act == TxsDDefs.ActQui
               then do IFS.nack "MAP" [ "Not a valid action" ]
                       cmdsIntpr
               else do act' <- lift $ TxsCore.txsMapper act
                       IFS.pack "MAP" [TxsShow.fshow act']
                       cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdNComp :: String -> IOS.IOS ()
cmdNComp args  =  do
     tdefs <- gets IOS.tdefs
     case words args of
     { [mname] -> case [ mdef
                       | mid@(TxsDefs.ModelId nm uid, mdef) <- Map.toList
                                                                 (TxsDefs.modelDefs tdefs)
                       , nm == mname
                       ] of
                  { [mdef]
                      -> do mayPurpId <- lift $ TxsCore.txsNComp mdef
                            case mayPurpId of
                            { Just purpid
                                -> do IFS.pack "NCOMP" [ "Test Purpose generated: "
                                                          ++ TxsShow.fshow purpid ]
                                      cmdsIntpr
                            ; Nothing
                                -> do IFS.nack "NCOMP" [ "Could not generate test purpose" ]
                                      cmdsIntpr
                            }
                  ; _ -> do IFS.nack "NCOMP" [ "No such MODELDEF" ]
                            cmdsIntpr
                  }
     ; _       -> do IFS.nack "NCOMP" [ "Argument must be one MODELDEF name" ]
                     cmdsIntpr
     }


-- ----------------------------------------------------------------------------------------- --
--
-- Helper Functions
--
-- ----------------------------------------------------------------------------------------- --
-- readAction  :  read Action from String

readAction :: [TxsDefs.ChanId] -> String -> IOS.IOS TxsDDefs.Action
readAction chids args  =  do
     uid              <- gets IOS.uid
     tdefs            <- gets IOS.tdefs
     sigs             <- gets IOS.sigs
     vals             <- gets IOS.locvals
     ((uid',offs'),e) <- lift $ lift $ catch
                           ( let p = TxsHappy.prefoffsParser
                                    (  ( TxsAlex.Ctdefs   $ tdefs )
                                     : ( TxsAlex.Csigs    $ sigs )
                                     : ( TxsAlex.Cchanenv $ chids )
                                     : ( TxsAlex.Cvarenv  $ Map.keys vals )
                                     : ( TxsAlex.Cunid    $ uid + 1 )
                                     : ( TxsAlex.txsLexer args )
                                    )
                              in return $!! (p,"")
                           )
                           ( \e -> return $ ((uid,Set.empty)
                                            ,(show (e::ErrorCall))
                                            )
                           )
     if  e /= ""
       then do IFS.nack "ERROR" [ "incorrect action: " ++ e ]
               return $ TxsDDefs.ActQui
       else do
         modify $ \env -> env { IOS.uid = uid' }
         qstnoffs <- return [ q | q@(TxsDefs.Quest vid)
                                    <- concat $ map TxsDefs.chanoffers (Set.toList offs') ]
         if  not $ null qstnoffs
           then do IFS.nack "ERROR" [ "incorrect action: no question mark offer allowed" ]
                   return $ TxsDDefs.ActQui
           else do
             acts <- lift $ sequence
                          $ [ Utils.liftP2 (chid, sequence [ TxsCore.txsEval vexp
                                                           | TxsDefs.Exclam vexp <- choffs
                                                           ]
                                           )
                            | TxsDefs.Offer chid choffs <- Set.toList offs'
                            ]
             return $ TxsDDefs.Act (Set.fromList acts)


-- * Configuration related functions.

data Config = Config
  { smtSolver :: !CoreConfig.SMTSolver
  , smtLog :: !Bool
  , portNumber :: !PortNumber
  }

-- | Uninterpreted configuration options.
data UnintConfig = UnintConfig
  { mSmtSolver :: !(Maybe CoreConfig.SMTSolver)
  , mSmtLog :: !(Maybe Bool)
  , mPortNumber :: !(Maybe PortNumber)
  }

type Error = String

-- TODO: change `Either` to `Validation`.
interpretConfig :: CmdLineConfig -> Either [Error] Config
interpretConfig cfg = 
  Right $ Config
  { smtSolver = clSmtSolver cfg
  , smtLog = clSmtLog cfg
  , portNumber = clPortNumber cfg
  }

-- | Load the configuration options. These options can be specified by
-- different means:
--
--     1. Command line arguments.
--     2. Configuration file.
--     3. Environment variables.
--
-- The configuration options are searched in the order specified, and the first
-- option found is used.
--
-- For now we only parse command line arguments.
loadConfig :: IO CmdLineConfig
loadConfig = parseCmdLine

-- | File name to look for.
configFileName = "txs.yaml"

-- | Create a `UnintConfig` value by trying to read the configuration options
-- given in a configuration file. The configuration file is assumed to be named
-- as defined by the variable `configFileName`.
--
-- This function looks for the configuration file in the following places:
--
--     1. The current working directory.
--     2. The home directory
--
-- The search proceeds in the order listed above, and it stops as soon as a
-- configuration file is found.
--
loadConfigFromFile :: IO UnintConfig
loadConfigFromFile = undefined

