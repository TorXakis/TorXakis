{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}

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

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.State
import qualified Data.Char           as Char
import qualified Data.List           as List
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           Network
import           System.IO

-- import from local
import           CmdLineParser
import           ToProcdef
import qualified TxsServerConfig     as SC

-- import from serverenv
import qualified EnvServer           as IOS
import qualified IfServer            as IFS

-- import from core
import qualified BuildInfo
import qualified TxsCore
import qualified VersionInfo

-- import from defs
import qualified TxsDDefs
import qualified TxsDefs
import qualified TxsShow
import qualified Utils
import qualified VarId

-- import from front
import qualified TxsAlex
import qualified TxsHappy

-- import from cnect
import SocketWorld

main :: IO ()
main = withSocketsDo $ do
  hSetBuffering stderr NoBuffering     -- alt: LineBuffering

  uConfig <- SC.loadConfig

  case SC.interpretConfig uConfig of
    Left xs -> do
      hPutStrLn stderr "Errors found while loading the configuration"
      hPrint stderr xs
    Right config -> do
      let portNr = (clPortNumber . SC.cmdLineCfg) uConfig
      servsock      <- listenOn (PortNumber portNr)
      (hs, host, _) <- accept servsock
      hSetBuffering hs LineBuffering
      hSetEncoding hs latin1
      hPutStrLn stderr "\nTXSSERVER >>  Starting  ..... \n"
      let initS = IOS.envsNone
            { IOS.host   = host
            , IOS.portNr = portNr
            , IOS.servhs = hs
            }
          coreConfig = config
      TxsCore.runTxsCore coreConfig cmdsIntpr initS
      threadDelay 1000000    -- 1 sec delay on closing
      sClose servsock
      hPutStrLn stderr "\nTXSSERVER >>  Closing  ..... \n"

-- * TorXakis server commands processing

cmdsIntpr :: IOS.IOS ()
cmdsIntpr = do
     modus      <- gets IOS.modus
     (cmd, args) <- IFS.getCmd
     case cmd of
-- ----------------------------------------------------------------------------------- modus --
       "START"     |       IOS.isNoned    modus  ->  cmdStart     args
       "START"     | not $ IOS.isNoned    modus  ->  cmdNoop      cmd
       "QUIT"      ->  cmdQuit      args
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
       _           ->  cmdUnknown   cmd


-- ----------------------------------------------------------------------------------------- --
-- torxakis server individual command processing

-- ----------------------------------------------------------------------------------------- --

cmdNoop :: String -> IOS.IOS ()
cmdNoop cmd = do
     IFS.nack cmd [ "NoOp : No action"]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdUnknown :: String -> IOS.IOS ()
cmdUnknown cmd = do
     IFS.nack "ERROR" [ "Unknown command : " ++ cmd ]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStart :: String -> IOS.IOS ()
cmdStart _ = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned
         -> do modify $ \env -> env { IOS.modus = IOS.Idled }
               host <- gets IOS.host
               port <- gets IOS.portNr
               IFS.pack "START" ["txsserver starting:  " ++ show host ++ " : " ++ show port]
               cmdsIntpr
       _ -> do IFS.nack "START" [ "only in Noned modus" ]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdQuit :: String -> IOS.IOS ()
cmdQuit _ = do
     envs <- get
     case IOS.modus envs of
       _ -> do modify $ \env -> env { IOS.modus = IOS.Noned }
               host <- gets IOS.host
               port <- gets IOS.portNr
               IFS.pack "QUIT" ["txsserver closing  " ++ show host ++ " : " ++ show port]
               return ()

-- ----------------------------------------------------------------------------------------- --

cmdInit :: String -> IOS.IOS ()
cmdInit args = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned  
         -> do IFS.nack "INIT" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled 
         -> do servhs      <- gets IOS.servhs
               unid        <- gets IOS.uid
               tdefs       <- gets IOS.tdefs
               sigs        <- gets IOS.sigs
               srctxts     <- lift $ lift $ mapM readFile (words args)
               let srctxt   = List.intercalate "\n\n" srctxts
               ((unid',tdefs', sigs'),e)
                           <- lift $ lift $ catch
                                ( let parsing = TxsHappy.txsParser (TxsAlex.txsLexer srctxt)
                                   in return $!! (parsing, "")
                                )
                                ( \e -> return ((unid, tdefs, sigs), show (e::ErrorCall)) )
               if e /= ""
                 then do IFS.nack "INIT" [e]
                         cmdsIntpr
                 else do modify $ \env -> env { IOS.modus  = IOS.Inited
                                              , IOS.uid    = unid'
                                              , IOS.tdefs  = tdefs'
                                              , IOS.sigs   = sigs'
                                              }
                         lift $ TxsCore.txsInit tdefs' sigs' ( IFS.hmack servhs . map TxsShow.pshow )
                         IFS.pack "INIT" [ "input files parsed:", unwords (words args) ]
                         cmdsIntpr
       IOS.Inited
         -> do cmdTermit args
               cmdInit args
               cmdsIntpr
       _ -> do                                       -- IOS.Tested, IOS.Simuled, IOS.Stepped --
               cmdStop args
               cmdTermit args
               cmdInit args
               cmdsIntpr
          
-- ----------------------------------------------------------------------------------------- --

cmdTermit :: String -> IOS.IOS ()
cmdTermit args = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned  
         -> do IFS.nack "TERMIT" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled 
         -> cmdsIntpr
       IOS.Inited 
         -> do modify $ \env -> env { IOS.modus  = IOS.Idled
                                    , IOS.tdefs  = TxsDefs.empty
                                    , IOS.tow    = ( Nothing, Nothing, [] )
                                    , IOS.frow   = ( Nothing, [],      [] )
                                    }
               lift TxsCore.txsTermit
               IFS.pack "TERMIT" []
               cmdsIntpr
       _ -> do                                       -- IOS.Tested, IOS.Simuled, IOS.Stepped --
               cmdStop args
               cmdTermit args
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStop :: String -> IOS.IOS ()
cmdStop _ = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned  
         -> do IFS.nack "STOP" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled 
         -> cmdsIntpr
       IOS.Inited
         -> cmdsIntpr
       IOS.Stepped
         -> do modify $ \env -> env { IOS.modus   = IOS.Inited
                                    , IOS.tow     = ( Nothing, Nothing, [] )
                                    , IOS.frow    = ( Nothing, [],      [] )
                                    }
               lift TxsCore.txsStopNOEW
               IFS.pack "STOP" []
               cmdsIntpr
       _ -> do                                                    -- IOS.Tested, IOS.Simuled --
               modify $ \env -> env { IOS.modus   = IOS.Inited
                                    , IOS.tow     = ( Nothing, Nothing, [] )
                                    , IOS.frow    = ( Nothing, [],      [] )
                                    }
               envs' <- lift $ TxsCore.txsStopEW envs
               put envs'
               IFS.pack "STOP" []
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdInfo :: String -> IOS.IOS ()
cmdInfo _ = do
     envs <- get
     case IOS.modus envs of
       _ -> do IFS.pack "INFO" [ "TorXakis version    : " ++ VersionInfo.version
                               , "Build time          : " ++ BuildInfo.buildTime
                               ]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdParam :: String -> IOS.IOS ()
cmdParam args = do
     envs <- get
     case IOS.modus envs of
       _ -> case words args of
              []        -> do params1 <- lift TxsCore.txsGetParams
                              params2 <- IOS.getParams []
                              mapM_ IFS.mack [ [ nm ++ " = " ++ val ]
                                             | (nm,val) <- params1 ++ params2
                                             ]
                              IFS.pack "PARAM" []
                              cmdsIntpr
              [prm]     -> do params1 <- lift $ TxsCore.txsGetParam prm
                              params2 <- IOS.getParams [prm]
                              case params1++params2 of
                                []         -> IFS.nack "PARAM" [ "No parameter: " ++ prm ]
                                [(_,val)]  -> IFS.pack "PARAM" [ prm++" = "++val ]
                                _          -> IFS.nack "PARAM" [ "More parameters: " ++ prm ]
                              cmdsIntpr
              [prm,val] -> do params1 <- lift $ TxsCore.txsSetParam prm val
                              params2 <- IOS.setParams [(prm,val)]
                              case params1++params2 of
                                []         -> IFS.nack "PARAM" [ "No parameter: " ++ prm ]
                                [(_,val')] -> IFS.pack "PARAM" [ prm++" = "++val' ]
                                _          -> IFS.nack "PARAM" [ "More parameters: " ++ prm ]
                              cmdsIntpr
              _         -> do IFS.nack "PARAM" [ "Unknown parameter action" ]
                              cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSeed :: String -> IOS.IOS ()
cmdSeed args = do
     envs <- get
     case IOS.modus envs of
       _ -> case words args of
              [val] -> let seed = read val
                        in do lift $ TxsCore.txsSetSeed seed
                              IFS.pack "SEED" []
                              cmdsIntpr
              _     -> do IFS.nack "SEED" [ "Incorrect seed" ]
                          cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdVar :: String -> IOS.IOS ()
cmdVar args = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned
         -> do IFS.nack "VAR" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled
         -> do IFS.nack "VAR" [ "not in Idled modus" ]
               cmdsIntpr
       _ -> do                           -- IOS.Inited, IOS.Tested, IOS.Simuled, IOS.Stepped --
               let uid          = IOS.uid envs
                   tdefs        = IOS.tdefs envs
                   sigs         = IOS.sigs envs
                   vars         = IOS.locvars envs
                   vals         = IOS.locvals envs
               if  args == ""
                 then do
                   IFS.pack "VAR" [ TxsShow.fshow vars ]
                   cmdsIntpr
                 else do
                   ((uid',vars'),e) <- lift $ lift $ catch
                                         ( let p = TxsHappy.vardeclsParser
                                                     ( TxsAlex.Ctdefs tdefs
                                                     : TxsAlex.Csigs sigs
                                                     : TxsAlex.Cunid (uid + 1)
                                                     : TxsAlex.txsLexer args
                                                     )
                                            in return $!! (p,"")
                                         )
                                         ( \e -> return ((uid,[]),show (e::ErrorCall)))
                   if  e /= ""
                     then do
                       modify $ \envs' -> envs' { IOS.uid = uid' }
                       IFS.nack "VAR" [ e ]
                       cmdsIntpr
                     else
                       if  let newnames = map VarId.name vars'
                            in null ( newnames `List.intersect` map VarId.name vars ) &&
                               null ( newnames `List.intersect` map VarId.name (Map.keys vals))
                         then do
                           modify $ \envs' -> envs' { IOS.locvars = vars ++ vars'
                                                    , IOS.uid  = uid'
                                                    }
                           IFS.pack "VAR" [ TxsShow.fshow vars' ]
                           cmdsIntpr
                         else do
                           modify $ \envs' -> envs' { IOS.uid = uid' }
                           IFS.nack "VAR" [ "double variable names: " ++ TxsShow.fshow vars' ]
                           cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdVal :: String -> IOS.IOS ()
cmdVal args = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned
         -> do IFS.nack "VAL" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled
         -> do IFS.nack "VAL" [ "not in Idled modus" ]
               cmdsIntpr
       _ -> do                           -- IOS.Inited, IOS.Tested, IOS.Simuled, IOS.Stepped --
               let uid          = IOS.uid envs
                   tdefs        = IOS.tdefs envs
                   sigs         = IOS.sigs envs
                   vars         = IOS.locvars envs
                   vals         = IOS.locvals envs
               if  args == ""
                 then do
                   IFS.pack "VAL" [ TxsShow.fshow vals ]
                   cmdsIntpr
                 else do
                   ((uid',venv'),e) <- lift $ lift $ catch
                                         ( let p = TxsHappy.valdefsParser
                                                     ( TxsAlex.Ctdefs tdefs
                                                     : TxsAlex.Csigs sigs
                                                     : TxsAlex.Cvarenv []
                                                     : TxsAlex.Cunid (uid + 1)
                                                     : TxsAlex.txsLexer args
                                                     )
                                            in return $!! (p,"")
                                         )
                                         ( \e -> return ((uid,Map.empty),show (e::ErrorCall)))
                   if  e /= ""
                     then do
                       modify $ \envs' -> envs' { IOS.uid = uid' }
                       IFS.nack "VAL" [ e ]
                       cmdsIntpr
                     else
                       if let newnames = map VarId.name (Map.keys venv')
                           in null (newnames `List.intersect` map VarId.name vars) &&
                              null (newnames `List.intersect` map VarId.name (Map.keys vals))
                         then do
                           modify $ \envs' -> envs' { IOS.locvals = vals `Map.union` venv'
                                                    , IOS.uid     = uid'
                                                    }
                           IFS.pack "VAL" [ TxsShow.fshow venv' ]
                           cmdsIntpr
                         else do
                           modify $ \envs' -> envs' { IOS.uid = uid' }
                           IFS.nack "VAR" [ "double value names: " ++ TxsShow.fshow venv' ]
                           cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdEval :: String -> IOS.IOS ()
cmdEval args = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned
         -> do IFS.nack "EVAL" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled
         -> do IFS.nack "EVAL" [ "not in Idled modus" ]
               cmdsIntpr
       _ -> do                           -- IOS.Inited, IOS.Tested, IOS.Simuled, IOS.Stepped --
               let uid          = IOS.uid envs
                   tdefs        = IOS.tdefs envs
                   sigs         = IOS.sigs envs
                   vals         = IOS.locvals envs
                   vars         = IOS.locvars envs
               ((uid',vexp'),e) <- lift $ lift $ catch
                                     ( let p = TxsHappy.vexprParser
                                                  ( TxsAlex.Ctdefs   tdefs
                                                  : TxsAlex.Csigs    sigs
                                                  : TxsAlex.Cvarenv (Map.keys vals ++ vars)
                                                  : TxsAlex.Cunid   (uid + 1)
                                                  : TxsAlex.txsLexer args
                                                  )
                                        in return $!! (p,"")
                                     )
                                     ( \e -> return ( (uid,TxsDefs.cstrError "")
                                                    , show (e::ErrorCall)
                                     )              )
               if  e /= ""
                 then do modify $ \envs' -> envs' { IOS.uid = uid' }
                         IFS.nack "EVAL" [ e ]
                         cmdsIntpr
                 else do modify $ \envs' -> envs' { IOS.uid = uid' }
                         walue <- lift $ TxsCore.txsEval (TxsDefs.cstrEnv vals vexp')
                         IFS.pack "EVAL" [ TxsShow.fshow walue ]
                         cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSolve :: String -> String -> IOS.IOS ()
cmdSolve args kind = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned
         -> do IFS.nack "SOLVE" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled
         -> do IFS.nack "SOLVE" [ "not in Idled modus" ]
               cmdsIntpr
       _ -> do                           -- IOS.Inited, IOS.Tested, IOS.Simuled, IOS.Stepped --
               (cmd,solver) <- case kind of
                                 "sol" -> return ( "SOLVE"   , TxsCore.txsSolve    )
                                 "uni" -> return ( "UNISOLVE", TxsCore.txsUniSolve )
                                 "ran" -> return ( "RANSOLVE", TxsCore.txsRanSolve )
                                 _     -> do IFS.nack "SOLVE" ["Illegal kind of solver: " ++
                                                         show kind ++ "; using 'sol' instead" ]
                                             return ( "SOLVE"   , TxsCore.txsSolve    )
               let uid          = IOS.uid envs
                   tdefs        = IOS.tdefs envs
                   sigs         = IOS.sigs envs
                   vars         = IOS.locvars envs
                   vals         = IOS.locvals envs
               ((uid',vexp'),e) <- lift $ lift $ catch
                                     ( let p = TxsHappy.vexprParser
                                                 ( TxsAlex.Ctdefs tdefs
                                                 : TxsAlex.Csigs sigs
                                                 : TxsAlex.Cvarenv (Map.keys vals ++ vars)
                                                 : TxsAlex.Cunid (uid + 1)
                                                 : TxsAlex.txsLexer args
                                                 )
                                        in return $!! (p,"")
                                     )
                                     ( \e -> return ( (uid,TxsDefs.cstrError "")
                                                    , show (e::ErrorCall)
                                     )              )
               if  e /= ""
                 then do modify $ \envs' -> envs' { IOS.uid = uid' }
                         IFS.nack cmd [ e ]
                         cmdsIntpr
                 else do modify $ \envs' -> envs' { IOS.uid = uid' }
                         sols  <- lift $ solver (TxsDefs.cstrEnv vals vexp')
                         IFS.pack cmd [ TxsShow.fshow sols ]
                         cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTester :: String -> IOS.IOS ()
cmdTester args = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned
         -> do IFS.nack "TESTER" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled
         -> do IFS.nack "TESTER" [ "not in Idled modus" ]
               cmdsIntpr
       IOS.Inited
         -> do let tdefs = IOS.tdefs envs
               case words args of
                 [m,c] -> do
                   let mdefs = [ mdef
                               | (TxsDefs.ModelId nm _, mdef) <- Map.toList (TxsDefs.modelDefs tdefs)
                               , T.unpack nm == m
                               ]
                       cdefs = [ cdef
                               | (TxsDefs.CnectId nm _, cdef) <- Map.toList (TxsDefs.cnectDefs tdefs)
                               , T.unpack nm == c
                               ]
                   case (mdefs,cdefs) of
                     ([modeldef],[cnectdef])
                           | isConsistentTester modeldef Nothing Nothing cnectdef
                       -> do modify $ \env -> env { IOS.modus = IOS.Tested cnectdef }
                             envs' <- lift $ TxsCore.txsSetTest envs modeldef Nothing Nothing
                             put envs'
                             IFS.pack "TESTER" []
                             cmdsIntpr
                     _ -> do IFS.nack "TESTER" [ "Wrong or inconsistent parameters" ]
                             cmdsIntpr
                 [m,x,c] -> do
                   let mdefs = [ mdef
                               | (TxsDefs.ModelId  nm _, mdef) <- Map.toList (TxsDefs.modelDefs tdefs)
                               , T.unpack nm == m
                               ]
                       adefs = [ adef
                               | (TxsDefs.MapperId nm _, adef) <- Map.toList (TxsDefs.mapperDefs tdefs)
                               , T.unpack nm == x
                               ]
                       pdefs = [ pdef
                               | (TxsDefs.PurpId   nm _, pdef) <- Map.toList (TxsDefs.purpDefs tdefs)
                               , T.unpack nm == x
                               ]
                       cdefs = [ cdef
                               | (TxsDefs.CnectId  nm _, cdef) <- Map.toList (TxsDefs.cnectDefs tdefs)
                               , T.unpack nm == c
                               ]
                   case (mdefs,adefs,pdefs,cdefs) of
                     ([modeldef],[mapperdef],[],[cnectdef])
                           | isConsistentTester modeldef (Just mapperdef) Nothing cnectdef
                       -> do modify $ \env -> env { IOS.modus = IOS.Tested cnectdef }
                             envs' <- lift $ TxsCore.txsSetTest envs modeldef (Just mapperdef) Nothing
                             put envs'
                             IFS.pack "TESTER" []
                             cmdsIntpr
                     ([modeldef],[],[purpdef],[cnectdef])
                           | isConsistentTester modeldef Nothing (Just purpdef) cnectdef
                       -> do modify $ \env -> env { IOS.modus = IOS.Tested cnectdef }
                             envs' <- lift $ TxsCore.txsSetTest envs modeldef Nothing (Just purpdef)
                             put envs'
                             IFS.pack "TESTER" [ ]
                             cmdsIntpr
                     _ -> do IFS.nack "TESTER" [ "Wrong or inconsistent parameters" ]
                             cmdsIntpr
                 [m,x,y,c] -> do
                   let mdefs = [ mdef
                               | (TxsDefs.ModelId  nm _, mdef) <- Map.toList (TxsDefs.modelDefs tdefs)
                               , T.unpack nm == m
                               ]
                       adefs = [ adef
                               | (TxsDefs.MapperId nm _, adef) <- Map.toList (TxsDefs.mapperDefs tdefs)
                               , T.unpack nm == x || T.unpack nm == y
                               ]
                       pdefs = [ pdef
                               | (TxsDefs.PurpId   nm _, pdef) <- Map.toList (TxsDefs.purpDefs tdefs)
                               , T.unpack nm == x || T.unpack nm == y
                               ]
                       cdefs = [ cdef
                               | (TxsDefs.CnectId  nm _, cdef) <- Map.toList (TxsDefs.cnectDefs tdefs)
                               , T.unpack nm == c
                               ]
                   case (mdefs,adefs,pdefs,cdefs) of
                     ([modeldef],[mapperdef],[purpdef],[cnectdef])
                           | isConsistentTester modeldef (Just mapperdef) (Just purpdef) cnectdef
                       -> do modify $ \env -> env { IOS.modus = IOS.Tested cnectdef }
                             envs' <- lift $ TxsCore.txsSetTest envs modeldef (Just mapperdef) (Just purpdef)
                             put envs'
                             IFS.pack "TESTER" [ ]
                             cmdsIntpr
                     _ -> do IFS.nack "TESTER" [ "Wrong or inconsistent parameters" ]
                             cmdsIntpr
                 _ -> do IFS.nack "TESTER" [ "Illegal arguments: " ++ show args ]
                         cmdsIntpr
       _ -> do                                       -- IOS.Tested, IOS.Simuled, IOS.Stepped --
               cmdStop args
               cmdTester args

isConsistentTester :: TxsDefs.ModelDef ->
                      Maybe TxsDefs.MapperDef ->
                      Maybe TxsDefs.PurpDef ->
                      TxsDefs.CnectDef ->
                      Bool

isConsistentTester (TxsDefs.ModelDef minsyncs moutsyncs _ _)
                   Nothing
                   _
                   (TxsDefs.CnectDef _ conndefs)
 = let { mins   = Set.fromList minsyncs
         ; mouts  = Set.fromList moutsyncs
         ; ctows  = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDtoW  chan _ _ _ _ <- conndefs ]
         ; cfrows = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDfroW chan _ _ _ _ <- conndefs ]
         }
      in    mins   == ctows
         && cfrows == mouts

-- why aren't Model and Mapper checked for consistency?
isConsistentTester _
                   (Just (TxsDefs.MapperDef achins achouts asyncsets _))
                   _
                   (TxsDefs.CnectDef _ conndefs)
 = let { ctows  = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDtoW  chan _ _ _ _ <- conndefs ]
         ; cfrows = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDfroW chan _ _ _ _ <- conndefs ]
         ; ains   = Set.fromList $ filter (not . Set.null)
                        [ sync `Set.intersection` Set.fromList achins  | sync <- asyncsets ]
         ; aouts  = Set.fromList $ filter (not . Set.null)
                        [ sync `Set.intersection` Set.fromList achouts | sync <- asyncsets ]
         }
      in    cfrows `Set.isSubsetOf` ains
         && ctows  `Set.isSubsetOf` aouts

-- ----------------------------------------------------------------------------------------- --

cmdSimulator :: String -> IOS.IOS ()
cmdSimulator args = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned
         -> do IFS.nack "SIMULATOR" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled
         -> do IFS.nack "SIMULATOR" [ "not in Idled modus" ]
               cmdsIntpr
       IOS.Inited
         -> do let tdefs = IOS.tdefs envs
               case words args of
                 [m,c] -> do
                   let mdefs = [ mdef
                               | (TxsDefs.ModelId nm _, mdef) <- Map.toList (TxsDefs.modelDefs tdefs)
                               , T.unpack nm == m
                               ]
                       cdefs = [ cdef
                               | (TxsDefs.CnectId nm _, cdef) <- Map.toList (TxsDefs.cnectDefs tdefs)
                               , T.unpack nm == c
                               ]
                   case (mdefs,cdefs) of
                     ([modeldef],[cnectdef])
                           | isConsistentSimulator modeldef Nothing cnectdef
                       -> do modify $ \env -> env { IOS.modus = IOS.Simuled cnectdef }
                             envs' <- lift $ TxsCore.txsSetSim envs modeldef Nothing
                             put envs'
                             IFS.pack "SIMULATOR" []
                             cmdsIntpr
                     _ -> do IFS.nack "SIMULATOR" [ "Wrong or inconsistent parameters" ]
                             cmdsIntpr
                 [m,a,c] -> do
                   let mdefs = [ mdef
                               | (TxsDefs.ModelId nm _, mdef) <- Map.toList (TxsDefs.modelDefs tdefs)
                               , T.unpack nm == m
                               ]
                       adefs = [ adef
                               | (TxsDefs.MapperId nm _, adef) <- Map.toList (TxsDefs.mapperDefs tdefs)
                               , T.unpack nm == a
                               ]
                       cdefs = [ cdef
                               | (TxsDefs.CnectId nm _, cdef) <- Map.toList (TxsDefs.cnectDefs tdefs)
                               , T.unpack nm == c
                               ]
                   case (mdefs,adefs,cdefs) of
                     ([modeldef],[mapperdef],[cnectdef])
                           | isConsistentSimulator modeldef (Just mapperdef) cnectdef
                       -> do modify $ \env -> env { IOS.modus = IOS.Simuled cnectdef }
                             envs' <- lift $ TxsCore.txsSetSim envs modeldef (Just mapperdef)
                             put envs'
                             IFS.pack "SIMULATOR" []
                             cmdsIntpr
                     _ -> do IFS.nack "SIMULATOR" [ "Wrong or inconsistent parameters" ]
                             cmdsIntpr
                 _ -> do IFS.nack "SIMULATOR" [ "Illegal arguments " ++ show args ]
                         cmdsIntpr
       _ -> do                                       -- IOS.Tested, IOS.Simuled, IOS.Stepped --
               cmdStop args
               cmdTester args

isConsistentSimulator :: TxsDefs.ModelDef ->
                         Maybe TxsDefs.MapperDef ->
                         TxsDefs.CnectDef ->
                         Bool

isConsistentSimulator (TxsDefs.ModelDef minsyncs moutsyncs _ _)
                      Nothing
                      (TxsDefs.CnectDef _ conndefs)
 = let { mins   = Set.fromList minsyncs
         ; mouts  = Set.fromList moutsyncs
         ; ctows  = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDtoW  chan _ _ _ _ <- conndefs ]
         ; cfrows = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDfroW chan _ _ _ _ <- conndefs ]
         }
      in    mins  == cfrows
         && mouts == ctows

isConsistentSimulator _
                      (Just (TxsDefs.MapperDef achins achouts asyncsets _))
                      (TxsDefs.CnectDef _ conndefs)
 = let { ctows  = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDtoW  chan _ _ _ _ <- conndefs ]
         ; cfrows = Set.fromList
                        [ Set.singleton chan | TxsDefs.ConnDfroW chan _ _ _ _ <- conndefs ]
         ; ains   = Set.fromList $ filter (not . Set.null)
                        [ sync `Set.intersection` Set.fromList achins  | sync <- asyncsets ]
         ; aouts  = Set.fromList $ filter (not . Set.null)
                        [ sync `Set.intersection` Set.fromList achouts | sync <- asyncsets ]
         }
      in    cfrows `Set.isSubsetOf` ains
         && ctows  `Set.isSubsetOf` aouts

-- ----------------------------------------------------------------------------------------- --

cmdStepper :: String -> IOS.IOS ()
cmdStepper args = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned
         -> do IFS.nack "STEPPER" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled
         -> do IFS.nack "STEPPER" [ "not in Idled modus" ]
               cmdsIntpr
       IOS.Inited
         -> do let tdefs = IOS.tdefs envs
                   mdefs = TxsDefs.modelDefs tdefs
               case words args of
                 [m] -> do
                    let mdefs' = [ mdef
                                   | (TxsDefs.ModelId nm _, mdef) <- Map.toList mdefs
                                   , T.unpack nm == m
                                   ]
                    case mdefs' of
                      [modeldef] -> do modify $ \env -> env { IOS.modus = IOS.Stepped }
                                       lift $ TxsCore.txsSetStep modeldef
                                       IFS.pack "STEPPER" []
                                       cmdsIntpr
                      _          -> do IFS.nack "STEPPER" [ "Wrong or inconsistent parameters" ]
                                       cmdsIntpr
                 _ -> do IFS.nack "SIMULATOR" [ "Illegal arguments " ++ show args ]
                         cmdsIntpr
       _ -> do                                       -- IOS.Tested, IOS.Simuled, IOS.Stepped --
               cmdStop args
               cmdTester args

-- ----------------------------------------------------------------------------------------- --

cmdTest :: String -> IOS.IOS ()
cmdTest args = do
     envs <- get
     case IOS.modus envs of
       IOS.Tested cdef
         -> do case words args of
                 [] -> do                                              -- observe one output --
                          verdict <-lift TxsCore.txsTestOut
                          IFS.pack "TEST" [TxsShow.fshow verdict]
                          cmdsIntpr
                 [d] | all Char.isDigit d                           -- d::int i/o test steps --
                    -> do verdict <- lift $ TxsCore.txsTestN (read d)
                          IFS.pack "TEST" [TxsShow.fshow verdict]
                          cmdsIntpr
                 _  -> do                                        -- do given action as input --
                          IOS.Tested (TxsDefs.CnectDef _ conndefs) <- gets IOS.modus
                          let ctows  =  [ chan | TxsDefs.ConnDtoW  chan _ _ _ _ <- conndefs ]
                          act <- readAction ctows args
                          if  act == TxsDDefs.ActQui
                            then cmdsIntpr
                            else do verdict <- lift $ TxsCore.txsTestIn act
                                    IFS.pack "TEST" [TxsShow.fshow verdict]
                                    cmdsIntpr
       _ -> do                 -- IOS.Noned, IOS.Idled, IOS.Inited, IOS.Simuled, IOS.Stepped --
               IFS.nack "TEST" [ "only in Tested modus" ]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSim :: String -> IOS.IOS ()
cmdSim args = do
     envs <- get
     case IOS.modus envs of
       IOS.Simuled cdef
         -> do case words args of
                 [] -> do                                            -- no arg: infinite sim --
                          verdict <- lift $ TxsCore.txsSimN (-1)
                          IFS.pack "SIM" [TxsShow.fshow verdict]
                          cmdsIntpr
                 [d] | all Char.isDigit d                                -- d::int sim steps --
                    -> do verdict <- lift $ TxsCore.txsSimN (read d)
                          IFS.pack "SIM" [TxsShow.fshow verdict]
                          cmdsIntpr
                 _  -> do                                                -- not a valid call --
                          IFS.nack "SIM" ["wrong parameter"]
                          cmdsIntpr
       _ -> do                  -- IOS.Noned, IOS.Idled, IOS.Inited, IOS.Tested, IOS.Stepped --
               IFS.nack "SIM" [ "only in Simuled modus" ]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStep :: String -> IOS.IOS ()
cmdStep args = do
     envs <- get
     case IOS.modus envs of
       IOS.Stepped
         -> do case words args of
                 [] -> do                                                -- no arg: one step --
                          verdict <- lift $ TxsCore.txsStepN 1
                          IFS.pack "STEP" [TxsShow.fshow verdict]
                          cmdsIntpr
                 [d] | all Char.isDigit d                                    -- d::int steps --
                    -> do verdict <- lift $ TxsCore.txsStepN (read d)
                          IFS.pack "STEP" [TxsShow.fshow verdict]
                          cmdsIntpr
                 _  -> do                                         -- action arg: step action --
                          let tdefs = IOS.tdefs envs
                              mdefs = TxsDefs.modelDefs tdefs
                              chids = Set.toList $ Set.unions
                                            [ Set.unions (chins ++ chouts ++ spls)
                                            | (_, TxsDefs.ModelDef chins chouts spls _)
                                              <- Map.toList mdefs
                                            ]
                          act <- readAction chids args
                          if  act == TxsDDefs.ActQui
                            then cmdsIntpr
                            else do verdict <- lift $ TxsCore.txsStepA act
                                    IFS.pack "STEP" [TxsShow.fshow verdict]
                                    cmdsIntpr
       _ -> do                  -- IOS.Noned, IOS.Idled, IOS.Inited, IOS.Tested, IOS.Simuled --
               IFS.nack "STEP" [ "only in Stepped modus" ]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdShow :: String -> IOS.IOS ()
cmdShow args = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned
         -> do IFS.nack "SHOW" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled
         -> do IFS.nack "SHOW" [ "not in Idled modus" ]
               cmdsIntpr
       _ -> do                           -- IOS.Inited, IOS.Tested, IOS.Simuled, IOS.Stepped --
               txt <- case words args of
                        ["tdefs"] -> lift $ TxsCore.txsShow "tdefs"     ""
                        ["var"]   -> return $ TxsShow.fshow (IOS.locvars envs)
                        ["val"]   -> return $ TxsShow.fshow (IOS.locvals envs)
                        ["state"    ,"nr"    ] -> lift $ TxsCore.txsShow "state"     ""
                        ["state"    ,"model" ] -> lift $ TxsCore.txsShow "model"     ""
                        ["state"    ,"mapper"] -> lift $ TxsCore.txsShow "mapper"    ""
                        ["state"    ,"purp"  ] -> lift $ TxsCore.txsShow "purp"      ""
                        ["modeldef" ,nm      ] -> lift $ TxsCore.txsShow "modeldef"  nm
                        ["mapperdef",nm      ] -> lift $ TxsCore.txsShow "mapperdef" nm
                        ["purpdef"  ,nm      ] -> lift $ TxsCore.txsShow "purpdef"   nm
                        ["procdef"  ,nm      ] -> lift $ TxsCore.txsShow "procdef"   nm
                        ["cnect"             ] -> return $ let (_, _, towhdls ) = IOS.tow envs
                                                               (_, _, frowhdls) = IOS.frow envs
                                                            in TxsShow.fshow (towhdls++frowhdls)
                        _         -> return ""
               case txt of
                 "" -> do IFS.nack "SHOW" ["nothing to be shown"]
                          cmdsIntpr
                 s  -> do IFS.mack [s]
                          IFS.pack "SHOW" ["\n"]
                          cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdGoTo :: String -> IOS.IOS ()
cmdGoTo args = do
     envs <- get
     case IOS.modus envs of
       IOS.Stepped
         -> do case words args of
                 []        -> do IFS.pack "GOTO" ["gone to current state"]
                                 cmdsIntpr
                 ["back"]  -> do lift $ TxsCore.txsGoTo (-1)
                                 IFS.pack "GOTO" ["gone to previous state"]
                                 cmdsIntpr
                 ["back",d] | all Char.isDigit d
                           -> let steps = read d
                               in if  steps == 0
                                    then do IFS.pack "GOTO" ["gone to current state"]
                                            cmdsIntpr
                                    else do lift $ TxsCore.txsGoTo (-steps)
                                            IFS.pack "GOTO" ["gone back " ++ show steps ++ " states"]
                                            cmdsIntpr
                 [d] | all Char.isDigit d
                           -> do lift $ TxsCore.txsGoTo (read d)
                                 IFS.pack "GOTO" ["gone to state " ++ d]
                                 cmdsIntpr
                 _         -> do IFS.nack "GOTO" ["unknown state"]
                                 cmdsIntpr
       _ -> do                  -- IOS.Noned, IOS.Idled, IOS.Inited, IOS.Tested, IOS.Simuled --
               IFS.nack "GOTO" [ "only in Stepped modus" ]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdPath :: String -> IOS.IOS ()
cmdPath _ = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned
         -> do IFS.nack "PATH" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled
         -> do IFS.nack "PATH" [ "not in Idled modus" ]
               cmdsIntpr
       IOS.Inited
         -> do IFS.nack "PATH" [ "not in Inited modus" ]
               cmdsIntpr
       _ -> do                                       -- IOS.Tested, IOS.Simuled, IOS.Stepped --
               path <- lift TxsCore.txsPath
               IFS.mack [ TxsShow.showN n 6 ++ ": " ++ TxsShow.fshow s1 ++ " -> " ++
                          unwords (lines $ TxsShow.fshow a) ++ " -> " ++ TxsShow.fshow s2
                        | (n,(s1,a,s2)) <- zip [1 ..] path
                        ]
               IFS.pack "PATH" ["\n"]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTrace :: String -> IOS.IOS ()
cmdTrace args = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned
         -> do IFS.nack "TRACE" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled
         -> do IFS.nack "TRACE" [ "not in Idled modus" ]
               cmdsIntpr
       IOS.Inited
         -> do IFS.nack "TRACE" [ "not in Inited modus" ]
               cmdsIntpr
       _ -> do                                       -- IOS.Tested, IOS.Simuled, IOS.Stepped --
               path  <- lift TxsCore.txsPath
               let trace = [ a | (_, a ,_) <- path ]
               case words args of
                 []       -> do IFS.mack [ TxsShow.showN n 6 ++ ":  " ++ TxsShow.fshow a
                                         | (n, (_, a, _)) <- zip [1..] path
                                         ]
                                IFS.pack "TRACE" ["\n"]
                                cmdsIntpr
                 ["proc"] -> do IFS.mack [T.unpack (toProcdef trace)]
                                IFS.pack "TRACE" ["\n"]
                                cmdsIntpr
                 ["purp"] -> do IFS.mack [T.unpack (toPurpdef trace)]
                                IFS.pack "TRACE" ["\n"]
                                cmdsIntpr
                 _        -> do IFS.nack "TRACE" [ "No such trace format" ]
                                cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdMenu :: String -> IOS.IOS ()
cmdMenu args = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned
         -> do IFS.nack "MENU" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled
         -> do IFS.nack "MENU" [ "not in Idled modus" ]
               cmdsIntpr
       IOS.Inited
         -> do IFS.nack "MENU" [ "not in Inited modus" ]
               cmdsIntpr
       _ -> do                                       -- IOS.Tested, IOS.Simuled, IOS.Stepped --
               let (kind,what) = case words args of
                                   ["in"]       -> ( "mod", "in" )
                                   ["out"]      -> ( "mod", "out" )
                                   ["map"]      -> ( "map", "" )
                                   ["purp",gnm] -> ( "purp", gnm )
                                   _            -> ( "mod", "all" )
                in do menu <- lift $ TxsCore.txsMenu kind what
                      IFS.mack [ TxsShow.fshow menu ]
                      IFS.pack "MENU" [ "\n" ]
                      cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdMap :: String -> IOS.IOS ()
cmdMap args = do
     envs <- get
     case IOS.modus envs of
       IOS.Noned
         -> do IFS.nack "MENU" [ "not in Noned modus" ]
               cmdsIntpr
       IOS.Idled
         -> do IFS.nack "MENU" [ "not in Idled modus" ]
               cmdsIntpr
       IOS.Inited
         -> do IFS.nack "MENU" [ "not in Inited modus" ]
               cmdsIntpr
       IOS.Stepped
         -> do IFS.nack "MENU" [ "not in Inited modus" ]
               cmdsIntpr
       _ -> do                                                    -- IOS.Tested, IOS.Simuled --
               let tdefs = IOS.tdefs envs
                   mdefs = TxsDefs.mapperDefs tdefs
                   inchids = concat [ chins
                                    | ( _, TxsDefs.MapperDef chins _ _ _ ) <- Map.toList mdefs
                                    ]
               if  null inchids
                 then do IFS.nack "MAP" [ "No mapper(s) defined" ]
                         cmdsIntpr
                 else do act <- readAction inchids args
                         if  act == TxsDDefs.ActQui
                           then do IFS.nack "MAP" [ "Not a valid action" ]
                                   cmdsIntpr
                           else do act' <- lift $ TxsCore.txsMapper act
                                   IFS.pack "MAP" [TxsShow.fshow act']
                                   cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdNComp :: String -> IOS.IOS ()
cmdNComp args = do
     envs <- get
     case IOS.modus envs of
       IOS.Inited
         -> do let tdefs = IOS.tdefs envs
               case words args of
                 [mname]
                   -> case [ mdef
                           | (TxsDefs.ModelId nm _,mdef) <- Map.toList(TxsDefs.modelDefs tdefs)
                           , T.unpack nm == mname
                           ] of
                        [mdef]
                          -> do mayPurpId <- lift $ TxsCore.txsNComp mdef
                                case mayPurpId of
                                  Just purpid
                                    -> do IFS.pack "NCOMP" [ "Test Purpose generated: "
                                                             ++ TxsShow.fshow purpid ]
                                          cmdsIntpr
                                  Nothing
                                    -> do IFS.nack "NCOMP" [ "Could not make test purpose" ]
                                          cmdsIntpr
                        _ -> do IFS.nack "NCOMP" [ "No such MODELDEF" ]
                                cmdsIntpr
                 _ -> do IFS.nack "NCOMP" [ "Argument must be one MODELDEF name" ]
                         cmdsIntpr
       _ -> do                 -- IOS.Noned, IOS.Idled, IOS.Tested, IOS.Simuled, IOS.Stepped --
               IFS.nack "NCOMP" [ "only in Inited modus" ]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --
--
-- Helper Functions
--
-- ----------------------------------------------------------------------------------------- --
-- readAction  :  read Action from String

readAction :: [TxsDefs.ChanId] -> String -> IOS.IOS TxsDDefs.Action
readAction chids args = do
     uid              <- gets IOS.uid
     tdefs            <- gets IOS.tdefs
     sigs             <- gets IOS.sigs
     vals             <- gets IOS.locvals
     ((uid',offs'),e) <- lift $ lift $ catch
                           ( let p = TxsHappy.prefoffsParser
                                    ( TxsAlex.Ctdefs   tdefs
                                    : TxsAlex.Csigs    sigs
                                    : TxsAlex.Cchanenv chids
                                    : TxsAlex.Cvarenv  (Map.keys vals)
                                    : TxsAlex.Cunid    (uid + 1)
                                    : TxsAlex.txsLexer args
                                    )
                              in return $!! (p,"")
                           )
                           ( \e -> return ((uid,Set.empty),show (e::ErrorCall)))
     if  e /= ""
       then do IFS.nack "ERROR" [ "incorrect action: " ++ e ]
               return TxsDDefs.ActQui
       else do
         modify $ \env -> env { IOS.uid = uid' }
         let qstnoffs  =  [ q | q@TxsDefs.Quest{}
                                    <- concatMap TxsDefs.chanoffers (Set.toList offs') ]
         if  not $ null qstnoffs
           then do IFS.nack "ERROR" [ "incorrect action: no question mark offer allowed" ]
                   return TxsDDefs.ActQui
           else do
             acts <- lift $ sequence
                            [ Utils.liftP2 (chid, sequence [ TxsCore.txsEval vexp
                                                           | TxsDefs.Exclam vexp <- choffs
                                                           ]
                                           )
                            | TxsDefs.Offer chid choffs <- Set.toList offs'
                            ]
             return $ TxsDDefs.Act (Set.fromList acts)

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

