{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- {-# LANGUAGE OverloadedStrings #-}

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

{-
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.State
import qualified Data.Char           as Char
import qualified Data.List           as List
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           Network             hiding (socketPort)
import           Network.Socket      hiding (accept, sClose)
import           System.IO

-- import from local
import           CmdLineParser
-- import           ToProcdef
import qualified TxsServerConfig     as SC

-- import from serverenv
import qualified EnvServer           as IOS
import qualified IfServer            as IFS

-- import from core
import qualified BuildInfo
import qualified TxsCore
import qualified TxsManual
import qualified VersionInfo

-- import from defs
import qualified TxsDDefs
import qualified TxsDefs
import qualified Sigs
import qualified TxsShow
import qualified Utils
-- import qualified VarId

-- import from valexpr
import           Id
-- import qualified ValExpr

-- import from front
import qualified TxsAlex
import qualified TxsHappy

-- import from cnect
import SockExplW
import SockImplW

-}

-- ----------------------------------------------------------------------------------------- --
-- main

main :: IO ()
main = return ()

{-

 withSocketsDo $ do
  hSetBuffering stderr NoBuffering     -- alt: LineBuffering
  hSetBuffering stdout LineBuffering
  uConfig <- SC.loadConfig

  case SC.interpretConfig uConfig of
    Left xs -> do
      hPutStrLn stderr "Errors found while loading the configuration"
      hPrint stderr xs
    Right config -> do
      (portNr, sock) <- txsListenOn $ (clPortNumber . SC.cmdLineCfg) uConfig
      (hs, host, _) <- accept sock
      hSetBuffering hs LineBuffering
      hSetEncoding hs latin1
      hPutStrLn stderr "\nTXSSERVER >>  Starting  ..... \n"
      let initS = IOS.envsNone
              { IOS.host   = host
              , IOS.portNr = portNr
              , IOS.servhs = hs
              , IOS.params = SC.updateParamVals -- updating parameters...
                              (IOS.params IOS.envsNone) -- ...defined in ServerEnv
                              $ SC.configuredParameters config
              }
          coreConfig = config
      TxsCore.runTxsCore coreConfig cmdsIntpr initS
      threadDelay 1000000    -- 1 sec delay on closing
      sClose sock
      hPutStrLn stderr "\nTXSSERVER >>  Closing  ..... \n"

-- | Listen on the given port. If no port number is given, then a free port is
-- determined, and this port number is printed to the standard output.
txsListenOn :: Maybe PortNumber -> IO (PortNumber, Socket)
txsListenOn Nothing = do -- Get a free port to listen to.
    sock <- listenOn (PortNumber aNY_PORT)
    portNr <- socketPort sock
    -- If no port was specified, then we print the port number in case the
    -- process that is starting 'txsserver' (most likely the 'torxakis'
    -- command) needs the port number to connect to it afterwards.
    print portNr
    return (portNr, sock)
txsListenOn (Just portNr) = do
    sock <- listenOn (PortNumber portNr)
    return (portNr, sock)

-- * TorXakis server commands processing


cmdsIntpr :: IOS.IOS ()
cmdsIntpr = do
     modus       <- gets IOS.modus
     (cmd, args) <- IFS.getCmd
     case (cmd, args) of
-- ----------------------------------------------------------------------------------- modus --
       ("START"    , "")   | IOS.isNoned    modus  ->  cmdStart
       ("START"    , _ )                           ->  cmdNoop      cmd
       ("QUIT"     , "")                           ->  cmdQuit
       ("QUIT"     , _ )                           ->  cmdNoop      cmd
       ("INIT"     , _ )   | IOS.isIdled    modus  ->  cmdInit      args
       ("INIT"     , _ )                           ->  cmdNoop      cmd
       ("TERMIT"   , "")   | IOS.isInited   modus  ->  cmdTermit
       ("TERMIT"   , _ )                           ->  cmdNoop      cmd
       ("STOP"     , "")   | IOS.isGtInited modus  ->  cmdStop
       ("STOP"     , _ )                           ->  cmdNoop      cmd
-- -------------------------------------------------------------------------------- settings --
       ("INFO"     , "")   | IOS.isGtNoned  modus  ->  cmdInfo
       ("INFO"     , _ )                           ->  cmdNoop      cmd
       ("PARAM"    , _ )   | IOS.isGtNoned  modus  ->  cmdParam     args
       ("PARAM"    , _ )                           ->  cmdNoop      cmd
       ("SEED"     , _ )   | IOS.isGtNoned  modus  ->  cmdSeed      args
       ("SEED"     , _ )                           ->  cmdNoop      cmd
-- ------------------------------------------------------------------------------------ data --
       ("VAR"      , _ )   | IOS.isGtIdled  modus  ->  cmdVar       args
       ("VAR"      , _ )                           ->  cmdNoop      cmd
       ("VAL"      , _ )   | IOS.isGtIdled  modus  ->  cmdVal       args
       ("VAL"      , _ )                           ->  cmdNoop      cmd
       ("EVAL"     , _ )   | IOS.isGtIdled  modus  ->  cmdEval      args
       ("EVAL"     , _ )                           ->  cmdNoop      cmd
       ("SOLVE"    , _ )   | IOS.isGtIdled  modus  ->  cmdSolve     args
       ("SOLVE"    , _ )                           ->  cmdNoop      cmd
-- ----- ------------------------------------------------------------------------------ exec --
       ("TESTER"   , _ )   | IOS.isInited   modus  ->  cmdTester    args
       ("TESTER"   , _ )                           ->  cmdNoop      cmd
       ("SIMULATOR", _ )   | IOS.isInited   modus  ->  cmdSimulator args
       ("SIMULATOR", _ )                           ->  cmdNoop      cmd
       ("STEPPER"  , _ )   | IOS.isInited   modus  ->  cmdStepper   args
       ("STEPPER"  , _ )                           ->  cmdNoop      cmd
       ("LEARNER"  , _ )   | IOS.isInited   modus  ->  cmdLearner   args
       ("LEARNER"  , _ )                           ->  cmdNoop      cmd
       ("MANUAL"   , _ )   | IOS.isInited   modus  ->  cmdManual    args
       ("MANUAL"   , _ )                           ->  cmdNoop      cmd
-- -------------------------------------------------------------------- test, simulate, step --
       ("TEST"     , _ )   | IOS.isTested   modus  ->  cmdTest      args
       ("TEST"     , _ )                           ->  cmdNoop      cmd
       ("SIM"      , _ )   | IOS.isSimuled  modus  ->  cmdSim       args
       ("SIM"      , _ )                           ->  cmdNoop      cmd
       ("STEP"     , _ )   | IOS.isStepped  modus  ->  cmdStep      args
       ("STEP"     , _ )                           ->  cmdNoop      cmd
       ("LEARN"    , _ )   | IOS.isLearned  modus  ->  cmdLearn     args
       ("LEARN"    , _ )                           ->  cmdNoop      cmd
       ("MAN"      , _ )   | IOS.isManualed modus  ->  cmdMan       args
       ("MAN"      , _ )                           ->  cmdNoop      cmd
-- ----------------------------------------------------------------------------- btree state --
       ("SHOW"     , _ )   | IOS.isGtIdled  modus  ->  cmdShow      args
       ("SHOW"     , _ )                           ->  cmdNoop      cmd
       ("GOTO"     , _ )   | IOS.isStepped  modus  ->  cmdGoTo      args
       ("GOTO"     , _ )                           ->  cmdNoop      cmd
       ("PATH"     , _ )   | IOS.isStepped  modus  ->  cmdPath      args

       ("TRACE"    , _ )                           ->  cmdNoop      cmd
       ("MENU"     , _ )   | IOS.isGtInited modus  ->  cmdMenu      args
       ("MENU"     , _ )                           ->  cmdNoop      cmd
       ("MAP"      , _ )   | IOS.isTested   modus  ->  cmdMap       args
       ("MAP"      , _ )   | IOS.isSimuled  modus  ->  cmdMap       cmd
       ("MAP"      , _ )                           ->  cmdNoop      cmd
       ("NCOMP"    , _ )   | IOS.isInited   modus  ->  cmdNComp     args
       ("NCOMP"    , _ )                           ->  cmdNoop      cmd
       ("LPE"      , _ )   | IOS.isInited   modus  ->  cmdLPE       args
       ("LPE"      , _ )                           ->  cmdNoop      cmd
       (_          , _ )                           ->  cmdUnknown   cmd

-}


-- ----------------------------------------------------------------------------------------- --
-- torxakis server individual command processing

-- ----------------------------------------------------------------------------------------- --


{-

cmdNoop :: String -> IOS.IOS ()
cmdNoop cmd = do
     IFS.nack cmd [ "inopportune command: " ++ cmd ]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdUnknown :: String -> IOS.IOS ()
cmdUnknown cmd = do
     IFS.nack cmd [ "unknown command: " ++ cmd ]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStart :: IOS.IOS ()
cmdStart = do                                                       -- PRE :  modus == Noned --
     modify $ \env -> env { IOS.modus = IOS.Idled }
     host <- gets IOS.host
     port <- gets IOS.portNr
     IFS.pack "START" ["txsserver starting:  " ++ show host ++ " : " ++ show port]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdQuit :: IOS.IOS ()
cmdQuit = do                                                             -- PRE :  any modus --
     modify $ \env -> env { IOS.modus = IOS.Noned }
     host <- gets IOS.host
     port <- gets IOS.portNr
     IFS.pack "QUIT" [ "txsserver closing  " ++ show host ++ " : " ++ show port ]
     return ()

-- ----------------------------------------------------------------------------------------- --

cmdInit :: String -> IOS.IOS ()
cmdInit args = do                                                   -- PRE :  modus == Idled --
     servhs             <- gets IOS.servhs
     unid               <- gets IOS.uid
     tdefs              <- lift TxsCore.txsGetTDefs
     sigs               <- gets IOS.sigs
     srctxts            <- lift $ lift $ mapM readFile (words args)
     let srctxt          = List.intercalate "\n\n" srctxts
     ((unid',tdefs', sigs'),e) <- lift $ lift $ catch
                             ( let parsing = TxsHappy.txsParser (TxsAlex.txsLexer srctxt)
                                in return $!! (parsing, "")
                             )
                             ( \e -> return ((unid, tdefs, sigs), show (e::ErrorCall)))
     if e /= ""
       then do IFS.nack "INIT" [e]
               cmdsIntpr
       else do modify $ \env -> env { IOS.modus  = IOS.Inited
                                    , IOS.uid    = unid'
                                    , IOS.sigs   = sigs'
                                    }
               lift $ TxsCore.txsInit tdefs' sigs' ( IFS.hmack servhs . map TxsShow.pshow )
               IFS.pack "INIT" ["input files parsed:", unwords (words args)]
               cmdsIntpr


     srctxts <- lift $ lift $ sequence
                  [ catch ( do srctxt <- readFile fname
                               return (fname,srctxt, "")
                          )
                          ( \e -> do let err = show (e :: IOException)
                                     return (fname, "",
                                             "Could not open open " ++ fname ++ ": " ++ err)
                          )
                  | fname <- [ (takeDirectory fpath) </> (takeBaseName fpath) <.> "txs"
                             |  fpath <- words args
                             ]
                  ]
     let fnames  = List.intercalate ", " (map frst srctxts)
         srctxt  = List.intercalate "\n\n" (map scnd srctxts)
         errtxts = map Utils.thrd srctxt
     if  not $ null errtxts
       then do IFS.nack "INIT" $ errtxts
               cmdsIntpr
       else do ((unid',tdefs',sigs'), e) <- lift $ lift $ catch
                                ( let parsing = TxsHappy.txsParser (TxsAlex.txsLexer srctxt)
                                   in return $!! (parsing, "")
                                )
                                ( \e -> return ((unid, tdefs, sigs), show (e::ErrorCall))
                                )
               if  e /= ""
                 then do IFS.nack "INIT" $ [e]
                         cmdsIntpr
                 else do modify $ \env -> env { IOS.modus  = IOS.Inited
                                              , IOS.uid    = unid'
                                              , IOS.tdefs  = tdefs'
                                              , IOS.sigs   = sigs'
                                              }
                         lift $ TxsCore.txsInit tdefs' sigs'
                                                ( IFS.hmack servhs . map TxsShow.pshow )
                         IFS.pack "INIT" [ "input files parsed: " ++ fnames ]
                         cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTermit :: IOS.IOS ()
cmdTermit = do                                                     -- PRE :  modus == Inited --
     modify $ \env -> env { IOS.modus  = IOS.Idled
                          , IOS.tdefs  = TxsDefs.empty
                          , IOS.sigs   = Sigs.empty
                          }
     lift TxsCore.txsTermit
     IFS.pack "TERMIT" []
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStop :: IOS.IOS ()  -- Shut
cmdStop = do                  -- PRE :  modus == Tested, Simuled, Stepped, Learned, Manualed --
     modus <- gets IOS.modus
     case modus of
       IOS.Tested _ _   -> do modify $ \env -> env { IOS.modus = IOS.Inited }
--                            _ <- lift $ TxsCore.txsStopEW ew 
--                            IFS.pack "STOP" []
                              cmdsIntpr
       IOS.Simuled _ _  -> do modify $ \env -> env { IOS.modus = IOS.Inited }
--                            _ <- lift $ TxsCore.txsStopEW ew 
--                            IFS.pack "STOP" []
                              cmdsIntpr
       IOS.Stepped           -> do -- modify $ \env -> env { IOS.modus = IOS.Inited }
--                                    lift TxsCore.txsStopNW
--                                    IFS.pack "STOP" []
                                   cmdsIntpr
       IOS.Learned _ _  -> do modify $ \env -> env { IOS.modus = IOS.Inited }
--                            _ <- lift $ TxsCore.txsStopEW ew
--                            IFS.pack "STOP" []
                              cmdsIntpr
       IOS.Manualed _ _ -> do modify $ \env -> env { IOS.modus = IOS.Inited }
                              lift TxsManual.mnlShutManual
                              IFS.pack "STOP" []
                              cmdsIntpr
       _              -> do IFS.nack "STOP" [ "cannot stop from current mode" ]
                            cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdInfo :: IOS.IOS ()
cmdInfo = do                                                         -- PRE :  modus > Noned --
     IFS.pack "INFO" [ "TorXakis version    : " ++ VersionInfo.version
                     , "Build time          : " ++ BuildInfo.buildTime
                     ]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdParam :: String -> IOS.IOS ()
cmdParam args =                                                      -- PRE :  modus > Noned --
     case words args of
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
cmdSeed args =                                                       -- PRE :  modus > Noned --
     case words args of
       [val] -> let seed = read val                                  -- catch error read
                  in do
                   lift $ TxsCore.txsSetSeed seed
                   IFS.pack "SEED" []
                   cmdsIntpr
       _     -> do IFS.nack "SEED" [ "Incorrect seed" ]
                   cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdVar :: String -> IOS.IOS ()
cmdVar args = do                                                     -- PRE :  modus > Idled --
     env             <- get
     let uid          = IOS.uid env
         sigs         = IOS.sigs env
         vars         = IOS.locvars env
         vals         = IOS.locvals env
     if  args == ""
       then do
         IFS.pack "VAR" [ TxsShow.fshow vars ]
         cmdsIntpr
       else do
         ((uid',vars'),e) <- lift $ lift $ catch
                               ( let p = TxsHappy.vardeclsParser
                                           ( TxsAlex.Csigs sigs
                                           : TxsAlex.Cunid (_id uid + 1)
                                           : TxsAlex.txsLexer args
                                           )
                                  in return $!! (p,"")
                               )
                               ( \e -> return ((uid,[]),show (e::ErrorCall)))
         if  e /= ""
           then do
             modify $ \env' -> env' { IOS.uid = uid' }
             IFS.nack "VAR" [ e ]
             cmdsIntpr
           else
             if  let newnames = map VarId.name vars'
                  in null ( newnames `List.intersect` map VarId.name vars ) &&
                     null ( newnames `List.intersect` map VarId.name (Map.keys vals))
               then do
                 modify $ \env' -> env' { IOS.locvars = vars ++ vars'
                                        , IOS.uid  = uid'
                                        }
                 IFS.pack "VAR" [ TxsShow.fshow vars' ]
                 cmdsIntpr
               else do
                 modify $ \env' -> env' { IOS.uid = uid' }
                 IFS.nack "VAR" [ "double variable names: " ++ TxsShow.fshow vars' ]
                 cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdVal :: String -> IOS.IOS ()
cmdVal args = do                                                     -- PRE :  modus > Idled --
     env             <- get
     let uid          = IOS.uid env
         sigs         = IOS.sigs env
         vars         = IOS.locvars env
         vals         = IOS.locvals env
     if  args == ""
       then do
         IFS.pack "VAL" [ TxsShow.fshow vals ]
         cmdsIntpr
       else do
         ((uid',venv'),e) <- lift $ lift $ catch
                               ( let p = TxsHappy.valdefsParser
                                           ( TxsAlex.Csigs sigs
                                           : TxsAlex.Cvarenv []
                                           : TxsAlex.Cunid (_id uid + 1)
                                           : TxsAlex.txsLexer args
                                           )
                                  in return $!! (p,"")
                               )
                               ( \e -> return ((uid,Map.empty),show (e::ErrorCall)))
         if  e /= ""
           then do
             modify $ \env' -> env' { IOS.uid = uid' }
             IFS.nack "VAL" [ e ]
             cmdsIntpr
           else
             if let newnames = map VarId.name (Map.keys venv')
                 in null (newnames `List.intersect` map VarId.name vars) &&
                    null (newnames `List.intersect` map VarId.name (Map.keys vals))
               then do
                 modify $ \env' -> env' { IOS.locvals = vals `Map.union` venv'
                                        , IOS.uid     = uid'
                                        }
                 IFS.pack "VAL" [ TxsShow.fshow venv' ]
                 cmdsIntpr
               else do
                 modify $ \env' -> env' { IOS.uid = uid' }
                 IFS.nack "VAR" [ "double value names: " ++ TxsShow.fshow venv' ]
                 cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdEval :: String -> IOS.IOS ()
cmdEval args = do                                                    -- PRE :  modus > Idled --
     env             <- get
     let uid          = IOS.uid env
         tdefs        = IOS.tdefs env
         sigs         = IOS.sigs env
         vals         = IOS.locvals env
         vars         = IOS.locvars env
     ((uid',vexp'),e) <- lift $ lift $ catch
                           ( let p = TxsHappy.vexprParser
                                        ( TxsAlex.Csigs    sigs
                                        : TxsAlex.Cvarenv (Map.keys vals ++ vars)
                                        : TxsAlex.Cunid   (_id uid + 1)
                                        : TxsAlex.txsLexer args
                                        )
                              in return $!! (p,"")
                           )
                           ( \e -> return ((uid,ValExpr.cstrError "cmdEval parse failed"), show (e::ErrorCall)))
     if  e /= ""
       then do modify $ \env' -> env' { IOS.uid = uid' }
               IFS.nack "EVAL" [ e ]
               cmdsIntpr
       else do modify $ \env' -> env' { IOS.uid = uid' }
               walue <- lift $ TxsCore.txsEval (ValExpr.subst vals (TxsDefs.funcDefs tdefs) vexp')
               IFS.pack "EVAL" [ TxsShow.fshow walue ]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdSolve :: String -> IOS.IOS ()
cmdSolve args = do                                                   -- PRE :  modus > Idled --
     env             <- get
     let uid          = IOS.uid env
         tdefs        = IOS.tdefs env
         sigs         = IOS.sigs env
         vars         = IOS.locvars env
         vals         = IOS.locvals env
     ((uid',vexp'),e) <- lift $ lift $ catch
                           ( let p = TxsHappy.vexprParser
                                       ( TxsAlex.Csigs sigs
                                       : TxsAlex.Cvarenv (Map.keys vals ++ vars)
                                       : TxsAlex.Cunid (_id uid + 1)
                                       : TxsAlex.txsLexer args
                                       )
                              in return $!! (p,"")
                           )
                           ( \e -> return ((uid,ValExpr.cstrError "cmdSolve parse failed."),
                                           show (e::ErrorCall)))
     let solver = case words args of
                    ("SOL":args') -> Just TxsCore.txsSolve
                    ("UNI":args') -> Just TxsCore.txsUniSolve
                    ("RAN":args') -> Just TxsCore.txsRanSolve
                    _             -> Nothing
     if  solver == Nothing
       then do modify $ \env' -> env' { IOS.uid = uid' }
               IFS.nack "SOLVE" ["unknown kind of solver"]
               cmdsIntpr
       else if  e /= ""
              then do modify $ \env' -> env' { IOS.uid = uid' }
                      IFS.nack "SOLVE" [ e ]
                      cmdsIntpr
              else do modify $ \env' -> env' { IOS.uid = uid' }
                      sols  <- lift $ solver (ValExpr.subst vals (TxsDefs.funcDefs tdefs) vexp')
                      IFS.pack "SOLVE" [ show sols ]
                      cmdsIntpr

-}

-- ----------------------------------------------------------------------------------------- --

{-

cmdTester :: String -> IOS.IOS ()
cmdTester args = do                                                -- PRE :  modus == Inited --
     envs <- get 
     let  tdefs     = IOS.tdefs envs
          deltaTime = case Map.lookup "param_Sut_deltaTime" (IOS.params envs) of
                        Nothing      -> 2000                -- default 2 sec
                        Just (val,_) -> read val
          ioTime    = case Map.lookup "param_Sut_ioTime" (IOS.params envs) of
                        Nothing      -> 10                  -- default 10 msec
                        Just (val,_) -> read val
     case words args of
       [m,c] -> do
            let mdefs = [ mdef
                        | (TxsDefs.ModelId nm _, mdef) <- Map.toList (TxsDefs.modelDefs tdefs)
                        , T.unpack  nm == m
                        ]
                cdefs = [ cdef
                        | (TxsDefs.CnectId nm _, cdef) <- Map.toList (TxsDefs.cnectDefs tdefs)
                        , T.unpack nm == c
                        ]
            case (mdefs,cdefs) of
              ([modeldef],[cnectdef])
                         | isConsistentTester modeldef Nothing Nothing cnectdef
                -> do ew  <- lift $ set-initSockWorld cnectdef deltaTime ioTime
                      ew' <- lift $ TxsCore.txsSetTest ew modeldef Nothing Nothing
                      modify $ \env -> env { IOS.modus = IOS.Tested ew' }
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
                -> do ew  <- lift $ initSockWorld cnectdef deltaTime ioTime
                      ew' <- lift $ TxsCore.txsSetTest ew modeldef (Just mapperdef) Nothing
                      modify $ \env -> env { IOS.modus  = IOS.Tested ew' }
                      IFS.pack "TESTER" []
                      cmdsIntpr
              ([modeldef],[],[purpdef],[cnectdef])
                         | isConsistentTester modeldef Nothing (Just purpdef) cnectdef
                -> do ew  <- lift $ initSockWorld cnectdef deltaTime ioTime
                      ew' <- lift $ TxsCore.txsSetTest ew modeldef Nothing (Just purpdef)
                      modify $ \env -> env { IOS.modus  = IOS.Tested ew' }
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
                -> do ew  <- lift $ initSockWorld cnectdef deltaTime ioTime
                      ew' <- lift $ TxsCore.txsSetTest ew modeldef (Just mapperdef)
                                                                   (Just purpdef)
                      modify $ \env -> env { IOS.modus  = IOS.Tested ew' }
                      IFS.pack "TESTER" [ ]
                      cmdsIntpr
              _ -> do IFS.nack "TESTER" [ "Wrong or inconsistent parameters" ]
                      cmdsIntpr
       _ -> do
            IFS.nack "TESTER" [ "Wrong number of parameters" ]
            cmdsIntpr


isConsistentTester :: TxsDefs.ModelDef
                   -> Maybe TxsDefs.MapperDef
                   -> Maybe TxsDefs.PurpDef
                   -> TxsDefs.CnectDef
                   -> Bool

isConsistentTester (TxsDefs.ModelDef minsyncs moutsyncs _ _)
                   Nothing
                   _
                   (TxsDefs.CnectDef _ _ conndefs)
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
                   (TxsDefs.CnectDef _ _ conndefs)
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
cmdSimulator args = do                                             -- PRE :  modus == Inited --
     envs <- get
     let  tdefs     = IOS.tdefs envs
          deltaTime = case Map.lookup "param_Sim_deltaTime" (IOS.params envs) of
                        Nothing      -> 2000                -- default 2 sec
                        Just (val,_) -> read val
          ioTime    = case Map.lookup "param_Sut_ioTime" (IOS.params envs) of
                        Nothing      -> 10                  -- default 10 msec
                        Just (val,_) -> read val
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
                -> do ew  <- lift $ initSockWorld cnectdef deltaTime ioTime
                      ew' <- lift $ TxsCore.txsSetSim ew modeldef Nothing
                      modify $ \env -> env { IOS.modus = IOS.Simuled ew' }
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
                -> do ew  <- lift $ initSockWorld cnectdef deltaTime ioTime
                      ew' <- lift $ TxsCore.txsSetSim ew modeldef (Just mapperdef)
                      modify $ \env -> env { IOS.modus = IOS.Simuled ew' }
                      IFS.pack "SIMULATOR" []
                      cmdsIntpr
              _ -> do IFS.nack "SIMULATOR" [ "Wrong or inconsistent parameters" ]
                      cmdsIntpr
       _ -> do
            IFS.nack "SIMULATOR" [ "Wrong number of parameters" ]
            cmdsIntpr


isConsistentSimulator :: TxsDefs.ModelDef
                      -> Maybe TxsDefs.MapperDef
                      -> TxsDefs.CnectDef
                      -> Bool

isConsistentSimulator (TxsDefs.ModelDef minsyncs moutsyncs _ _)
                      Nothing
                      (TxsDefs.CnectDef _ _ conndefs)
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
                      (TxsDefs.CnectDef _ _ conndefs)
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
cmdStepper args = do                                               -- PRE :  modus == Inited --
     envs <- get
     let  tdefs = IOS.tdefs envs
     case words args of
       [m] -> do
            let mdefs = [ mdef
                        | (TxsDefs.ModelId nm _, mdef) <- Map.toList (TxsDefs.modelDefs tdefs)
                        , T.unpack nm == m
                        ]
            case mdefs of
              [modeldef]
                -> do lift $ TxsCore.txsSetStep modeldef
                      modify $ \env -> env { IOS.modus = IOS.Stepped }
                      IFS.pack "STEPPER" []
                      cmdsIntpr
              _ -> do IFS.nack "STEPPER" [ "Wrong or inconsistent parameters" ]
                      cmdsIntpr
       _ -> do
            IFS.nack "STEPPER" [ "Wrong number of parameters" ]
            cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdLearner :: String -> IOS.IOS ()
cmdLearner args = do                                               -- PRE :  modus == Inited --
     IFS.nack "LEARNER" ["learner not implemented yet"]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdManual :: String -> IOS.IOS ()
cmdManual args = do                                                -- PRE :  modus == Inited --
     envs <- get
     [(_,connDelayVal)]  <- IOS.getParams ["param_EW_connDelay"]
     [(_,deltaTimeVal)]  <- IOS.getParams ["param_Sut_deltaTime"]
     [(_,chReadTimeVal)] <- IOS.getParams ["param_Sut_ioTime"]
     let connDelay  = read connDelayVal
         deltaTime  = read deltaTimeVal
         chReadTime = read chReadTimeVal
         cnectDefs  = Map.toList $ TxsDefs.cnectDefs (IOS.tdefs envs)
         cdefs      = [ cdef | (TxsDefs.CnectId nm _, cdef) <- cnectDefs, T.unpack nm == args ]
     case cdefs of
       [ cdef@(TxsDefs.CnectSockExplW _ _ connDefs) ]
         -> do let ew = SockExplW.setSockExplW cdef connDelay deltaTime chReadTime
               lift $ TxsManual.mnlSetManual ew
               let chanToWs  = [ chan | TxsDefs.ConnDtoW  chan _ _ _ _ <- connDefs ]
                   chanFroWs = [ chan | TxsDefs.ConnDfroW chan _ _ _ _ <- connDefs ]
               modify $ \env -> env { IOS.modus = IOS.Manualed chanToWs chanFroWs }
               IFS.pack "MANUAL" []
               cmdsIntpr
       [ cdef@(TxsDefs.CnectSockImplW _ connDefs) ]
         -> do let ew = SockImplW.setSockImplW cdef connDelay deltaTime chReadTime
               lift $ TxsManual.mnlSetManual ew
               let chanToWs  = [ chan | TxsDefs.ConnDtoW  chan _ _ _ _ <- connDefs ]
                   chanFroWs = [ chan | TxsDefs.ConnDfroW chan _ _ _ _ <- connDefs ]
               modify $ \env -> env { IOS.modus = IOS.Manualed chanToWs chanFroWs }
               IFS.pack "MANUAL" []
               cmdsIntpr
       _ -> do IFS.nack "MANUAL" [ "no (unique) cnectdef" ]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --


cmdTest :: String -> IOS.IOS ()
cmdTest args                                                       -- PRE :  modus == Tested --
  =  case words args of
       []                                                              -- observe one output --
         -> do verdict <-lift TxsCore.txsTestOut
               IFS.pack "TEST" [TxsShow.fshow verdict]
               cmdsIntpr
       [d] | all Char.isDigit d                                     -- d::int i/o test steps --
         -> do verdict <- lift $ TxsCore.txsTestN (read d)
               IFS.pack "TEST" [TxsShow.fshow verdict]
               cmdsIntpr
       _                                                         -- do given action as input --
         -> do modus <- gets IOS.modus                
               conndefs <- case modus of              
                             IOS.Tested (CtRunSW (TxsDefs.CnectDef _ _ conndefs) _ _ _ _ _)
                               -> return conndefs
                             IOS.Tested (UCtRunSW (TxsDefs.CnectDef _ _ conndefs) _ _ _ _)
                               -> return conndefs 
                             _ -> return []
               let ctows = [ chan | TxsDefs.ConnDtoW  chan _ _ _ _ <- conndefs ]
               act <- readAction ctows args
               if  act == TxsDDefs.ActQui
                 then cmdsIntpr
                 else do verdict <- lift $ TxsCore.txsTestIn act
                         IFS.pack "TEST" [TxsShow.fshow verdict]
                         cmdsIntpr

  9          -> case words args of
 10               ["start"] 
 11                 -> do lift TxsCore.txsTestStart
 12                       IFS.pack "TEST" [ "External World started"]
 13                       cmdsIntpr
 14               ["stop"] 
 15                 -> do lift TxsCore.txsTestStop
 16                       IFS.pack "TEST" [ "External World stopped"]
 17                       cmdsIntpr
 18               ("step":args')
 19                 -> do verdict <- cmdTestStep args'
 20                       IFS.pack "TEST" [TxsShow.fshow verdict]
 21                       cmdsIntpr
 22               ("goal":args')
 23                 -> do verdict <- cmdTestGoal args'
 24                       IFS.pack "TEST" [TxsShow.fshow verdict]
 25                       cmdsIntpr
 26               ("purp":args')
 27                 -> do verdict <- cmdTestPurp args'
 28                       cmdsIntpr
 29               ("purps":args')
 30                 -> do verdict <- cmdTestPurps args'
 31                       IFS.pack "TEST" [TxsShow.fshow verdict]
 32                       cmdsIntpr
 33               _ -> do IFS.nack "TEST" [ "Not a TEST action: ", unwords args ]
 34                       cmdsIntpr

 5                
  6 cmdTestStep :: String -> IOS.IOS TxsDDefs.Verdict
  7 cmdTest args = do
  8      case words args of
  9        [] -> do                                                        -- observe one output --
 10                 verdict <-lift TxsCore.txsTestOut
 11                 return verdict 
 12        [d] | all Char.isDigit d                                     -- d::int i/o test steps --
 13           -> do verdict <- lift $ TxsCore.txsTestN (read d)
 14                 IFS.pack "TEST" [TxsShow.fshow verdict]
 15                 return verdict
 16        _  -> do                                                  -- do given action as input --
 17                 modus <- gets IOS.modus
 18                 conndefs <- case modus of
 19                               IOS.Tested (CtRunSW (TxsDefs.CnectDef _ _ conndefs) _ _ _ _ _)
 20                                 -> return conndefs
 21                               IOS.Tested (UCtRunSW (TxsDefs.CnectDef _ _ conndefs) _ _ _ _)
 22                                 -> return conndefs 
 23                               _ -> return []
 24                 let ctows = [ chan | TxsDefs.ConnDtoW chan _ _ _ _ <- conndefs ]
 25                 act <- readAction ctows args
 26                 if  act == TxsDDefs.ActQui
 27                   then cmdsIntpr
 28                   else do verdict <- lift $ TxsCore.txsTestIn act
 29                           IFS.pack "TEST" [TxsShow.fshow verdict]
 30                           cmdsIntpr
 31                           


-- ----------------------------------------------------------------------------------------- --

cmdSim :: String -> IOS.IOS ()
cmdSim args =                                                     -- PRE :  modus == Simuled --
     case words args of
       []                                                            -- no arg: infinite sim --
         -> do verdict <- lift $ TxsCore.txsSimN (-1)
               IFS.pack "SIM" [TxsShow.fshow verdict]
               cmdsIntpr
       [d] | all Char.isDigit d                                          -- d::int sim steps --
         -> do verdict <- lift $ TxsCore.txsSimN (read d)
               IFS.pack "SIM" [TxsShow.fshow verdict]
               cmdsIntpr
       _                                                                 -- not a valid call --
         -> do IFS.nack "SIM" ["wrong parameter"]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdStep :: String -> IOS.IOS ()
cmdStep args =                                                    -- PRE :  modus == Stepped --
     case words args of
       []                                                                -- no arg: one step --
         -> do verdict <- lift $ TxsCore.txsStepN 1
               IFS.pack "STEP" [TxsShow.fshow verdict]
               cmdsIntpr
       [d] | all Char.isDigit d                                              -- d::int steps --
         -> do verdict <- lift $ TxsCore.txsStepN (read d)
               IFS.pack "STEP" [TxsShow.fshow verdict]
               cmdsIntpr
       _                                                          -- action arg: step action --
         -> do tdefs <- gets IOS.tdefs
               let mdefs = TxsDefs.modelDefs tdefs
                   chids = Set.toList $ Set.unions [ Set.unions (chins ++ chouts ++ spls)
                                                   | (_, TxsDefs.ModelDef chins chouts spls _)
                                                     <- Map.toList mdefs
                                                   ]
               act <- readAction chids args
               if  act == TxsDDefs.ActQui
                 then cmdsIntpr
                 else do verdict <- lift $ TxsCore.txsStepA act
                         IFS.pack "STEP" [TxsShow.fshow verdict]
                         cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdLearn :: String -> IOS.IOS ()
cmdLearn args =                                                   -- PRE :  modus == Learned --
     IFS.nack "LEARNER" ["learn not implemented yet"]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdMan :: String -> IOS.IOS ()
cmdMan args = do                                                 -- PRE :  modus == Manualed --
     IOS.Manualed chansToW _chansFroW <- gets IOS.modus
     case words args of
       ["start"]
         -> do lift $ TxsManual.mnlStartW
               IFS.pack "MAN" []
               cmdsIntpr
       ["stop"] 
         -> do lift $ TxsManual.mnlStopW
               IFS.pack "MAN" []
               cmdsIntpr
       ("act":args')
         -> do act   <- readAction chansToW (unwords args')
               _act' <- lift $ TxsManual.mnlActToW act
               IFS.pack "MAN" []
               cmdsIntpr
       ["obs"]
         -> do _act' <- lift $ TxsManual.mnlObsFroW
               IFS.pack "MAN" []
               cmdsIntpr
       ("offer":args')
         -> do offers  <- readOffers chansToW (unwords args')
               case Set.toList offers of
                 [offer]
                   -> do _act' <- lift $ TxsManual.mnlOfferToW offer
                         IFS.pack "MAN" []
                         cmdsIntpr
                 _ -> do IFS.nack "MAN" [ "To External Word only one offer allowed" ]
                         cmdsIntpr
       ["run"]
         -> do verdict <- lift $ TxsManual.mnlRunW (-1)
               IFS.pack "MAN" [ TxsShow.pshow verdict ]
               cmdsIntpr
       ["run",nrsteps] | all Char.isDigit nrsteps
         -> do verdict <- lift $ TxsManual.mnlRunW (read nrsteps)
               IFS.pack "MAN" [ TxsShow.pshow verdict ]
               cmdsIntpr
       ["stnr"]
         -> do stNr <- lift $ TxsManual.mnlGetStNr
               IFS.pack "MAN" [ "State Nr: "++ TxsShow.pshow stNr ]
               cmdsIntpr
       ["path"]
         -> do path <- lift $ TxsManual.mnlGetPath
               IFS.pack "MAN" [ "Path:", TxsShow.pshow path ]
               cmdsIntpr
       _ -> do IFS.nack "MAN" [ "Unknown manual operation: " ++ args ]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdShow :: String -> IOS.IOS ()
cmdShow args = do                                                    -- PRE :  modus > Idled --
     envs  <- get
     txt   <- case words args of
                ["tdefs"             ] -> lift $ TxsCore.txsShow "tdefs"     ""
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
                                                    in TxsShow.fshow (towhdls ++ frowhdls)
                ["var"]     -> return $ TxsShow.fshow (IOS.locvars envs)
                ["val"]     -> return $ TxsShow.fshow (IOS.locvals envs)
                _           -> return ""
     case txt of
       "" -> do IFS.nack "SHOW" ["nothing to be shown"]
                cmdsIntpr
       s  -> do IFS.mack [s]
                IFS.pack "SHOW" ["\n"]
                cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdGoTo :: String -> IOS.IOS ()
cmdGoTo args =                                                    -- PRE :  modus == Stepped --
     case words args of
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

-- ----------------------------------------------------------------------------------------- --

cmdPath :: String -> IOS.IOS ()
cmdPath _ = do                                                    -- PRE :  modus == Stepped --
     path <- lift TxsCore.txsPath
     IFS.mack [ TxsShow.showN n 6 ++ ": " ++ TxsShow.fshow s1 ++ " -> " ++
                unwords (lines $ TxsShow.fshow a) ++ " -> " ++ TxsShow.fshow s2
              | (n,(s1,a,s2)) <- zip [1 ..] path
              ]
     IFS.pack "PATH" ["\n"]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTrace :: String -> IOS.IOS ()
cmdTrace args = do                                                  -- PRE :  modus > Inited --
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
cmdMenu args =                                                      -- PRE :  modus > Inited --
     let (kind,what) =
          case words args of
              ["in"]       -> ( "mod", "in" )
              ["out"]      -> ( "mod", "out" )
              ["map"]      -> ( "map", "" )
              ["purp",gnm] -> ( "purp", gnm )
              _            -> ( "mod", "all" )
     in do
       menu <- lift $ TxsCore.txsMenu kind what
       IFS.mack [ TxsShow.fshow menu ]
       IFS.pack "MENU" [ "\n" ]
       cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdMap :: String -> IOS.IOS ()
cmdMap args = do                                        -- PRE :  modus == tested \/ simuled --
     tdefs   <- gets IOS.tdefs
     let mdefs   = TxsDefs.mapperDefs tdefs
         inchids = concat [ chins
                          | ( _ , TxsDefs.MapperDef chins _ _ _ ) <- Map.toList mdefs
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
cmdNComp args = do                                                 -- PRE :  modus == Inited --
     tdefs <- gets IOS.tdefs
     case words args of
       [mname] -> case [ mdef
                       | (TxsDefs.ModelId nm _, mdef) <- Map.toList (TxsDefs.modelDefs tdefs)
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
                                -> do IFS.nack "NCOMP" [ "Could not generate test purpose" ]
                                      cmdsIntpr
                    _ -> do IFS.nack "NCOMP" [ "No such MODELDEF" ]
                            cmdsIntpr
       _       -> do IFS.nack "NCOMP" [ "Argument must be one MODELDEF name" ]
                     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdLPE :: String -> IOS.IOS ()
cmdLPE args = do                                                   -- PRE :  modus == Inited --
     tdefs <- gets IOS.tdefs
     let mdefs = TxsDefs.modelDefs tdefs
         mids  = [ modelid | (modelid@(TxsDefs.ModelId nm _uid), _) <- Map.toList mdefs
                           , T.unpack nm == args
                 ]
         chids = Set.toList $ Set.unions [ Set.unions (chins ++ chouts ++ spls)
                                         | (_, TxsDefs.ModelDef chins chouts spls _)
                                           <- Map.toList mdefs
                                         ]
     case mids of
       [ modelId ]
         -> do mayModelId' <- lift $ TxsCore.txsLPE (Right modelId)
               case mayModelId' of
                 Just (Right modelId') -> do IFS.pack "LPE" [ "LPE modeldef generated: "
                                                            , TxsShow.fshow modelId'
                                                            ]
                                             cmdsIntpr
                 _                     -> do IFS.nack "LPE" [ "Could not generate LPE" ]
                                             cmdsIntpr
       _ -> do bexpr       <- readBExpr chids args
               mayBexpr'   <- lift $ TxsCore.txsLPE (Left bexpr)
               case mayBexpr' of
                 Just (Left bexpr')    -> do IFS.pack "LPE" [ "LPE behaviour generated: "
                                                            , TxsShow.fshow bexpr'
                                                            ]
                                             cmdsIntpr
                 _                     -> do IFS.nack "LPE" [ "Could not generate LPE" ]
                                             cmdsIntpr

-}

-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

