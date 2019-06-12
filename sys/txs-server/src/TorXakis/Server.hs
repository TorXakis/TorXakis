{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}

module TorXakis.Server

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

--import           Control.Concurrent
--import           Control.DeepSeq
--import           Control.Exception
--import           Control.Monad.State
--import qualified Data.Char           as Char
--import qualified Data.Either         as Either
--import qualified Data.List           as List
--import qualified Data.Map            as Map
--import qualified Data.Set            as Set
--import qualified Data.String.Utils   as Utils
--import qualified Data.Text           as T
--import           Network             hiding (socketPort)
--import           Network.Socket      hiding (accept, sClose)
--import           System.IO
--
---- import from local
--import           CmdLineParser
--import           ToProcdef
import qualified TorXakis.ServerConfig     as SC

---- import from serverenv
--import qualified EnvServer           as IOS
--import qualified IfServer            as IFS
--
---- import from core
--import qualified BuildInfo
--import qualified TxsCore
--import qualified VersionInfo
--import qualified EnvCore as IOC
--
---- import from defs
--import qualified TxsDDefs
--import qualified TxsDefs
--import qualified TxsShow
--import qualified Utils
--import qualified VarId
--import qualified ModelId
--
---- import from bexpr
--import qualified ProcId
--import qualified ChanId
--
---- import from valexpr
--import qualified Constant
--import           Id
--import qualified ValExpr
--
---- import from cnect
--import qualified SocketWorld         as World
--
---- import from txs-compiler
--import           TorXakis.Compiler


main :: IO ()
main = withSocketsDo $ do
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
     modus      <- gets IOS.modus
     (cmd, args) <- IFS.getCmd
     case cmd of
-- ----------------------------------------------------------------------------------- modus --
       "START"     |       IOS.isNoned    modus ->  cmdStart     args
       "START"     | not $ IOS.isNoned    modus ->  cmdNoop      cmd
       "QUIT"                                   ->  cmdQuit      args
       "INIT"      |       IOS.isIdled    modus ->  cmdInit      args
       "INIT"      | not $ IOS.isIdled    modus ->  cmdNoop      cmd
       "TERMIT"    |       IOS.isGtIdled  modus ->  cmdTermit    args
       "TERMIT"    | not $ IOS.isGtIdled  modus ->  cmdNoop      cmd
-- -------------------------------------------------------------------------------- settings --
       "INFO"      |       IOS.isGtNoned  modus ->  cmdInfo      args
       "INFO"      | not $ IOS.isGtNoned  modus ->  cmdNoop      cmd
-- ------------------------------------------------------------------------------------ data --
       "VAR"       |       IOS.isGtIdled  modus ->  cmdVar       args
       "VAR"       | not $ IOS.isGtIdled  modus ->  cmdNoop      cmd
       "VAL"       |       IOS.isGtIdled  modus ->  cmdVal       args
       "VAL"       | not $ IOS.isGtIdled  modus ->  cmdNoop      cmd
       "EVAL"      |       IOS.isGtIdled  modus ->  cmdEval      args
       "EVAL"      | not $ IOS.isGtIdled  modus ->  cmdNoop      cmd
       _                                        ->  cmdUnknown   cmd


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
     modify $ \env -> env { IOS.modus = IOS.Idled }
     host <- gets IOS.host
     port <- gets IOS.portNr
     IFS.pack "START" ["txsserver starting:  " ++ show host ++ " : " ++ show port]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdQuit :: String -> IOS.IOS ()
cmdQuit _ = do
     modify $ \env -> env { IOS.modus = IOS.Noned }
     host <- gets IOS.host
     port <- gets IOS.portNr
     IFS.pack "QUIT" ["txsserver closing  " ++ show host ++ " : " ++ show port]
     return ()

-- ----------------------------------------------------------------------------------------- --

cmdInit :: String -> IOS.IOS ()
cmdInit args = do
     servhs             <- gets IOS.servhs
     unid               <- gets IOS.uid
     tdefs              <- lift TxsCore.txsGetTDefs
     sigs               <- gets IOS.sigs
     srctxts            <- lift $ lift $ mapM readFile (read args :: [String])
     let srctxt          = List.intercalate "\n\n" srctxts
     ((unid',tdefs', sigs'),e) <- lift $ lift $ catch
                             ( let parsing = compileLegacy srctxt
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
               IFS.pack "INIT" ["input files parsed:", args]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTermit :: String -> IOS.IOS ()
cmdTermit _ = do
     modify $ \env -> env { IOS.modus  = IOS.Idled
                          , IOS.tow    = ( Nothing, Nothing, [] )
                          , IOS.frow   = ( Nothing, [],      [] )
                          }
     lift TxsCore.txsTermit
     IFS.pack "TERMIT" []
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdInfo :: String -> IOS.IOS ()
cmdInfo _ = do
     IFS.pack "INFO" [ "TorXakis version    : " ++ VersionInfo.version
                     , "Build time          : " ++ BuildInfo.buildTime
                     ]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdVar :: String -> IOS.IOS ()
cmdVar args = do
     env              <- get
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
                               ( let p = compileUnsafe $
                                         compileVarDecls sigs (_id uid + 1) args
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
cmdVal args = do
     env              <- get
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
                               ( let p = compileUnsafe $
                                         compileValDefs sigs [] (_id uid + 1) args
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
cmdEval args = do
     env              <- get
     let uid           = IOS.uid env
         sigs          = IOS.sigs env
         vals          = IOS.locvals env
         vars          = IOS.locvars env
     tdefs            <- lift TxsCore.txsGetTDefs

     ((uid',vexp'),e) <- lift $ lift $ catch
                           ( let (i,p) = compileUnsafe $
                                         compileValExpr sigs (Map.keys vals ++ vars) (_id uid + 1) args
                              in return $!! ((i, Just p),"")
                           )
                           ( \e -> return ((uid, Nothing),show (e::ErrorCall)))

     case vexp' of
       Just vexp'' -> do
                        modify $ \env' -> env' { IOS.uid = uid' }
                        mwalue <- lift $ TxsCore.txsEval (ValExpr.subst vals (TxsDefs.funcDefs tdefs) vexp'')
                        case mwalue of
                            Right walue -> do
                                            IFS.pack "EVAL" [ TxsShow.fshow walue ]
                                            cmdsIntpr
                            Left t      -> do
                                            IFS.nack "EVAL" [ "eval 2 - " ++ t ]
                                            cmdsIntpr

       Nothing -> do
                    modify $ \env' -> env' { IOS.uid = uid' }
                    IFS.nack "EVAL" [ "eval 1 - " ++ e ]
                    cmdsIntpr

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

