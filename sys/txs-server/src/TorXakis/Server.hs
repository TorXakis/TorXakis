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

import           Control.Concurrent
--import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.State
--import qualified Data.Char           as Char
--import qualified Data.Either         as Either
import qualified Data.List           as List
import qualified Data.Map            as Map
--import qualified Data.Set            as Set
--import qualified Data.String.Utils   as Utils
--import qualified Data.Text           as T
import           Network             hiding (socketPort)
import           Network.Socket      hiding (accept, sClose)
import           System.IO

import           TorXakis.Language

---- import from local
import           TorXakis.CmdLineParser
--import           ToProcdef
--import qualified TorXakis.ServerConfig     as SC

---- import from serverenv
import qualified TorXakis.ServerState         as IOS
import qualified TorXakis.IfServer            as IFS

---- import from core
import           TorXakis.BuildInfo
import           TorXakis.TxsCore
import           TorXakis.VersionInfo
import qualified TorXakis.CoreState as IOC

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
import           TorXakis.ContextValExpr
import           TorXakis.ValExpr
import           TorXakis.Var

---- import from cnect
--import qualified SocketWorld         as World
--
---- import from txs-compiler
--import           TorXakis.Compiler


main :: IO ()
main = withSocketsDo $ do
      hSetBuffering stderr NoBuffering     -- alt: LineBuffering
      hSetBuffering stdout LineBuffering
  --uConfig <- SC.loadConfig

--  case SC.interpretConfig uConfig of
--    Left xs -> do
--      hPutStrLn stderr "Errors found while loading the configuration"
--      hPrint stderr xs
--    Right config -> do
      (portNr, sock) <- txsListenOn Nothing --  (clPortNumber . SC.cmdLineCfg) uConfig
      (hs, host, _) <- accept sock
      hSetBuffering hs LineBuffering
      hSetEncoding hs latin1
      hPutStrLn stderr "\nTXSSERVER >>  Starting  ..... \n"
      let initS = IOS.envsNone
              { IOS.host   = host
              , IOS.portNr = portNr
              , IOS.servhs = hs
--            , IOS.params = SC.updateParamVals -- updating parameters...
--                            (IOS.params IOS.envsNone) -- ...defined in ServerEnv
--                            $ SC.configuredParameters config
              }
--          coreConfig = config
      runTxsCore cmdsIntpr initS
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
     srctxts            <- lift $ lift $ mapM readFile (read args :: [String])
     let srctxt          = List.intercalate "\n\n" srctxts
     r                  <- lift $ lift $ compileString srctxt
     let (txsctx,e) = case r of
                        Left  e'            -> ( TorXakis.ContextValExpr.empty, e' )
                        Right (_,txsctx',_) -> ( txsctx'                      , "" )
     if e /= ""
       then do IFS.nack "INIT" [e]
               cmdsIntpr
       else do modify $ \env -> env { IOS.modus      = IOS.Inited
                                    , IOS.locVexpCtx = fromFuncContext txsctx
                                    }
               lift $ txsInit txsctx
               IFS.pack "INIT" ["input files parsed:", args]
               cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdTermit :: String -> IOS.IOS ()
cmdTermit _ = do
     modify $ \env -> env { IOS.modus  = IOS.Idled
                          -- , IOS.tow    = ( Nothing, Nothing, [] )
                          -- , IOS.frow   = ( Nothing, [],      [] )
                          }
     lift txsTermit
     IFS.pack "TERMIT" []
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdInfo :: String -> IOS.IOS ()
cmdInfo _ = do
     IFS.pack "INFO" [ "TorXakis version    : " ++ version
                     , "Build time          : " ++ buildTime
                     ]
     cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdVar :: String -> IOS.IOS ()
cmdVar args = do
     env            <- get
     let locVexpCtx  = IOS.locVexpCtx env
         locVarVals  = IOS.locVarVals env
     if  args == ""
       then do
         IFS.pack "VAR" [ toString $ map (prettyPrint Options(False,True) locVexpCtx) (elemsVar locVexpCtx) ]
         cmdsIntpr
       else do
         (vars',e) <- lift $ lift $ catch
                               ( let p = compileUnsafe $ compileVarDecls locVexpCtx args
                                  in return $!! (p,"")
                               )
                               ( \e -> return ([],show (e::ErrorCall)))
         if  e /= ""
           then do
             IFS.nack "VAR" [ e ]
             cmdsIntpr
           else
             let newnames = map TorXakis.Var.name vars'
                 doublenames = newnames `List.intersect` map TorXakis.Var.name (Map.keys vars)
              in if null doublenames
                   then do
                          modify $ \env' -> env' { IOS.locVexpCtx = addVars locVexpCtx vars' } 
                          IFS.pack "VAR" [ toString $ map (prettyPrint Options(False,True) locVexpCtx) vars' ]
                          cmdsIntpr
                   else do
                          IFS.nack "VAR" [ "double variable names: " ++ toString $ map (prettyPrint Options(False,True) locVexpCtx) doublenames ]
                          cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdVal :: String -> IOS.IOS ()
cmdVal args = do
     env            <- get
     let locVexpCtx  = IOS.locVexpCtx env
         locVarVals  = IOS.locVarVals env
     if  args == ""
       then do
         IFS.pack "VAL" [ toString $ prettyPrint Options(False,True) locVexpCtx locVarVals ]
         cmdsIntpr
       else do
         (venv',e) <- lift $ lift $ catch
                               ( let p = compileUnsafe $ compileValDefs locVexpCtx args
                                 in return $!! (p,"")
                               )
                               ( \e -> return (Map.empty,show (e::ErrorCall)))
         if  e /= ""
           then do
             IFS.nack "VAL" [ e ]
             cmdsIntpr
           else
             let newvars     = Map.keys venv'
                 newnames    = map TorXakis.Var.name newvars
                 oldvars     = elemsVar locVexpCtx
                 oldnames    = map TorXakis.Var.name oldvars
                 doublenames = newnames `List.intersect` oldnames
                 
              in if null doublenames
                    then do
                            let locVexpCtx' = addVars locVexpCtx newvars
                                substMap    = Map.fromList [ (toRef(TorXakis.Var.name vardef), vexp)
                                                           | (vardef,vexp) <- toList venv'
                                                           ]
                                locVarVals' = Map.union locVarVals
                                                Map.fromList [ ( varref, case subst locVexpCtx' substMap vexp of
                                                                                Left e  -> error e
                                                                                Right x -> eval x 
                                                               )
                                                             | (varref,vexp) <- toList substMap
                                                             ]
                            modify $ \env' -> env' { IOS.locVexpCtx = locVexpCtx'
                                                   , IOS.locVarVals = locVarVals'
                                                   }
                            IFS.pack "VAL" [ toString $ map (prettyPrint Options(False,True) locVexpCtx')  (VEnv venv') ]
                            cmdsIntpr
                    else do
                        IFS.nack "VAL" [ "double variable names: " ++ toString $ map (prettyPrint Options(False,True) tdefs) doublenames ]
                        cmdsIntpr

-- ----------------------------------------------------------------------------------------- --

cmdEval :: String -> IOS.IOS ()
cmdEval args = do
     env            <- get
     let locVexpCtx  = IOS.locVexpCtx env
         locVarVals  = IOS.locVarVals env
     (vexp,e) <- lift $ lift $ catch
                          ( let p = compileUnsafe $ compileValExpr tdefs (Map.keys $ toMap vars) args
                              in return $!! (Just p,"")
                           )
                           ( \e -> return (Nothing,show (e::ErrorCall)))

     case vexp' of
       Just vexp'' -> do
                        -- modify $ \env' -> env' { IOS.uid = uid' }
                        mwalue <- lift $ txsEval (subst vars tdefs vexp'')
                        case mwalue of
                            Right walue -> do
                                            IFS.pack "EVAL" [ toString $ prettyPrint Options(False,True) tdefs walue ]
                                            cmdsIntpr

                            Left t      -> do
                                            IFS.nack "EVAL" [ "eval 2 - " ++ t ]
                                            cmdsIntpr

       Nothing -> do
                    IFS.nack "EVAL" [ "eval 1 - " ++ e ]
                    cmdsIntpr

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

