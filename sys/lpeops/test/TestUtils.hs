{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module TestUtils
(
createTestEnvC,
printInputExpectedFound,
validateLPE,
tryLPEOperation,
module LPETypes
)
where

import Test.HUnit
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified EnvData
import Control.Monad.State
import TxsDefs
import qualified SMT
import Sigs

import TxsShow
import qualified Config
import qualified EnvCore as IOC
import qualified ParamCore
import qualified Solve.Params
import qualified LPEValidity as Val
import LPEPrettyPrint
import LPEEquivalence
import LPETypes
import ValExpr
import Constant

createTestEnvC :: IO IOC.EnvC
createTestEnvC = do
    smtEnv <- SMT.createSMTEnv (Maybe.fromJust (Config.getProc initConfig)) False -- Set to True to write SMT solver logs!
    (_info,smtEnv') <- runStateT SMT.openSolver smtEnv
    return $ IOC.EnvC { IOC.config = initConfig
                      , IOC.unid   = 0
                      , IOC.params = Config.updateParamVals initParams $ Config.configuredParameters initConfig
                      , IOC.state  = initState { IOC.smts = Map.singleton "current" smtEnv' }
                      }
  where
    initConfig = Config.defaultConfig
    initState = IOC.Initing { IOC.smts = Map.empty
                            , IOC.tdefs = TxsDefs.empty
                            , IOC.sigs = Sigs.empty
                            , IOC.putmsgs = putMsgs
                            , IOC.chanoffers = Map.empty
                            }
    initParams = Map.union ParamCore.initParams Solve.Params.initParams
-- createTestEnvC

putMsgs :: [EnvData.Msg] -> IOC.IOC ()
putMsgs msgs = do printMsg msgs
                  return ()
  where
    printMsg :: [EnvData.Msg] -> IOC.IOC ()
    printMsg [] = return ()
    printMsg (x:xs) = do liftIO $ putStrLn (pshow x)
                         printMsg xs
                         return ()
-- putMsgs

printInputExpectedFound :: LPE -> LPE -> LPE -> String
printInputExpectedFound input expected found =
    "\nInput:\n\n" ++ showLPE input ++
    "\n\nExpected output:\n\n" ++ showLPE expected ++
    "\n\nActual output:\n\n" ++ showLPE found ++ "\n"
-- printInputExpectedFound

validateLPE :: LPE -> [String]
validateLPE = Val.validateLPEModel

tryLPEOperation :: LPEOperation -> LPE -> LPE -> IO ()
tryLPEOperation op input expected =
    case validateLPE input of
      [] -> case validateLPE expected of
              [] -> do env <- createTestEnvC
                       msgsOrFound <- evalStateT (op input "Out" (cstrConst (Cbool True))) env
                       case msgsOrFound of
                         Left msgs -> assertBool ("\nCould not produce output LPE:\n\n" ++ List.intercalate "\n" msgs ++ "\n") False
                         Right found -> do equivalent <- evalStateT (isEquivalentLPE found expected (cstrConst (Cbool True))) env
                                           assertBool (printInputExpectedFound input expected found) equivalent
              msgs -> assertBool ("\nInvalid expected LPE:\n\n" ++ showLPE expected ++ "\nProblems:\n\n" ++ List.intercalate "\n" msgs ++ "\n") False
      msgs -> assertBool ("\nInvalid input LPE:\n\n" ++ showLPE input ++ "\nProblems:\n\n" ++ List.intercalate "\n" msgs ++ "\n") False
-- tryLPEOperation


