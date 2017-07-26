{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TxsCore
-- Copyright   :  TNO and Radboud University
-- License     :  BSD3
-- Maintainer  :  jan.tretmans
-- Stability   :  experimental
--
-- Core Module TorXakis API:
-- API for TorXakis core functionality.
-----------------------------------------------------------------------------
module TxsCore
( -- * run TorXakis core
  runTxsCore
  
  -- * initialize TorXakis core
, txsInit

  -- * terminate TorXakis core
, txsTermit

  -- * Mode
  -- ** start testing 
, txsSetTest

  -- *** test Input Action
, txsTestIn

  -- *** test Output Action
, txsTestOut

  -- *** test number of Actions
, txsTestN

  -- ** start simulating 
, txsSetSim

  -- *** simulate number of Actions
, txsSimN

  -- ** start stepping
, txsSetStep

  -- *** step Action
, txsStepA

  -- *** step number of Actions
, txsStepN

  -- *** go back to previous state
, txsGoTo

  -- ** stop testing, simulating, or stepping
, txsStop

  -- * Parameters
  -- ** get all parameter values
, txsGetParams

  -- ** get value of parameter
, txsGetParam

  -- ** set value of parameter
, txsSetParam

  -- * set random seed
, txsSetSeed

  -- * evaluation of value expression
, txsEval 

  -- * Solving
  -- ** finding a solution for value expression
, txsSolve

  -- ** finding an unique solution for value expression
, txsUniSolve

  -- ** finding a random solution for value expression
, txsRanSolve

  -- * show item
, txsShow

  -- * give path
, txsPath

  -- * give menu
, txsMenu

  -- * give action to mapper
, txsMapper

)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import Control.Monad
import Control.Monad.State
import System.Random

import qualified Data.Set  as Set
import qualified Data.Map  as Map

-- import from local
import Ioco
import Mapper
import Purpose
import CoreUtils
import Test
import Sim
import Step

import qualified Config
import Config (Config)

-- import from behave(defs)
import qualified BTree 
import qualified Behave

-- import from coreenv
import qualified EnvCore     as IOC
import qualified EnvData
import qualified ParamCore

-- import from defs
import qualified TxsDefs
import qualified SortOf
import qualified TxsDDefs
import qualified TxsShow
import qualified Sigs

-- import from solve
import qualified FreeVar
import qualified SMT
import qualified Solve
import qualified SolveDefs
import qualified SolveDefs.Params
-- import from value
import qualified Eval

-- | TorXakis core main api -- start
runTxsCore :: Config -> StateT s IOC.IOC a -> s -> IO ()
runTxsCore initConfig ctrl s0  =  do
     _ <- runStateT (runTxsCtrl ctrl s0)
               (IOC.EnvC { IOC.config = initConfig
                         , IOC.unid   = 0
                         , IOC.params = initParams
                         , IOC.state  = initState
                         }
               )
     return ()
       where initState = IOC.Noning
             initParams =
               Map.union ParamCore.initParams SolveDefs.Params.initParams

runTxsCtrl :: StateT s IOC.IOC a -> s -> IOC.IOC ()
runTxsCtrl ctrl s0  =  do 
     _ <- runStateT ctrl s0
     return ()

-- | TorXakis core main api -- modus transition general
txsInit :: TxsDefs.TxsDefs -> Sigs.Sigs TxsDefs.VarId -> ([EnvData.Msg] -> IOC.IOC ()) -> IOC.IOC ()
txsInit tdefs sigs putMsgs  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> do smtEnv         <- lift $ SMT.createSMTEnv SMT.cmdZ3 False tdefs
               (info,smtEnv') <- lift $ runStateT SMT.openSolver smtEnv
               (_,smtEnv'')   <- lift $ runStateT (SMT.addDefinitions tdefs) smtEnv'
               putMsgs [ EnvData.TXS_CORE_USER_INFO $ "Solver initialized : " ++ info
                       , EnvData.TXS_CORE_USER_INFO   "TxsCore initialized"
                       ]
               put envc {
                 IOC.state = 
                     IOC.Initing { IOC.smts    = Map.singleton "current" smtEnv''
                                 , IOC.tdefs   = tdefs
                                 , IOC.sigs    = sigs
                                 , IOC.putmsgs = putMsgs
                                 }
                 }
       IOC.Initing { }
         -> do TxsCore.txsTermit
               TxsCore.txsInit tdefs sigs putMsgs
       _ -> do TxsCore.txsStop                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
               TxsCore.txsTermit
               TxsCore.txsInit tdefs sigs putMsgs
       where mkSmtSolverCmd cfg =
               case Config.smtSolver cfg of
                 Config.Z3 -> SMT.cmdZ3
                 Config.CVC4 -> SMT.cmdCVC4
                    
-- | terminate TorXakis core
txsTermit :: IOC.IOC ()
txsTermit  =  do
     envc <- get
     case (IOC.state envc) of
       IOC.Noning
         -> return ()
       IOC.Initing { IOC.smts = smts, IOC.putmsgs = putmsgs }
         -> do lift $ mapM_ (runStateT SMT.close) (Map.elems smts)
               putmsgs [ EnvData.TXS_CORE_USER_INFO "Solver(s) closed"
                       , EnvData.TXS_CORE_USER_INFO "TxsCore terminated"
                       ]
               put envc { IOC.state = IOC.Noning }
       _ -> do TxsCore.txsStop                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
               TxsCore.txsTermit

-- | stop testing, simulating, or stepping.
-- See 'txsSetTest', 'txsSetSim', and 'txsSetStep', respectively.
txsStop :: IOC.IOC ()
txsStop  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> return ()
       IOC.Initing {}
         -> return ()
       _ -> do                                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Simulation/Testing/Stepping stopped" ]
               let st = IOC.state envc
               put envc {
                 IOC.state =
                     IOC.Initing { IOC.smts    = IOC.smts    st
                                 , IOC.tdefs   = IOC.tdefs   st
                                 , IOC.sigs    = IOC.sigs    st
                                 , IOC.putmsgs = IOC.putmsgs st
                                 }
                 }

-- | Get the values of all parameters.
txsGetParams :: IOC.IOC [(String,String)]
txsGetParams  =
     IOC.getParams []

-- | Get the value of the provided parameter.
txsGetParam :: String -> IOC.IOC [(String,String)]
txsGetParam prm  =
     IOC.getParams [prm]

-- | Set the provided parameter to the provided value.
txsSetParam :: String -> String -> IOC.IOC [(String,String)]
txsSetParam prm val  =
     IOC.setParams [(prm,val)]

-- | Set the random seed to the provided value.
txsSetSeed :: Int -> IOC.IOC ()
txsSetSeed seed  =  do
     lift $ setStdGen(mkStdGen seed)
     IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_INFO $ "Seed set to " ++ show seed ]

-- | Evaluate the provided value expression.
--
--   Only possible when txscore is initialized with a model.
txsEval :: TxsDefs.VExpr -> IOC.IOC TxsDefs.Const
txsEval vexp  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR $ "No 'eval' without model" ]
               return $ TxsDefs.Cerror ""
       _ -> let frees = FreeVar.freeVars vexp
            in if  not $ null frees
                     then do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                           $ "Value expression not closed: " ++
                                             TxsShow.fshow frees ]
                             return $ TxsDefs.Cerror ""
                     else do envb         <- filterEnvCtoEnvB
                             (wal',envb') <- lift $ runStateT (Eval.eval vexp) envb
                             writeEnvBtoEnvC envb'
                             return wal'

-- | Find a solution for the provided Boolean value expression.
--
--   Only possible when txscore is initialized with a model.
txsSolve :: TxsDefs.VExpr -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
txsSolve vexp  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR $ "No 'solve' without model" ]
               return Map.empty
       _ -> if  SortOf.sortOf vexp /= SortOf.sortId_Bool
                 then do
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                 "Value expression for solve shall be Bool" ]
                   return Map.empty
                 else do
                   let frees = FreeVar.freeVars vexp
                       assertions = Solve.add vexp Solve.empty
                   smtEnv        <- IOC.getSMT "current"
                   (sat,smtEnv') <- lift $ runStateT (Solve.solve frees assertions) smtEnv
                   IOC.putSMT "current" smtEnv'
                   case sat of
                     SolveDefs.Solved sol    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "sat" ]
                                                   return sol
                     SolveDefs.Unsolvable    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "unsat" ]
                                                   return Map.empty
                     SolveDefs.UnableToSolve -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "unknown" ]
                                                   return Map.empty
          
                                                   
-- | Find an unique solution for the provided Boolean value expression.
--
--   Only possible when txscore is initialized with a model.
txsUniSolve :: TxsDefs.VExpr -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
txsUniSolve vexp  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR $ "No 'solve' without model" ]
               return Map.empty
       _ -> if  SortOf.sortOf vexp /= SortOf.sortId_Bool
                 then do
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Value expression shall be Bool" ]
                   return Map.empty
                 else do
                   let frees = FreeVar.freeVars vexp
                       assertions = Solve.add vexp Solve.empty
                   smtEnv        <- IOC.getSMT "current"
                   (sat,smtEnv') <- lift $ runStateT (Solve.uniSolve frees assertions) smtEnv
                   IOC.putSMT "current" smtEnv'
                   case sat of
                     SolveDefs.Solved sol    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "sat" ]
                                                   return sol
                     SolveDefs.Unsolvable    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "unsat" ]
                                                   return Map.empty
                     SolveDefs.UnableToSolve -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "unknown" ]
                                                   return Map.empty

-- | Find a random solution for the provided Boolean value expression.
--
--   Only possible when txscore is initialized with a model.
txsRanSolve :: TxsDefs.VExpr -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
txsRanSolve vexp  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR $ "No 'solve' without model" ]
               return Map.empty
       _ -> if  SortOf.sortOf vexp /= SortOf.sortId_Bool
                 then do
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Value expression shall be Bool" ]
                   return Map.empty
                else do
                   let frees      = FreeVar.freeVars vexp
                       assertions = Solve.add vexp Solve.empty
                   smtEnv        <- IOC.getSMT "current"
                   parammap <- gets IOC.params
                   let p = Solve.toRandParam parammap
                   (sat,smtEnv') <- lift $ runStateT (Solve.randSolve p frees assertions) smtEnv
                   IOC.putSMT "current" smtEnv'
                   case sat of
                     SolveDefs.Solved sol    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "sat" ]
                                                   return sol
                     SolveDefs.Unsolvable    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "unsat" ]
                                                   return Map.empty
                     SolveDefs.UnableToSolve -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE
                                                                 "unknown" ]
                                                   return Map.empty
          

-- | Start testing using the provided
-- 
--   * callback function for sending an input action to the SUT (world),
--
--   * callback function for receiving an output action from the SUT (world),
--
--   * model definition,
--
--   * optional mapper definition, and
--
--   * optional test purpose definition.
--
--   Only possible when txscore is initialized with a model.
--
-- modus transition general.
txsSetTest :: (TxsDDefs.Action -> IOC.IOC TxsDDefs.Action) ->
              IOC.IOC TxsDDefs.Action ->
              TxsDefs.ModelDef -> Maybe TxsDefs.MapperDef -> Maybe TxsDefs.PurpDef ->
              IOC.IOC ()
txsSetTest putToW getFroW moddef mapdef purpdef  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester started without model file" ]
       IOC.Initing smts tdefs sigs putmsgs -> do
         (maybt,mt,gls) <- startTester moddef mapdef purpdef
         case maybt of
           Nothing ->
             IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester start failed" ]
           Just bt -> do
             put envc {
               IOC.state =
                   IOC.Testing { IOC.smts      = smts
                               , IOC.tdefs     = tdefs
                               , IOC.sigs      = sigs
                               , IOC.modeldef  = moddef
                               , IOC.mapperdef = mapdef
                               , IOC.purpdef   = purpdef
                               , IOC.puttow    = putToW
                               , IOC.getfrow   = getFroW
                               , IOC.behtrie   = []
                               , IOC.inistate  = 0
                               , IOC.curstate  = 0
                               , IOC.modsts    = bt
                               , IOC.mapsts    = mt
                               , IOC.purpsts   = gls
                               , IOC.putmsgs   = putmsgs
                               }
               }
             IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Tester started" ]
       _ -> do                                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
            TxsCore.txsStop
            TxsCore.txsSetTest putToW getFroW moddef mapdef purpdef

startTester :: TxsDefs.ModelDef ->
               Maybe TxsDefs.MapperDef ->
               Maybe TxsDefs.PurpDef ->
               IOC.IOC ( Maybe BTree.BTree, BTree.BTree, [(TxsDefs.GoalId,BTree.BTree)] )

startTester (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
                 Nothing
                 Nothing =
     let allSyncs = minsyncs ++ moutsyncs ++ msplsyncs
     in do 
       envb            <- filterEnvCtoEnvB
       (maybt', envb') <- lift $ runStateT (Behave.behInit allSyncs mbexp) envb
       writeEnvBtoEnvC envb'
       return ( maybt', [], [] )

startTester (TxsDefs.ModelDef  minsyncs moutsyncs msplsyncs mbexp)
                 (Just (TxsDefs.MapperDef achins achouts asyncsets abexp))
                 Nothing =
     let { mins   = Set.unions minsyncs
         ; mouts  = Set.unions moutsyncs
         ; ains   = Set.fromList achins
         ; aouts  = Set.fromList achouts
         }
      in if     mins  `Set.isSubsetOf` ains
             && mouts `Set.isSubsetOf` aouts
           then do let allSyncs = minsyncs ++ moutsyncs ++ msplsyncs
                   envb            <- filterEnvCtoEnvB
                   (maybt',envb' ) <- lift $ runStateT (Behave.behInit allSyncs  mbexp) envb
                   (maymt',envb'') <- lift $ runStateT (Behave.behInit asyncsets abexp) envb'
                   case (maybt',maymt') of
                     (Nothing , _       ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester model failed" ]
                          return ( Nothing, [], [] )
                     (_       , Nothing ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester mapper failed" ]
                          return ( Nothing, [], [] )
                     (Just _, Just mt') -> do
                          writeEnvBtoEnvC envb''
                          return ( maybt', mt', [] )
           else do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Inconsistent definitions" ]
                   return ( Nothing, [], [] )

startTester (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
                 Nothing
                 (Just (TxsDefs.PurpDef pinsyncs poutsyncs psplsyncs goals)) =
     let { mins   = Set.unions minsyncs
         ; mouts  = Set.unions moutsyncs
         ; pins   = Set.unions pinsyncs
         ; pouts  = Set.unions poutsyncs
         }
      in if     ( (pins  == Set.empty) || (pins  == mins)  )
             && ( (pouts == Set.empty) || (pouts == mouts) )
           then do let allSyncs  = minsyncs ++ moutsyncs ++ msplsyncs
                       pAllSyncs = pinsyncs ++ poutsyncs ++ psplsyncs
                   envb           <- filterEnvCtoEnvB
                   (maybt',envb') <- lift $ runStateT (Behave.behInit allSyncs  mbexp) envb
                   case maybt' of
                     Nothing -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester model failed" ]
                          return ( Nothing, [], [] )
                     Just _ -> do
                          writeEnvBtoEnvC envb'
                          gls <- mapM (goalInit pAllSyncs) goals
                          return ( maybt', [], concat gls )
           else do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Inconsistent definitions" ]
                   return ( Nothing, [], [] )

startTester (TxsDefs.ModelDef  minsyncs moutsyncs msplsyncs mbexp)
                 (Just (TxsDefs.MapperDef achins achouts asyncsets abexp))
                 (Just (TxsDefs.PurpDef pinsyncs poutsyncs psplsyncs goals)) =
     let { mins   = Set.unions minsyncs
         ; mouts  = Set.unions moutsyncs
         ; ains   = Set.fromList achins
         ; aouts  = Set.fromList achouts
         ; pins   = Set.unions pinsyncs
         ; pouts  = Set.unions poutsyncs
         }
      in if     ( (pins  == Set.empty) || (pins  == mins)  )
             && ( (pouts == Set.empty) || (pouts == mouts) )
             && mins  `Set.isSubsetOf` ains
             && mouts `Set.isSubsetOf` aouts
           then do let allSyncs  = minsyncs ++ moutsyncs ++ msplsyncs
                       pAllSyncs = pinsyncs ++ poutsyncs ++ psplsyncs
                   envb            <- filterEnvCtoEnvB
                   (maybt',envb')  <- lift $ runStateT (Behave.behInit allSyncs  mbexp) envb
                   (maymt',envb'') <- lift $ runStateT (Behave.behInit asyncsets abexp) envb'
                   case (maybt',maymt') of
                     (Nothing , _       ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester model failed" ]
                          return ( Nothing, [], [] )
                     (_       , Nothing ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester mapper failed" ]
                          return ( Nothing, [], [] )
                     (Just _, Just mt') -> do
                          writeEnvBtoEnvC envb''
                          gls <- mapM (goalInit pAllSyncs) goals
                          return ( maybt', mt', concat gls )
           else do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Inconsistent definitions" ]
                   return ( Nothing, [], [] )

goalInit :: [ Set.Set TxsDefs.ChanId ] -> 
            (TxsDefs.GoalId,TxsDefs.BExpr) ->
            IOC.IOC [(TxsDefs.GoalId,BTree.BTree)]
goalInit chsets (gid,bexp)  =  do
     envb           <- filterEnvCtoEnvB
     (maypt',envb') <- lift $ runStateT (Behave.behInit chsets bexp) envb
     writeEnvBtoEnvC envb'
     return $ case maypt' of
              { Nothing  -> []   
              ; Just pt' -> [ (gid, pt') ]
              }
                   
-- | Start simulating using the provided
--
--   * callback function for sending an output action to the environment (world),
--
--   * callback function for receiving an input action from the environment (world),
--
--   * model definition, and
--
--   * optional mapper definition.
--
--   Only possible when txscore is initialized with a model.
--
-- modus transition general.
txsSetSim :: (TxsDDefs.Action -> IOC.IOC TxsDDefs.Action) ->
             IOC.IOC TxsDDefs.Action ->
             TxsDefs.ModelDef -> Maybe TxsDefs.MapperDef ->
             IOC.IOC ()
txsSetSim putToW getFroW moddef mapdef  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Simulator started without model file" ]
       IOC.Initing smts tdefs sigs putmsgs -> do
         (maybt,mt) <- startSimulator moddef mapdef
         case maybt of
           Nothing -> 
             IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Simulator start failed" ]
           Just bt -> do
             put envc {
               IOC.state =
                   IOC.Simuling { IOC.smts      = smts
                                , IOC.tdefs     = tdefs
                                , IOC.sigs      = sigs
                                , IOC.modeldef  = moddef
                                , IOC.mapperdef = mapdef
                                , IOC.puttow    = putToW
                                , IOC.getfrow   = getFroW
                                , IOC.behtrie   = []
                                , IOC.inistate  = 0
                                , IOC.curstate  = 0
                                , IOC.modsts    = bt
                                , IOC.mapsts    = mt
                                , IOC.putmsgs   = putmsgs
                                }
               }
             IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Simulator started" ]
       _ -> do                                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
            TxsCore.txsStop
            TxsCore.txsSetSim putToW getFroW moddef mapdef

startSimulator :: TxsDefs.ModelDef ->
                  Maybe TxsDefs.MapperDef ->
                  IOC.IOC ( Maybe BTree.BTree, BTree.BTree )

startSimulator (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
                Nothing =
     let allSyncs = minsyncs ++ moutsyncs ++ msplsyncs
     in do
       envb            <- filterEnvCtoEnvB
       (maybt', envb') <- lift $ runStateT (Behave.behInit allSyncs mbexp) envb
       writeEnvBtoEnvC envb'
       return ( maybt', [] )

startSimulator (TxsDefs.ModelDef  minsyncs moutsyncs msplsyncs mbexp)
                (Just (TxsDefs.MapperDef achins achouts asyncsets abexp))
  =  let { mins   = Set.unions minsyncs
         ; mouts  = Set.unions moutsyncs
         ; ains   = Set.fromList achins
         ; aouts  = Set.fromList achouts
         }
      in if     mouts `Set.isSubsetOf` ains
             && mins  `Set.isSubsetOf` aouts
           then do let allSyncs = minsyncs ++ moutsyncs ++ msplsyncs
                   envb            <- filterEnvCtoEnvB
                   (maybt',envb' ) <- lift $ runStateT (Behave.behInit allSyncs  mbexp) envb
                   (maymt',envb'') <- lift $ runStateT (Behave.behInit asyncsets abexp) envb'
                   case (maybt',maymt') of
                     (Nothing , _       ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester model failed" ]
                          return ( Nothing, [] )
                     (_       , Nothing ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester mapper failed" ]
                          return ( Nothing, [] )
                     (Just _, Just mt') -> do
                          writeEnvBtoEnvC envb''
                          return ( maybt', mt' )
           else do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Inconsistent definitions" ]
                   return ( Nothing, [] )

-- | Start stepping using the provided model definition.
--
--   Only possible when txscore is initialized with a model.
--
-- modus transition general.
txsSetStep :: TxsDefs.ModelDef -> IOC.IOC ()
txsSetStep moddef  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning -> do
            IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Stepper started without model file" ]
       IOC.Initing { .. } -> do
            maybt <- startStepper moddef
            case maybt of
              Nothing -> 
                IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Stepper start failed" ]
              Just bt -> do
                put envc {
                  IOC.state =
                      IOC.Stepping { IOC.smts      = smts
                                   , IOC.tdefs     = tdefs
                                   , IOC.sigs      = sigs
                                   , IOC.modeldef  = moddef
                                   , IOC.behtrie   = []
                                   , IOC.inistate  = 0
                                   , IOC.curstate  = 0
                                   , IOC.maxstate  = 0
                                   , IOC.modstss   = Map.singleton 0 bt
                                   , IOC.putmsgs   = putmsgs
                                   }
                  }
                IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Stepper started" ]
       _ -> do                                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
            TxsCore.txsStop
            TxsCore.txsSetStep moddef


startStepper :: TxsDefs.ModelDef ->
                IOC.IOC ( Maybe BTree.BTree )

startStepper (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)  =  do
     let allSyncs = minsyncs ++ moutsyncs ++ msplsyncs
     envb            <- filterEnvCtoEnvB
     (maybt', envb') <- lift $ runStateT (Behave.behInit allSyncs mbexp) envb
     writeEnvBtoEnvC envb'
     return maybt'

-- | Test SUT with the provided input action.
-- core action.
--
-- Only possible in test modus (see 'txsSetTest').
-- Not possible with test purpose.
txsTestIn :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Verdict
txsTestIn act  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing { purpdef = Nothing}
         -> do Test.testIn act 1
       IOC.Testing { }
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "No test action with test purpose" ]
               return $ TxsDDefs.NoVerdict
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Tester mode" ]
               return $ TxsDDefs.NoVerdict

-- | Test SUT by observing output action.
-- core action.
--
-- Only possible in test modus (see 'txsSetTest').
-- Not possible with test purpose.
txsTestOut :: IOC.IOC TxsDDefs.Verdict
txsTestOut  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing { purpdef = Nothing } -> Test.testOut 1
       IOC.Testing { } -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "No test output with test purpose" ]
         return $ TxsDDefs.NoVerdict
       _ -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Tester mode" ]
         return TxsDDefs.NoVerdict

-- | Test SUT with the provided number of actions.
-- core action.
--
-- Only possible in test modus (see 'txsSetTest').
txsTestN :: Int -> IOC.IOC TxsDDefs.Verdict
txsTestN depth  =  do  
     envc <- get
     case IOC.state envc of
      IOC.Testing { }-> Test.testN depth 1
      _ -> do
        IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Tester mode" ]
        return TxsDDefs.NoVerdict

-- | Simulate model with the provided number of actions.
-- core action.
--
-- Only possible in simulation modus (see 'txsSetSim').
txsSimN :: Int -> IOC.IOC TxsDDefs.Verdict
txsSimN depth  =  do
     envc <- get
     case IOC.state envc of
       IOC.Simuling {} -> Sim.simN depth 1
       _ -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Simulator mode" ]
         return TxsDDefs.NoVerdict

-- | Step model with the provided number of actions.
-- core action.
--
-- Only possible in stepper modus (see 'txsSetStep').
txsStepN :: Int -> IOC.IOC TxsDDefs.Verdict
txsStepN depth  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping {} -> do Step.stepN depth 1
       _ -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Stepper mode" ]
         return TxsDDefs.NoVerdict

-- | Step model with the provided action.
-- core action.
--
-- Only possible in stepper modus (see 'txsSetStep').
txsStepA :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Verdict
txsStepA act  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping {} -> do Step.stepA act
       _ -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Stepper mode" ]
         return  TxsDDefs.NoVerdict

-- | Show provided item.
--
-- Valid items are:
--
--     - `"tdefs"`
--     - `"state"`
--     - `"model"`
--     - `"mapper"`
--     - `"purp"`
txsShow :: String -> IOC.IOC String
txsShow item  =  do
     st <- gets IOC.state
     case item of
       "tdefs"  -> return $ show (IOC.tdefs st)
       "state"  -> return $ show (IOC.curstate st)
       "model"  -> return $ TxsShow.fshow (IOC.modsts st)
       "mapper" -> return $ TxsShow.fshow (IOC.mapsts st)
       "purp"   -> return $ TxsShow.fshow (IOC.purpsts st)
       _        -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR $ "nothing to be shown" ]
         return $ "\n"

-- | Go to state with the provided state number.
-- core action.
--
-- Only possible in stepper modus (see 'txsSetStep').
txsGoTo :: EnvData.StateNr -> IOC.IOC ()
txsGoTo stateNr  =
  if  stateNr >= 0
  then do
    modStss <- gets (IOC.modstss . IOC.state)
    case Map.lookup stateNr modStss of
       Nothing -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR $ "no such state" ]
       Just bt ->
         modify $
           \env ->
             env { IOC.state =
                     (IOC.state env)
                     { IOC.curstate = stateNr }
                 }
  else do
    ltsBackN (-stateNr)
  where
     ltsBackN :: Int -> IOC.IOC ()
     ltsBackN backsteps
        | backsteps <= 0 = return ()
        | backsteps > 0  = do
            st <- gets IOC.state
            let iniState = IOC.inistate st
                curState = IOC.curstate st
                behTrie = IOC.behtrie st
            case [ s | (s,a,s') <- behTrie, s' == curState ] of
              [prev] -> do
                modify $
                  \env ->
                    env { IOC.state =
                            (IOC.state env) {
                            IOC.curstate = prev
                            }
                        }
                unless (prev == iniState) (ltsBackN (backsteps-1))
              _      -> do
                IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "LtsBack error" ]
                return ()

-- | Provide the path.
txsPath :: IOC.IOC [(EnvData.StateNr, TxsDDefs.Action, EnvData.StateNr)]
txsPath  =  do
  st <- gets IOC.state
  path (IOC.inistate st) (IOC.curstate st)
  where
     path :: EnvData.StateNr -> EnvData.StateNr ->
             IOC.IOC [(EnvData.StateNr, TxsDDefs.Action, EnvData.StateNr)]
     path from to | from >= to = return []
     path from to = do -- from < to
       iniState <- gets (IOC.inistate . IOC.state)
       behTrie  <- gets (IOC.behtrie . IOC.state)
       case [ (s1,a,s2) | (s1,a,s2) <- behTrie, s2 == to ] of
         [(s1,a,s2)] ->
           if (s1 == from) || (s1 == iniState)
           then return [(s1,a,s2)]
           else do
             pp <- path from s1
             return $ pp ++ [(s1,a,s2)]
         _           -> do
           IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "Path error" ]
           return []

-- | Returns the menu for the provided
--
--   * kind ("mod" or "purp")
--
--   * what ("all", "in", or "out"), and
--
--   * state number.
txsMenu :: String -> String -> EnvData.StateNr -> IOC.IOC BTree.Menu
txsMenu kind what stnr  =  do
     curState <- gets (IOC.curstate . IOC.state)
     let stateNr = if stnr == -1 then curState else stnr
     case kind of
       "mod"  -> do txsGoTo stateNr
                    menuIn   <- Ioco.iocoModelMenuIn
                    menuOut  <- Ioco.iocoModelMenuOut
                    txsGoTo curState
                    case what of
                      "all" -> return $ menuIn ++ menuOut
                      "in"  -> return menuIn
                      "out" -> return menuOut
                      _     -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                "error in menu" ]
                                  return []
       "purp" -> do txsGoTo stateNr
                    gmenu <- Purpose.goalMenu what
                    txsGoTo curState
                    return gmenu
       _      -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "error in menu" ]
                    return []

-- | Give the provided action to the mapper.
--
-- Not possible in stepper modus (see 'txsSetStep').
txsMapper :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Action
txsMapper act  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing {} -> mapperMap act
       IOC.Simuling {} -> mapperMap act
       _ -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                        "Mapping only allowed in Testing or Simulating mode" ]
         return act

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

