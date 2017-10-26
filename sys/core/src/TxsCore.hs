{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


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
{-# LANGUAGE OverloadedStrings #-}
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

  -- * test purpose for N complete coverage
, txsNComp

  -- * LPE transformation
, txsLPE
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad
import           Control.Monad.State
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           System.Random

-- import from local
import           CoreUtils
import           Ioco
import           Mapper
import           NComp
import           Purpose
import           Sim
import           Step
import           Test

import           Config              (Config)
import qualified Config

-- import from behave(defs)
import qualified Behave
import qualified BTree
import           Expand              (relabel)

-- import from coreenv
import qualified EnvCore             as IOC
import qualified EnvData
import qualified ParamCore

-- import from defs
import qualified Sigs
import qualified SortOf
import qualified TxsDDefs
import qualified TxsDefs
import qualified TxsShow

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
               IOC.EnvC { IOC.config = initConfig
                         , IOC.unid   = 0
                         , IOC.params = initParams
                         , IOC.state  = initState
                         }
     return ()
       where initState = IOC.Noning
             initParams =
               Map.union ParamCore.initParams SolveDefs.Params.initParams

runTxsCtrl :: StateT s IOC.IOC a -> s -> IOC.IOC ()
runTxsCtrl ctrl s0  =  do
     _ <- runStateT ctrl s0
     return ()

-- | TorXakis core main api -- modus transition general
txsInit :: TxsDefs.TxsDefs                  -- ^ Definitions for computations.
        -> Sigs.Sigs TxsDefs.VarId          -- ^ Signatures needed to parse.
        -> ([EnvData.Msg] -> IOC.IOC ())    -- ^ Handler for info, warning, and error messages.
        -> IOC.IOC ()
txsInit tdefs sigs putMsgs  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> do
               let cfg    = IOC.config envc
                   smtLog = Config.smtLog cfg
                   -- An error will be thrown if the selected solver is not in
                   -- the list of available solvers. The sanity of the
                   -- configuration is checked outside this function, however
                   -- nothing prevents a client of this function from injecting
                   -- a wrong configuration. A nicer error handling requires
                   -- some refactoring of the TorXakis core to take this into
                   -- account.
                   smtProc = fromJust (Config.getProc cfg)
               smtEnv         <- lift $ SMT.createSMTEnv smtProc smtLog tdefs
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
       IOC.Initing {}
         -> do TxsCore.txsTermit
               TxsCore.txsInit tdefs sigs putMsgs
       _ -> do TxsCore.txsStop                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
               TxsCore.txsTermit
               TxsCore.txsInit tdefs sigs putMsgs

-- | terminate TorXakis core
txsTermit :: IOC.IOC ()
txsTermit  =  do
     envc <- get
     case IOC.state envc of
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
-- returns txscore to the initialized state.
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
txsGetParam :: String                       -- ^ name of the parameter.
            -> IOC.IOC [(String,String)]
txsGetParam prm  =
     IOC.getParams [prm]

-- | Set the provided parameter to the provided value.
txsSetParam :: String                       -- ^ name of the parameter.
            -> String                       -- ^ new value for the parameter.
            -> IOC.IOC [(String,String)]
txsSetParam prm val  =
     IOC.setParams [(prm,val)]

-- | Set the random seed to the provided value.
txsSetSeed :: Int                           -- ^ new value for seed.
           -> IOC.IOC ()
txsSetSeed seed  =  do
     lift $ setStdGen(mkStdGen seed)
     IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_INFO $ "Seed set to " ++ show seed ]

-- | Evaluate the provided value expression.
--
--   Only possible when txscore is initialized.
txsEval :: TxsDefs.VExpr                    -- ^ value expression to be evaluated.
        -> IOC.IOC TxsDefs.Const
txsEval vexp  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "No 'eval' without model" ]
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
--   Only possible when txscore is initialized.
txsSolve :: TxsDefs.VExpr                   -- ^ value expression to solve.
         -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
txsSolve vexp  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR  "No 'solve' without model" ]
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
--   Only possible when txscore is initialized.
txsUniSolve :: TxsDefs.VExpr            -- ^ value expression to solve uniquely.
            -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
txsUniSolve vexp  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "No 'solve' without model" ]
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
--   Only possible when txscore is initialized.
txsRanSolve :: TxsDefs.VExpr                -- ^ value expression to solve randomly.
            -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
txsRanSolve vexp  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "No 'solve' without model" ]
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


-- | Start testing.
--
--   Only possible when txscore is initialized.
--
-- modus transition general.
txsSetTest :: (TxsDDefs.Action -> IOC.IOC TxsDDefs.Action)  -- ^ callback function for sending an input action to the SUT (world).
                                                            --   The callback function is called with a proposed input action.
                                                            --   When the SUT has produced an output action,
                                                            --   the callback function must return that output action,
                                                            --   otherwise the callback function must return the input action.
           -> IOC.IOC TxsDDefs.Action                       -- ^ callback function for receiving an output action from the SUT (world).
                                                            --   The callback function signals that the SUT has produced an output action.
           -> TxsDefs.ModelDef                              -- ^ model definition.
           -> Maybe TxsDefs.MapperDef                       -- ^ optional mapper definition.
           -> Maybe TxsDefs.PurpDef                         -- ^ optional test purpose definition.
           -> IOC.IOC ()
txsSetTest putToW getFroW moddef mapdef purpdef  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning {} ->
            IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester started without model file" ]
       IOC.Initing { IOC.smts = smts
                   , IOC.tdefs = tdefs
                   , IOC.sigs = sigs
                   , IOC.putmsgs = putmsgs} -> do
            IOC.putCS IOC.Testing { IOC.smts      = smts
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
                                 , IOC.modsts    = []
                                 , IOC.mapsts    = []
                                 , IOC.purpsts   = []
                                 , IOC.putmsgs   = putmsgs
                                 }
            (maybt,mt,gls) <- startTester moddef mapdef purpdef
            case maybt of
              Nothing ->
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester start failed" ]
              Just bt -> do
                   IOC.modifyCS $ \st -> st { IOC.modsts  = bt
                                            , IOC.mapsts  = mt
                                            , IOC.purpsts = gls
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
            Nothing  =
     let allSyncs = minsyncs ++ moutsyncs ++ msplsyncs
     in do
       envb            <- filterEnvCtoEnvB
       (maybt', envb') <- lift $ runStateT (Behave.behInit allSyncs mbexp) envb
       writeEnvBtoEnvC envb'
       return ( maybt', [], [] )

startTester (TxsDefs.ModelDef  minsyncs moutsyncs msplsyncs mbexp)
            (Just (TxsDefs.MapperDef achins achouts asyncsets abexp))
            Nothing  =
     let { mins  = Set.fromList minsyncs
         ; mouts = Set.fromList moutsyncs
         ; ains  = Set.fromList $ filter (not . Set.null)
                       [ sync `Set.intersection` Set.fromList achins  | sync <- asyncsets ]
         ; aouts = Set.fromList $ filter (not . Set.null)
                       [ sync `Set.intersection` Set.fromList achouts | sync <- asyncsets ]
         }
      in if     mins  `Set.isSubsetOf` ains
             && mouts `Set.isSubsetOf` aouts
           then do let allSyncs = minsyncs ++ moutsyncs ++ msplsyncs
                   envb            <- filterEnvCtoEnvB
                   (maybt',envb' ) <- lift $ runStateT (Behave.behInit allSyncs  mbexp) envb
                   (maymt',envb'') <- lift $ runStateT (Behave.behInit asyncsets abexp) envb'
                   writeEnvBtoEnvC envb''
                   case (maybt',maymt') of
                     (Nothing , _       ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester model failed" ]
                          return ( Nothing, [], [] )
                     (_       , Nothing ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester mapper failed" ]
                          return ( Nothing, [], [] )
                     (Just _, Just mt') ->
                          return ( maybt', mt', [] )
           else do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Inconsistent definitions" ]
                   return ( Nothing, [], [] )

startTester (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
            Nothing
            (Just (TxsDefs.PurpDef pinsyncs poutsyncs psplsyncs goals))  =
     let { mins   = Set.fromList minsyncs
         ; mouts  = Set.fromList moutsyncs
         ; pins   = Set.fromList pinsyncs
         ; pouts  = Set.fromList poutsyncs
         }
      in if     ( (pins  == Set.empty) || (pins  == mins)  )
             && ( (pouts == Set.empty) || (pouts == mouts) )
           then do let allSyncs  = minsyncs ++ moutsyncs ++ msplsyncs
                       pAllSyncs = pinsyncs ++ poutsyncs ++ psplsyncs
                   envb           <- filterEnvCtoEnvB
                   (maybt',envb') <- lift $ runStateT (Behave.behInit allSyncs mbexp) envb
                   writeEnvBtoEnvC envb'
                   case maybt' of
                     Nothing -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester model failed" ]
                          return ( Nothing, [], [] )
                     Just _ -> do
                          gls <- mapM (goalInit pAllSyncs) goals
                          return ( maybt', [], concat gls )
           else do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Inconsistent definitions" ]
                   return ( Nothing, [], [] )

startTester (TxsDefs.ModelDef  minsyncs moutsyncs msplsyncs mbexp)
            (Just (TxsDefs.MapperDef achins achouts asyncsets abexp))
            (Just (TxsDefs.PurpDef pinsyncs poutsyncs psplsyncs goals))  =
     let { mins  = Set.fromList minsyncs
         ; mouts = Set.fromList moutsyncs
         ; ains  = Set.fromList $ filter (not . Set.null)
                       [ sync `Set.intersection` Set.fromList achins  | sync <- asyncsets ]
         ; aouts = Set.fromList $ filter (not . Set.null)
                       [ sync `Set.intersection` Set.fromList achouts | sync <- asyncsets ]
         ; pins  = Set.fromList pinsyncs
         ; pouts = Set.fromList poutsyncs
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
                   writeEnvBtoEnvC envb''
                   case (maybt',maymt') of
                     (Nothing , _       ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester model failed" ]
                          return ( Nothing, [], [] )
                     (_       , Nothing ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester mapper failed" ]
                          return ( Nothing, [], [] )
                     (Just _, Just mt') -> do
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

-- | Start simulating.
--
--   Only possible when txscore is initialized.
--
-- modus transition general.
txsSetSim :: (TxsDDefs.Action -> IOC.IOC TxsDDefs.Action)   -- ^callback function for sending an output action to the environment (world).
                                                            --   The callback function is called with a proposed output action.
                                                            --   When the environment has produced an input action,
                                                            --   the callback function must return that input action,
                                                            --   otherwise the callback function must return the output action.
          -> IOC.IOC TxsDDefs.Action                        -- ^ callback function for receiving an input action from the environment (world).
                                                            --   The callback function signals that the environment has produced an input action.
          -> TxsDefs.ModelDef                               -- ^ model definition.
          -> Maybe TxsDefs.MapperDef                        -- ^ optional mapper definition.
          -> IOC.IOC ()
txsSetSim putToW getFroW moddef mapdef  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning {} ->
            IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Simulator started without model file" ]
       IOC.Initing { IOC.smts = smts
                   , IOC.tdefs = tdefs
                   , IOC.sigs = sigs
                   , IOC.putmsgs = putmsgs} -> do
            IOC.putCS IOC.Simuling { IOC.smts      = smts
                                   , IOC.tdefs     = tdefs
                                   , IOC.sigs      = sigs
                                   , IOC.modeldef  = moddef
                                   , IOC.mapperdef = mapdef
                                   , IOC.puttow    = putToW
                                   , IOC.getfrow   = getFroW
                                   , IOC.behtrie   = []
                                   , IOC.inistate  = 0
                                   , IOC.curstate  = 0
                                   , IOC.modsts    = []
                                   , IOC.mapsts    = []
                                   , IOC.putmsgs   = putmsgs
                                   }
            (maybt,mt) <- startSimulator moddef mapdef
            case maybt of
              Nothing ->
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Simulator start failed" ]
              Just bt -> do
                   IOC.modifyCS $
                     \st -> st { IOC.modsts = bt
                               , IOC.mapsts = mt
                               }
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Simulator started" ]
       _ -> do                                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
            TxsCore.txsStop
            TxsCore.txsSetSim putToW getFroW moddef mapdef

startSimulator :: TxsDefs.ModelDef ->
                  Maybe TxsDefs.MapperDef ->
                  IOC.IOC ( Maybe BTree.BTree, BTree.BTree )

startSimulator (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
               Nothing  =
     let allSyncs = minsyncs ++ moutsyncs ++ msplsyncs
     in do
       envb            <- filterEnvCtoEnvB
       (maybt', envb') <- lift $ runStateT (Behave.behInit allSyncs mbexp) envb
       writeEnvBtoEnvC envb'
       return ( maybt', [] )

startSimulator (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
               (Just (TxsDefs.MapperDef achins achouts asyncsets abexp))  =
     let { mins  = Set.fromList minsyncs
         ; mouts = Set.fromList moutsyncs
         ; ains  = Set.fromList $ filter (not . Set.null)
                       [ sync `Set.intersection` Set.fromList achins  | sync <- asyncsets ]
         ; aouts = Set.fromList $ filter (not . Set.null)
                       [ sync `Set.intersection` Set.fromList achouts | sync <- asyncsets ]
         }
      in if     mouts `Set.isSubsetOf` ains
             && mins  `Set.isSubsetOf` aouts
           then do let allSyncs = minsyncs ++ moutsyncs ++ msplsyncs
                   envb            <- filterEnvCtoEnvB
                   (maybt',envb' ) <- lift $ runStateT (Behave.behInit allSyncs  mbexp) envb
                   (maymt',envb'') <- lift $ runStateT (Behave.behInit asyncsets abexp) envb'
                   writeEnvBtoEnvC envb''
                   case (maybt',maymt') of
                     (Nothing , _       ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester model failed" ]
                          return ( Nothing, [] )
                     (_       , Nothing ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester mapper failed" ]
                          return ( Nothing, [] )
                     (Just _, Just mt') ->
                          return ( maybt', mt' )
           else do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Inconsistent definitions" ]
                   return ( Nothing, [] )

-- | Start stepping using the provided model definition.
--
--   Only possible when txscore is initialized.
--
-- modus transition general.
txsSetStep :: TxsDefs.ModelDef              -- ^ model definition.
           -> IOC.IOC ()
txsSetStep moddef  =  do
     envc <- get
     case IOC.state envc of
       IOC.Noning ->
            IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Stepper started without model file" ]
       IOC.Initing { IOC.smts = smts
                   , IOC.tdefs = tdefs
                   , IOC.sigs = sigs
                   , IOC.putmsgs = putmsgs } -> do
            IOC.putCS IOC.Stepping { IOC.smts      = smts
                                   , IOC.tdefs     = tdefs
                                   , IOC.sigs      = sigs
                                   , IOC.modeldef  = moddef
                                   , IOC.behtrie   = []
                                   , IOC.inistate  = 0
                                   , IOC.curstate  = 0
                                   , IOC.maxstate  = 0
                                   , IOC.modstss   = Map.empty
                                   , IOC.putmsgs   = putmsgs
                                   }
            maybt <- startStepper moddef
            case maybt of
              Nothing ->
                IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Stepper start failed" ]
              Just bt -> do
                   IOC.modifyCS $ \st -> st { IOC.modstss = Map.singleton 0 bt }
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
txsTestIn :: TxsDDefs.Action                    -- ^ input action to test SUT.
          -> IOC.IOC TxsDDefs.Verdict           -- ^ Verdict of test with provided input action.
txsTestIn act  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing { IOC.purpdef = Nothing }  -> do
         (_,verdict) <- Test.testIn act 1
         return verdict
       IOC.Testing {}                       -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "No test action with test purpose" ]
         return TxsDDefs.NoVerdict
       _                                    -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Tester mode" ]
         return TxsDDefs.NoVerdict

-- | Test SUT by observing output action.
-- core action.
--
-- Only possible in test modus (see 'txsSetTest').
-- Not possible with test purpose.
txsTestOut :: IOC.IOC TxsDDefs.Verdict
txsTestOut  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing { IOC.purpdef = Nothing }    -> do
         (_, verdict) <- Test.testOut 1
         return verdict
       IOC.Testing {}                       -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "No test output with test purpose" ]
         return TxsDDefs.NoVerdict
       _                                    -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Tester mode" ]
         return TxsDDefs.NoVerdict

-- | Test SUT with the provided number of actions.
-- core action.
--
-- Only possible in test modus (see 'txsSetTest').
txsTestN :: Int                         -- ^ number of actions to test SUT.
         -> IOC.IOC TxsDDefs.Verdict    -- ^ Verdict of test with provided number of actions.
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
txsSimN :: Int                      -- ^ number of actions to simulate model.
        -> IOC.IOC TxsDDefs.Verdict -- ^ Verdict of simulation with number of actions.
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
txsStepN :: Int                                 -- ^ number of actions to step model.
         -> IOC.IOC TxsDDefs.Verdict            -- ^ Verdict of stepping with provided number of actions.
txsStepN depth  =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping {} -> Step.stepN depth 1
       _ -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Stepper mode" ]
         return TxsDDefs.NoVerdict

-- | Step model with the provided action.
-- core action.
--
-- Only possible in stepper modus (see 'txsSetStep').
txsStepA :: TxsDDefs.Action                         -- ^ action to step in model.
         -> IOC.IOC TxsDDefs.Verdict                -- ^ Verdict of stepping with provided action.
txsStepA act =  do
     envc <- get
     case IOC.state envc of
       IOC.Stepping {} -> Step.stepA act
       _ -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Stepper mode" ]
         return  TxsDDefs.NoVerdict

-- | Show provided item.
txsShow :: String               -- ^ kind of item to be shown.
        -> String               -- ^ name of item to be shown.
                                --   Valid items are "tdefs", "state",
                                --   "model", "mapper", "purp", "modeldef" <name>,
                                --   "mapperdef" <name>, "purpdef" <name>
        -> IOC.IOC String
txsShow item name  = do
     envc  <- gets IOC.state
     let tdefs = IOC.tdefs envc
     case envc of
      IOC.Noning{ }
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Noning: nothing to be shown" ]
               return "\n"
      IOC.Initing{ }
         -> case (item,name) of
              ("tdefs"    ,"") -> return $ show (IOC.tdefs envc)
              ("modeldef" ,nm) -> return $ nm2string nm TxsDefs.IdModel TxsDefs.DefModel
                                                     (TxsDefs.modelDefs tdefs)
              ("mapperdef",nm) -> return $ nm2string nm TxsDefs.IdMapper TxsDefs.DefMapper
                                                     (TxsDefs.mapperDefs tdefs)
              ("purpdef"  ,nm) -> return $ nm2string nm TxsDefs.IdPurp TxsDefs.DefPurp
                                                     (TxsDefs.purpDefs tdefs)
              ("procdef"  ,nm) -> return $ nm2string nm TxsDefs.IdProc TxsDefs.DefProc
                                                     (TxsDefs.procDefs tdefs)
              _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "nothing to be shown 1" ]
                      return "\n"
      _ -> case (item,name) of
              ("tdefs"    ,"") -> return $ show (IOC.tdefs envc)
              ("state"    ,"") -> return $ show (IOC.curstate envc)
              ("model"    ,"") -> return $ TxsShow.fshow (IOC.modsts envc)
              ("mapper"   ,"") -> return $ TxsShow.fshow (IOC.mapsts envc)
              ("purp"     ,"") -> return $ TxsShow.fshow (IOC.purpsts envc)
              ("modeldef" ,nm) -> return $ nm2string nm TxsDefs.IdModel TxsDefs.DefModel
                                                     (TxsDefs.modelDefs tdefs)
              ("mapperdef",nm) -> return $ nm2string nm TxsDefs.IdMapper TxsDefs.DefMapper
                                                     (TxsDefs.mapperDefs tdefs)
              ("purpdef"  ,nm) -> return $ nm2string nm TxsDefs.IdPurp TxsDefs.DefPurp
                                                     (TxsDefs.purpDefs tdefs)
              ("procdef"  ,nm) -> return $ nm2string nm TxsDefs.IdProc TxsDefs.DefProc
                                                     (TxsDefs.procDefs tdefs)
              _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "nothing to be shown 2" ]
                      return "\n"

  where
     nm2string :: String
               -> (id -> TxsDefs.Ident)
               -> (def -> TxsDefs.TxsDef)
               -> Map.Map id def
               -> String
     nm2string nm id2ident id2def iddefs =
       let defs = [ (id2ident id', id2def def) | (id', def) <- Map.toList iddefs
                                              , TxsDefs.name (id2ident id') == T.pack nm ]
       in case defs of
            [(ident,txsdef)] -> TxsShow.fshow (ident,txsdef)
            _                -> "no (uniquely) defined item to be shown: " ++ nm ++ "\n"

-- | Go to state with the provided state number.
-- core action.
--
-- Only possible in stepper modus (see 'txsSetStep').
txsGoTo :: EnvData.StateNr              -- ^ state to go to.
        -> IOC.IOC ()
txsGoTo stateNr  =
  if  stateNr >= 0
  then do
    modStss <- gets (IOC.modstss . IOC.state)
    case Map.lookup stateNr modStss of
       Nothing -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "no such state" ]
       Just _ ->
         modify $
           \env ->
             env { IOC.state =
                     (IOC.state env)
                     { IOC.curstate = stateNr }
                 }
  else ltsBackN (-stateNr)
  where
     ltsBackN :: Int -> IOC.IOC ()
     ltsBackN backsteps
        | backsteps <= 0 = return ()
        | otherwise  = do    -- backsteps > 0
            st <- gets IOC.state
            let iniState = IOC.inistate st
                curState = IOC.curstate st
                behTrie = IOC.behtrie st
            case [ s | (s, _, s') <- behTrie, s' == curState ] of
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

-- | Return the menu, i.e., all possible actions.
txsMenu :: String                               -- ^ kind (valid values are "mod", "purp", or "map")
        -> String                               -- ^ what (valid values are "all", "in", "out", or a <goal name>)
        -> IOC.IOC BTree.Menu
txsMenu kind what  =  do
     envSt <- gets IOC.state
     case (kind,envSt) of
       ("mod",IOC.Testing {})  -> do
            menuIn   <- Ioco.iocoModelMenuIn
            menuOut  <- Ioco.iocoModelMenuOut
            case what of
              "all" -> return $ menuIn ++ menuOut
              "in"  -> return menuIn
              "out" -> return menuOut
              _     -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "error in menu" ]
                          return []
       ("mod",IOC.Simuling {}) -> do
            menuIn   <- Ioco.iocoModelMenuIn
            menuOut  <- Ioco.iocoModelMenuOut
            case what of
              "all" -> return $ menuIn ++ menuOut
              "in"  -> return menuIn
              "out" -> return menuOut
              _     -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "error in menu" ]
                          return []
       ("mod",IOC.Stepping {}) -> do
            menuIn  <- Step.stepModelMenuIn
            menuOut <- Step.stepModelMenuOut
            case what of
              "all" -> return $ menuIn ++ menuOut
              "in"  -> return menuIn
              "out" -> return menuOut
              _     -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "error in menu" ]
                          return []
       ("map",IOC.Testing {})  -> Mapper.mapperMenu
       ("map",IOC.Simuling {}) -> Mapper.mapperMenu
       ("purp",IOC.Testing {}) -> Purpose.goalMenu what
       _ -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "error in menu" ]
               return []

-- | Give the provided action to the mapper.
--
-- Not possible in stepper modus (see 'txsSetStep').
txsMapper :: TxsDDefs.Action                    -- ^ Action to be provided to mapper.
          -> IOC.IOC TxsDDefs.Action
txsMapper act  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing {} -> mapperMap act
       IOC.Simuling {} -> mapperMap act
       _ -> do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                        "Mapping only allowed in Testing or Simulating mode" ]
         return act


-- | NComplete derivation by Petra van den Bos.
txsNComp :: TxsDefs.ModelDef                   -- ^ model. Currently only
                                               -- `StautDef` without data is
                                               -- supported.
         -> IOC.IOC (Maybe TxsDefs.PurpId)     -- ^ Derived purpose, when
                                               -- succesful.
txsNComp (TxsDefs.ModelDef insyncs outsyncs splsyncs bexp) =  do
  envc <- get
  case (IOC.state envc, bexp) of
    ( IOC.Initing {IOC.tdefs = tdefs}
      , TxsDefs.ProcInst procid@(TxsDefs.ProcId pnm _ _ _ _) chans []
      ) | and [ Set.size sync == 1 | sync <- insyncs ++ outsyncs ]
          && and [ null srts
                 | TxsDefs.ChanId _ _ srts <- Set.toList $ Set.unions $ insyncs ++ outsyncs
                 ]
          && null splsyncs
       -> case Map.lookup procid (TxsDefs.procDefs tdefs) of
              Just (TxsDefs.ProcDef chids [] staut@(TxsDefs.StAut _ ve _)) | Map.null ve
                 -> do let chanmap                       = Map.fromList (zip chids chans)
                           TxsDefs.StAut statid _ trans = Expand.relabel chanmap staut
                       maypurp <- NComp.nComplete insyncs outsyncs statid trans
                       case maypurp of
                         Just purpdef -> do
                           unid <- gets IOC.unid
                           let purpid = TxsDefs.PurpId ("PURP_"<>pnm) (unid+1)
                               tdefs' = tdefs
                                 { TxsDefs.purpDefs = Map.insert
                                                      purpid purpdef (TxsDefs.purpDefs tdefs)
                                 }
                           IOC.incUnid
                           IOC.modifyCS $ \st -> st { IOC.tdefs = tdefs' }
                           return $ Just purpid
                         _ -> return Nothing

              _ -> do
                IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                "N-Complete requires a data-less STAUTDEF" ]
                return Nothing
    _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                          $ "N-Complete should be used after initialization, before testing, "
                            ++ "with a STAUTDEF with data-less, singleton channels" ]
            return Nothing

-- ----------------------------------------------------------------------------------------- --

-- | LPE transformation by Carsten Ruetz
txsLPE :: TxsDefs.BExpr                     -- ^ behaviour expression, to be transformed;
                                            --   shall be a process instantiation.
       -> IOC.IOC (Maybe TxsDefs.BExpr)     -- ^ transformed process instantiation
txsLPE bexpr  =  do
  envc <- get
  case (IOC.state envc, bexpr) of
    (IOC.Initing {IOC.tdefs = tdefs}, TxsDefs.ProcInst procid _ _)
      -> case Map.lookup procid (TxsDefs.procDefs tdefs) of
           Just TxsDefs.ProcDef{}
             -> case lpeTransform bexpr (TxsDefs.procDefs tdefs) of
                  Just (procinst'@(TxsDefs.ProcInst procid' _ _), procdef')
                    -> case Map.lookup procid' (TxsDefs.procDefs tdefs) of
                         Nothing
                           -> do let tdefs' = tdefs
                                       { TxsDefs.procDefs = Map.insert procid' procdef'
                                                                       (TxsDefs.procDefs tdefs)
                                       }
                                 IOC.modifyCS $ \st -> st { IOC.tdefs = tdefs' }
                                 return $ Just procinst'
                         _ -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                               "LPE: generated process id already exists" ]
                                 return Nothing

                  _ -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                        "LPE: transformation failed" ]
                          return Nothing
           _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                 "LPE: process in process instantiation not defined" ]
                   return Nothing
    _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                          "LPE: only allowed when initialized and with process instantiation" ]
            return Nothing

-- ----------------------------------------------------------------------------------------- --
-- temp function for lpe                                                                     --

-- | placeholder for LPE tranformation
lpeTransform :: TxsDefs.BExpr              -- ^ behaviour expression to be transformed,
                                           --   assumed to be a process instantiation
             -> Map.Map TxsDefs.ProcId TxsDefs.ProcDef  -- ^ context of process definitions
                                                        --   in which process instantiation
                                                        --   is assumed to be defined
             -> Maybe (TxsDefs.BExpr, TxsDefs.ProcDef)  -- ^ transformed process instantiation
                                                        --   with its LPE definition
lpeTransform procinst procdefs
  =  case procinst of
       TxsDefs.ProcInst procid@(TxsDefs.ProcId nm uid chids vars ext) chans vexps
         -> case Map.lookup procid procdefs of
              Just procdef -> Just ( TxsDefs.ProcInst
                                       (TxsDefs.ProcId ("LPE_"<>nm) uid chids vars ext)
                                       chans
                                       vexps
                                   , procdef
                                   )
              __           -> error "LPE transformation: undefined process instantiation\n"
       _ -> error "LPE transformation: only defined for process instantiation\n"


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

