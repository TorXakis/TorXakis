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
( -- * run torxakis core
  runTxsCore
  
  -- * initialize torxakis core
, txsInit

  -- * terminate torxakis core
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

  -- ** stop testing/simulating/stepping
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

-- import from behave(defs)
import qualified BTree       as BTree
import qualified Behave      as Behave

-- import from behaveenv
import qualified EnvBTree    as EnvBTree

-- import from coreenv
import qualified EnvCore     as IOC
import qualified EnvData     as EnvData
import qualified ParamCore   as ParamCore

-- import from defs
import qualified TxsDefs
import qualified SortOf
import qualified TxsDDefs
import qualified TxsShow
import qualified TxsUtils
import qualified StdTDefs
import qualified Sigs

-- import from solve
import qualified FreeVar     as FreeVar
import qualified SMT         as SMT
import qualified Solve       as Solve
import qualified SolveDefs   as SolveDefs
import qualified SolveDefs.Params as SolveDefs.Params
-- import from value
import qualified Eval        as Eval


-- | torxakis core main api -- start
runTxsCore :: StateT s IOC.IOC a -> s -> IO ()
runTxsCore ctrl s0  =  do
     runStateT (runTxsCtrl ctrl s0)
               (IOC.Noning { IOC.params = Map.union ParamCore.initParams
                                                    SolveDefs.Params.initParams
                           , IOC.unid   = 0
                           }
               )
     return ()

runTxsCtrl :: StateT s IOC.IOC a -> s -> IOC.IOC ()
runTxsCtrl ctrl s0  =  do 
     runStateT ctrl s0
     return ()


-- | torxakis core main api -- modus transition general
txsInit :: TxsDefs.TxsDefs -> Sigs.Sigs TxsDefs.VarId -> ([EnvData.Msg] -> IOC.IOC ()) -> IOC.IOC ()
txsInit tdefs sigs putMsgs  =  do
     envc <- get
     case envc of
       IOC.Noning params unid
         -> do smtEnv         <- lift $ SMT.createSMTEnv SMT.cmdZ3 False tdefs
               (info,smtEnv') <- lift $ runStateT SMT.openSolver smtEnv
               (_,smtEnv'')   <- lift $ runStateT (SMT.addDefinitions tdefs) smtEnv'
               putMsgs [ EnvData.TXS_CORE_USER_INFO $ "Solver initialized : " ++ info
                       , EnvData.TXS_CORE_USER_INFO $ "TxsCore initialized"
                       ]
               put $ IOC.Initing { IOC.smts    = Map.singleton "current" smtEnv''
                                 , IOC.tdefs   = tdefs
                                 , IOC.sigs    = sigs
                                 , IOC.params  = params
                                 , IOC.unid    = unid
                                 , IOC.putmsgs = putMsgs
                                 }
       IOC.Initing { }
         -> do TxsCore.txsTermit
               TxsCore.txsInit tdefs sigs putMsgs
       _ -> do TxsCore.txsStop                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
               TxsCore.txsTermit
               TxsCore.txsInit tdefs sigs putMsgs
                    
-- | terminate torxakis core
txsTermit :: IOC.IOC ()
txsTermit  =  do
     envc <- get
     case envc of
       IOC.Noning { }
         -> return ()
       IOC.Initing { IOC.smts = smts , IOC.params = params , IOC.unid = unid, IOC.putmsgs = putmsgs }
         -> do lift $ mapM_ (runStateT SMT.close) (Map.elems smts)
               putmsgs [ EnvData.TXS_CORE_USER_INFO "Solver(s) closed"
                       , EnvData.TXS_CORE_USER_INFO "TxsCore terminated"
                       ]
               put $ IOC.Noning { IOC.params = params
                                , IOC.unid   = unid
                                }
       _ -> do TxsCore.txsStop                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
               TxsCore.txsTermit

-- | stop testing/simulating/stepping
txsStop :: IOC.IOC ()
txsStop  =  do
     envc <- get
     case envc of
       IOC.Noning params unid
         -> return ()
       IOC.Initing smts tdefs sigs params unid putmsgs
         -> return ()
       _ -> do                                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Simulation/Testing/Stepping stopped" ]
               put $ IOC.Initing { IOC.smts    = IOC.smts    envc
                                 , IOC.tdefs   = IOC.tdefs   envc
                                 , IOC.sigs    = IOC.sigs    envc
                                 , IOC.params  = IOC.params  envc
                                 , IOC.unid    = IOC.unid    envc
                                 , IOC.putmsgs = IOC.putmsgs envc
                                 }


-- | Get the values of all parameters.
txsGetParams :: IOC.IOC [(String,String)]
txsGetParams  =  do
     IOC.getParams []

-- | Get the value of the provided parameter.
txsGetParam :: String -> IOC.IOC [(String,String)]
txsGetParam prm  =  do
     IOC.getParams [prm]

-- | Set the provided parameter to the provided value.
txsSetParam :: String -> String -> IOC.IOC [(String,String)]
txsSetParam prm val  =  do
     IOC.setParams [(prm,val)]

-- | Set the random seed to the provided value.
txsSetSeed :: Int -> IOC.IOC ()
txsSetSeed seed  =  do
     lift $ setStdGen(mkStdGen seed)
     IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_INFO $ "Seed set to " ++ (show seed) ]

-- | Evaluate the provided value expression.
--   Only possible when txscore is initialized with a model.
txsEval :: TxsDefs.VExpr -> IOC.IOC TxsDefs.Const
txsEval vexp  =  do
     envc <- get
     case envc of
       IOC.Noning params unid
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR $ "No 'eval' without model" ]
               return $ TxsDefs.Cerror ""
       _ -> do                       -- IOC.Initing, IOC.Testing, IOC.Simuling, IOC.Stepping --
               let frees = FreeVar.freeVars vexp
                in if  not $ null frees
                     then do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                           $ "Value expression not closed: " ++
                                             (TxsShow.fshow frees) ]
                             return $ TxsDefs.Cerror ""
                     else do envb         <- filterEnvCtoEnvB
                             (wal',envb') <- lift $ runStateT (Eval.eval vexp) envb
                             writeEnvBtoEnvC envb'
                             return $ wal'

-- | Find a solution for the provided Boolean value expression.
--   Only possible when txscore is initialized with a model.
txsSolve :: TxsDefs.VExpr -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
txsSolve vexp  =  do
     envc <- get
     case envc of
       IOC.Noning params unid
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR $ "No 'solve' without model" ]
               return Map.empty
       _ -> do                       -- IOC.Initing, IOC.Testing, IOC.Simuling, IOC.Stepping --
               if  SortOf.sortOf vexp /= SortOf.sortId_Bool
                 then do
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                 $ "Value expression for solve shall be Bool" ]
                   return Map.empty
                 else do
                   frees         <- return $ FreeVar.freeVars vexp
                   assertions    <- return $ Solve.add vexp Solve.empty
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
--   Only possible when txscore is initialized with a model.
txsUniSolve :: TxsDefs.VExpr -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
txsUniSolve vexp  =  do
     envc <- get
     case envc of
       IOC.Noning params unid
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR $ "No 'solve' without model" ]
               return Map.empty
       _ -> do                       -- IOC.Initing, IOC.Testing, IOC.Simuling, IOC.Stepping --
               if  SortOf.sortOf vexp /= SortOf.sortId_Bool
                 then do
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Value expression shall be Bool" ]
                   return Map.empty
                 else do
                   frees         <- return $ FreeVar.freeVars vexp
                   assertions    <- return $ Solve.add vexp Solve.empty
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
--   Only possible when txscore is initialized with a model.
txsRanSolve :: TxsDefs.VExpr -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
txsRanSolve vexp  =  do
     envc <- get
     case envc of
       IOC.Noning params unid
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR $ "No 'solve' without model" ]
               return Map.empty
       _ -> do                       -- IOC.Initing, IOC.Testing, IOC.Simuling, IOC.Stepping --
               if  SortOf.sortOf vexp /= SortOf.sortId_Bool
                 then do
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Value expression shall be Bool" ]
                   return Map.empty
                else do
                   frees         <- return $ FreeVar.freeVars vexp
                   assertions    <- return $ Solve.add vexp Solve.empty
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
--   * callback function for sending an input action to the SUT (world),
--   * callback function for receiving an output action from the SUT (world),
--   * model definition,
--   * optional mapper definition, and
--   * optional test purpose definition.
--   Only possible when txscore is initialized with a model.
--
-- modus transition general.
txsSetTest :: (TxsDDefs.Action -> IOC.IOC TxsDDefs.Action) ->
              IOC.IOC TxsDDefs.Action ->
              TxsDefs.ModelDef -> Maybe TxsDefs.MapperDef -> Maybe TxsDefs.PurpDef ->
              IOC.IOC ()
txsSetTest putToW getFroW moddef mapdef purpdef  =  do
     envc <- get
     case envc of
     { IOC.Noning params unid -> do
            IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester started without model file" ]
     ; IOC.Initing smts tdefs sigs params unid putmsgs -> do
            (maybt,mt,gls) <- startTester moddef mapdef purpdef
            case maybt of
            { Nothing -> do
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester start failed" ]
            ; Just bt -> do
                   put $ IOC.Testing { IOC.smts      = smts
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
                                     , IOC.params    = params
                                     , IOC.unid      = unid
                                     , IOC.putmsgs   = putmsgs
                                     }
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Tester started" ]
            }
     ; _ -> do                                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
            TxsCore.txsStop
            TxsCore.txsSetTest putToW getFroW moddef mapdef purpdef
     }


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
       return $ ( maybt', [], [] )

startTester (TxsDefs.ModelDef  minsyncs moutsyncs msplsyncs mbexp)
                 (Just (TxsDefs.MapperDef achins achouts asyncsets abexp))
                 Nothing =
     let { mins   = Set.unions minsyncs
         ; mouts  = Set.unions moutsyncs
         ; ains   = Set.fromList $ achins
         ; aouts  = Set.fromList $ achouts
         }
      in if     mins  `Set.isSubsetOf` ains
             && mouts `Set.isSubsetOf` aouts
           then do let allSyncs = minsyncs ++ moutsyncs ++ msplsyncs
                   envb            <- filterEnvCtoEnvB
                   (maybt',envb' ) <- lift $ runStateT (Behave.behInit allSyncs  mbexp) envb
                   (maymt',envb'') <- lift $ runStateT (Behave.behInit asyncsets abexp) envb'
                   case (maybt',maymt') of
                   { (Nothing , _       ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester model failed" ]
                          return $ ( Nothing, [], [] )
                   ; (_       , Nothing ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester mapper failed" ]
                          return $ ( Nothing, [], [] )
                   ; (Just bt', Just mt') -> do
                          writeEnvBtoEnvC envb''
                          return $ ( maybt', mt', [] )
                   }
           else do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Inconsistent definitions" ]
                   return $ ( Nothing, [], [] )

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
                   { Nothing -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester model failed" ]
                          return $ ( Nothing, [], [] )
                   ; Just bt' -> do
                          writeEnvBtoEnvC envb'
                          gls <- mapM (goalInit pAllSyncs) goals
                          return $ ( maybt', [], concat gls )
                   }
           else do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Inconsistent definitions" ]
                   return $ ( Nothing, [], [] )

startTester (TxsDefs.ModelDef  minsyncs moutsyncs msplsyncs mbexp)
                 (Just (TxsDefs.MapperDef achins achouts asyncsets abexp))
                 (Just (TxsDefs.PurpDef pinsyncs poutsyncs psplsyncs goals)) =
     let { mins   = Set.unions minsyncs
         ; mouts  = Set.unions moutsyncs
         ; ains   = Set.fromList $ achins
         ; aouts  = Set.fromList $ achouts
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
                   { (Nothing , _       ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester model failed" ]
                          return $ ( Nothing, [], [] )
                   ; (_       , Nothing ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester mapper failed" ]
                          return $ ( Nothing, [], [] )
                   ; (Just bt', Just mt') -> do
                          writeEnvBtoEnvC envb''
                          gls <- mapM (goalInit pAllSyncs) goals
                          return $ ( maybt', mt', concat gls )
                   }
           else do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Inconsistent definitions" ]
                   return $ ( Nothing, [], [] )

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
--   * callback function for sending an output action to the environment (world),
--   * callback function for receiving an input action from the environment (world),
--   * model definition, and
--   * optional mapper definition.
--   Only possible when txscore is initialized with a model.
--
-- modus transition general.
txsSetSim :: (TxsDDefs.Action -> IOC.IOC TxsDDefs.Action) ->
             IOC.IOC TxsDDefs.Action ->
             TxsDefs.ModelDef -> Maybe TxsDefs.MapperDef ->
             IOC.IOC ()
txsSetSim putToW getFroW moddef mapdef  =  do
     envc <- get
     case envc of
     { IOC.Noning params unid -> do
            IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Simulator started without model file" ]
     ; IOC.Initing smts tdefs sigs params unid putmsgs -> do
            (maybt,mt) <- startSimulator moddef mapdef
            case maybt of
            { Nothing -> do
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Simulator start failed" ]
            ; Just bt -> do
                   put $ IOC.Simuling { IOC.smts      = smts
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
                                      , IOC.params    = params
                                      , IOC.unid      = unid
                                      , IOC.putmsgs   = putmsgs
                                      }
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Simulator started" ]
            }
     ; _ -> do                                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
            TxsCore.txsStop
            TxsCore.txsSetSim putToW getFroW moddef mapdef
     }


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
       return $ ( maybt', [] )

startSimulator (TxsDefs.ModelDef  minsyncs moutsyncs msplsyncs mbexp)
                (Just (TxsDefs.MapperDef achins achouts asyncsets abexp))
  =  let { mins   = Set.unions minsyncs
         ; mouts  = Set.unions moutsyncs
         ; ains   = Set.fromList $ achins
         ; aouts  = Set.fromList $ achouts
         }
      in if     mouts `Set.isSubsetOf` ains
             && mins  `Set.isSubsetOf` aouts
           then do let allSyncs = minsyncs ++ moutsyncs ++ msplsyncs
                   envb            <- filterEnvCtoEnvB
                   (maybt',envb' ) <- lift $ runStateT (Behave.behInit allSyncs  mbexp) envb
                   (maymt',envb'') <- lift $ runStateT (Behave.behInit asyncsets abexp) envb'
                   case (maybt',maymt') of
                   { (Nothing , _       ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester model failed" ]
                          return $ ( Nothing, [] )
                   ; (_       , Nothing ) -> do
                          IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester mapper failed" ]
                          return $ ( Nothing, [] )
                   ; (Just bt', Just mt') -> do
                          writeEnvBtoEnvC envb''
                          return $ ( maybt', mt' )
                   }
           else do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Inconsistent definitions" ]
                   return $ ( Nothing, [] )

-- | Start stepping using the provided model definition.
--   Only possible when txscore is initialized with a model.
--
-- modus transition general.
txsSetStep :: TxsDefs.ModelDef -> IOC.IOC ()
txsSetStep moddef  =  do
     envc <- get
     case envc of
     { IOC.Noning params unid -> do
            IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Stepper started without model file" ]
     ; IOC.Initing { .. } -> do
            maybt <- startStepper moddef
            case maybt of
            { Nothing -> do
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Stepper start failed" ]
            ; Just bt -> do
                   put $ IOC.Stepping { IOC.smts      = smts
                                      , IOC.tdefs     = tdefs
                                      , IOC.sigs      = sigs
                                      , IOC.modeldef  = moddef
                                      , IOC.behtrie   = []
                                      , IOC.inistate  = 0
                                      , IOC.curstate  = 0
                                      , IOC.maxstate  = 0
                                      , IOC.modstss   = Map.singleton 0 bt
                                      , IOC.params    = params
                                      , IOC.unid      = unid
                                      , IOC.putmsgs   = putmsgs
                                      }
                   IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Stepper started" ]
            }
     ; _ -> do                                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
            TxsCore.txsStop
            TxsCore.txsSetStep moddef
     }


startStepper :: TxsDefs.ModelDef ->
                IOC.IOC ( Maybe BTree.BTree )

startStepper (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)  =  do
     let allSyncs = minsyncs ++ moutsyncs ++ msplsyncs
     envb            <- filterEnvCtoEnvB
     (maybt', envb') <- lift $ runStateT (Behave.behInit allSyncs mbexp) envb
     writeEnvBtoEnvC envb'
     return $ maybt'

-- | Test SUT with the provided input action.
-- core action.
-- Only possible in test modus (see @txsSetTest).
-- Not possible with test purpose.
txsTestIn :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Verdict
txsTestIn act  =  do
     envc <- get
     case envc of
     { IOC.Testing _ _ _ modeldef mapperdef Nothing _ _ _ _ _ _ _ _ _ _ _
         -> do Test.testIn act 1
     ; IOC.Testing _ _ _ modeldef mapperdef purpdef       _ _ _ _ _ _ _ _ _ _ _
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "No test action with test purpose" ]
               return $ TxsDDefs.NoVerdict
     ; _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Tester mode" ]
               return $ TxsDDefs.NoVerdict
     }


-- | Test SUT by observing output action.
-- core action.
-- Only possible in test modus (see @txsSetTest).
-- Not possible with test purpose.
txsTestOut :: IOC.IOC TxsDDefs.Verdict
txsTestOut  =  do
     envc <- get
     case envc of
     { IOC.Testing _ _ _ modeldef mapperdef Nothing _ _ _ _ _ _ _ _ _ _ _
         -> do Test.testOut 1
     ; IOC.Testing _ _ _ modeldef mapperdef purpdef       _ _ _ _ _ _ _ _ _ _ _
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "No test output with test purpose" ]
               return $ TxsDDefs.NoVerdict
     ; _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Tester mode" ]
               return $ TxsDDefs.NoVerdict
     }


-- | Test SUT with the provided number of actions.
-- core action.
-- Only possible in test modus (see @txsSetTest).
txsTestN :: Int -> IOC.IOC TxsDDefs.Verdict
txsTestN depth  =  do  
     envc <- get
     case envc of
     { IOC.Testing _ _ _ modeldef mapperdef purpdef _ _ _ _ _ _ _ _ _ _ _
         -> do Test.testN depth 1
     ; _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Tester mode" ]
               return $ TxsDDefs.NoVerdict
     }



-- | Simulate model with the provided number of actions.
-- core action.
-- Only possible in simulation modus (see @txsSetSim).
txsSimN :: Int -> IOC.IOC TxsDDefs.Verdict
txsSimN depth  =  do
     envc <- get
     case envc of
     { IOC.Simuling _ _ _ modeldef mapperdef _ _ _ _ _ _ _ _ _ _
         -> do Sim.simN depth 1
     ; _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Simulator mode" ]
               return $ TxsDDefs.NoVerdict
     }


-- | Step model with the provided number of actions.
-- core action.
-- Only possible in stepper modus (see @txsSetStep).
txsStepN :: Int -> IOC.IOC TxsDDefs.Verdict
txsStepN depth  =  do
     envc <- get
     case envc of
     { IOC.Stepping _ _ _ modeldef _ _ _ _ _ _ _ _
         -> do Step.stepN depth 1
     ; _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Stepper mode" ]
               return $ TxsDDefs.NoVerdict
     }


-- | Step model with the provided action.
-- core action.
-- Only possible in stepper modus (see @txsSetStep).
txsStepA :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Verdict
txsStepA act  =  do
     envc <- get
     case envc of
     { IOC.Stepping _ _ _ modeldef _ _ _ _ _ _ _ _
         -> do Step.stepA act
     ; _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Stepper mode" ]
               return $ TxsDDefs.NoVerdict
     }


-- ----------------------------------------------------------------------------------------- --

-- | Show provided item.
--   Valid items are 
--       "tdefs"  
--       "state" 
--       "model" 
--       "mapper"
--       "purp"  
txsShow :: String -> IOC.IOC String
txsShow item  =  do
     envc <- get
     case item of
     { "tdefs"  -> do return $ show (IOC.tdefs envc)
     ; "state"  -> do return $ show (IOC.curstate envc)
     ; "model"  -> do return $ TxsShow.fshow (IOC.modsts envc)
     ; "mapper" -> do return $ TxsShow.fshow (IOC.mapsts envc)
     ; "purp"   -> do return $ TxsShow.fshow (IOC.purpsts envc)
     ; _        -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR $ "nothing to be shown" ]
                      return $ "\n"
     }
  

-- | Go to state with the provided state number.
-- core action.
-- Only possible in stepper modus (see @txsSetStep).
txsGoTo :: EnvData.StateNr -> IOC.IOC ()
txsGoTo stateNr  =  do
     if  stateNr >= 0
       then do modStss <- gets IOC.modstss
               case Map.lookup stateNr modStss of
               { Nothing -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR $ "no such state" ]
               ; Just bt -> do modify $ \env -> env { IOC.curstate = stateNr }
               }
       else do ltsBackN (-stateNr)

  where
     ltsBackN :: Int -> IOC.IOC ()
     ltsBackN backsteps
        | backsteps <= 0  =  do
            return $ ()
        | backsteps > 0   =  do
            iniState <- gets IOC.inistate
            curState <- gets IOC.curstate
            behTrie  <- gets IOC.behtrie
            case [ s | (s,a,s') <- behTrie, s' == curState ] of
            { [prev] -> do modify $ \env -> env { IOC.curstate = prev }
                           if  prev == iniState
                             then do return $ ()
                             else do ltsBackN (backsteps-1)
            ; _      -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $ "LtsBack error" ]
                           return $ ()
            }


-- | Provide the path.
txsPath :: IOC.IOC [(EnvData.StateNr, TxsDDefs.Action, EnvData.StateNr)]
txsPath  =  do
     iniState <- gets IOC.inistate
     curState <- gets IOC.curstate
     path iniState curState

  where

     path :: EnvData.StateNr -> EnvData.StateNr ->
             IOC.IOC [(EnvData.StateNr, TxsDDefs.Action, EnvData.StateNr)]
     path from to
        | from >= to  =  do
            return $ []
        | from < to   =  do
            iniState <- gets IOC.inistate
            behTrie  <- gets IOC.behtrie
            case [ (s1,a,s2) | (s1,a,s2) <- behTrie, s2 == to ] of
            { [(s1,a,s2)] -> do if (s1 == from) || (s1 == iniState)
                                  then do return $ [(s1,a,s2)]
                                  else do pp <- path from s1
                                          return $ pp ++ [(s1,a,s2)]
            ; _           -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $ "Path error" ]
                                return $ []
            }

-- | Returns the menu for the provided
--   * kind ("mod" or "purp")
--   * what ("all", "in", or "out"), and
--   * state number.
txsMenu :: String -> String -> EnvData.StateNr -> IOC.IOC BTree.Menu
txsMenu kind what stnr  =  do
     curState <- gets IOC.curstate
     stateNr  <- return $ if stnr == (-1) then curState else stnr
     case kind of
     { "mod"  -> do txsGoTo stateNr
                    menuIn   <- Ioco.iocoModelMenuIn
                    menuOut  <- Ioco.iocoModelMenuOut
                    txsGoTo curState
                    case what of
                    { "all" -> return $ menuIn ++ menuOut
                    ; "in"  -> return $ menuIn
                    ; "out" -> return $ menuOut
                    ; _     -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                $ "error in menu" ]
                                  return $ []
                    }
     ; "purp" -> do txsGoTo stateNr
                    gmenu <- Purpose.goalMenu what
                    txsGoTo curState
                    return $ gmenu
     ; _      -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $ "error in menu" ]
                    return $ []
     }

-- | Give the provided action to the mapper.
-- Not possible in stepper modus (see @txsSetStep).
txsMapper :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Action
txsMapper act  =  do
     envc <- get
     case envc of
     { IOC.Testing{ }
         -> do mapperMap act
     ; IOC.Simuling { }
         -> do mapperMap act
     ; _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                             $ "Mapping only allowed in Testing or Simulating mode" ]
               return $ act
     }

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

