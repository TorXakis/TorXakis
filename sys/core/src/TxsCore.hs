{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module TxsCore

-- ----------------------------------------------------------------------------------------- --
--
--   Core Module TorXakis API
--
-- ----------------------------------------------------------------------------------------- --
-- export

( runTxsCore      -- :: StateT s IOC.IOC a -> s -> IO ()
                  --    torxakis core main api start
, txsInit         -- :: TxsDefs.TxsDefs -> ([EnvData.Msg] -> IOC.IOC ()) -> IOC.IOC ()
                  --    initialize torxakis core
, txsTermit       -- :: IOC.IOC ()
                  --    terminate torxakis core
, txsStop         -- :: IOC.IOC ()
                  --    stop executing testing/simulating/stepping
, txsGetParams    -- :: IOC.IOC [(String,String)]
                  --    get all parameter values
, txsGetParam     -- :: String -> IOC.IOC [(String,String)]
                  --    get parameter value
, txsSetParam     -- :: String -> String -> IOC.IOC [(String,String)]
                  --    set parameter values
, txsSeed         -- :: Int -> IOC.IOC ()
                  --    setting random seed
, txsEval         -- :: TxsDefs.VExpr -> IOC.IOC TxsDefs.Const
                  --    evaluation of ValExpr
, txsSolve        -- :: TxsDefs.VExpr -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
                  --    solving Bool VExpr
, txsUniSolve     -- :: TxsDefs.VExpr -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
                  --    solving Bool VExpr uniquely
, txsRanSolve     -- :: TxsDefs.VExpr -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
                  --    solving Bool VExpr randomly
, txsSetTest      -- :: (TxsDDefs.Action -> IOC.IOC TxsDDefs.Action) ->
                  --    IOC.IOC TxsDDefs.Action ->
                  --    TxsDefs.ModelDef -> Maybe TxsDefs.MapperDef -> Maybe TxsDefs.PurpDef ->
                  --    IOC.IOC ()
, txsSetSim       -- :: (TxsDDefs.Action -> IOC.IOC TxsDDefs.Action) ->
                  --    IOC.IOC TxsDDefs.Action ->
                  --    TxsDefs.ModelDef -> Maybe TxsDefs.MapperDef ->
                  --    IOC.IOC ()
, txsSetStep      -- :: TxsDefs.ModelDef -> IOC.IOC ()
, txsTestIn       -- :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Verdict
, txsTestOut      -- :: IOC.IOC TxsDDefs.Verdict
, txsTestN        -- :: Int -> IOC.IOC TxsDDefs.Verdict
, txsSimN         -- :: Int -> IOC.IOC TxsDDefs.Verdict
, txsStepN        -- :: Int -> IOC.IOC TxsDDefs.Verdict
, txsStepA        -- :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Verdict
, txsShow         -- :: String -> IOC.IOC String
, txsGoTo         -- :: EnvData.StateNr -> IOC.IOC ()
, txsPath         -- :: IOC.IOC [(EnvData.StateNr, TxsDDefs.Action, EnvData.StateNr)]
, txsMenu         -- :: String -> EnvData.StateNr -> IOC.IOC Menu
, txsMapper       -- :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Action
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

-- import from behavedefs
import qualified BTree       as BTree

-- import from behaveenv
import qualified EnvBTree    as EnvBTree

-- import from coreenv
import qualified EnvCore     as IOC
import qualified EnvData     as EnvData
import qualified ParamCore   as ParamCore

-- import from defs
import qualified TxsDefs     as TxsDefs
import qualified SortOf      as SortOf
import qualified TxsDDefs    as TxsDDefs
import qualified TxsShow     as TxsShow
import qualified TxsUtils    as TxsUtils
import qualified StdTDefs    as StdTDefs

-- import from solve
import qualified FreeVar     as FreeVar
import qualified SMT         as SMT
import qualified Solve       as Solve
import qualified SolveDefs   as SolveDefs
import qualified SolveDefs.Params as SolveDefs.Params
-- import from value
import qualified Eval        as Eval


-- ----------------------------------------------------------------------------------------- --
-- torxakis core main api -- start


runTxsCore :: StateT s IOC.IOC a -> s -> IO ()
runTxsCore ctrl s0  =  do
     runStateT (runTxsCtrl ctrl s0) (IOC.Noning { IOC.params = Map.union ParamCore.initParams SolveDefs.Params.initParams
                                                , IOC.unid   = 0
                                                }
                                    )
     return ()

runTxsCtrl :: StateT s IOC.IOC a -> s -> IOC.IOC ()
runTxsCtrl ctrl s0  =  do 
     runStateT ctrl s0
     return ()


-- ----------------------------------------------------------------------------------------- --
-- torxakis core main api -- modus transition general


txsInit :: TxsDefs.TxsDefs -> ([EnvData.Msg] -> IOC.IOC ()) -> IOC.IOC ()
txsInit tdefs putMsgs  =  do
     envc <- get
     case envc of
       IOC.Noning params unid
         -> do smtEnv         <- lift $ SMT.createSMTEnv SMT.cmdZ3 True tdefs
               (info,smtEnv') <- lift $ runStateT SMT.openSolver smtEnv
               (_,smtEnv'')   <- lift $ runStateT (SMT.addDefinitions tdefs) smtEnv'
               putMsgs [ EnvData.TXS_CORE_USER_INFO $ "Solver initialized : " ++ info
                       , EnvData.TXS_CORE_USER_INFO $ "TxsCore initialized"
                       ]
               put $ IOC.Initing { IOC.smts    = Map.singleton "current" smtEnv''
                                 , IOC.tdefs   = tdefs
                                 , IOC.params  = params
                                 , IOC.unid    = unid
                                 , IOC.putmsgs = putMsgs
                                 }
       IOC.Initing _ _ _ _ _
         -> do TxsCore.txsTermit
               TxsCore.txsInit tdefs putMsgs
       _ -> do TxsCore.txsStop                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
               TxsCore.txsTermit
               TxsCore.txsInit tdefs putMsgs
                    
-- ----------------------------------------------------------------------------------------- --

txsTermit :: IOC.IOC ()
txsTermit  =  do
     envc    <- get
     case envc of
       IOC.Noning params unid
         -> return ()
       IOC.Initing smts tdefs params unid putmsgs
         -> do lift $ mapM_ (runStateT SMT.close) (Map.elems smts)
               putmsgs [ EnvData.TXS_CORE_USER_INFO "Solver(s) closed"
                       , EnvData.TXS_CORE_USER_INFO "TxsCore terminated"
                       ]
               put $ IOC.Noning { IOC.params = params
                                , IOC.unid   = unid
                                }
       _ -> do TxsCore.txsStop                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
               TxsCore.txsTermit

-- ----------------------------------------------------------------------------------------- --

txsStop :: IOC.IOC ()
txsStop  =  do
     envc <- get
     case envc of
       IOC.Noning params unid
         -> return ()
       IOC.Initing smts tdefs params unid putmsgs
         -> return ()
       _ -> do                                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Simulation/Testing/Stepping stopped" ]
               put $ IOC.Initing { IOC.smts    = IOC.smts    envc
                                 , IOC.tdefs   = IOC.tdefs   envc
                                 , IOC.params  = IOC.params  envc
                                 , IOC.unid    = IOC.unid    envc
                                 , IOC.putmsgs = IOC.putmsgs envc
                                 }


-- ----------------------------------------------------------------------------------------- --
-- torxakis core main api -- get and set parameters, seed


txsGetParams :: IOC.IOC [(String,String)]
txsGetParams  =  do
       IOC.getParams []

-- ----------------------------------------------------------------------------------------- --

txsGetParam :: String -> IOC.IOC [(String,String)]
txsGetParam prm  =  do
       IOC.getParams [prm]

-- ----------------------------------------------------------------------------------------- --

txsSetParam :: String -> String -> IOC.IOC [(String,String)]
txsSetParam prm val  =  do
       IOC.setParams [(prm,val)]

-- ----------------------------------------------------------------------------------------- --

txsSeed :: Int -> IOC.IOC ()
txsSeed seed  =  do
     lift $ setStdGen(mkStdGen seed)
     IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_INFO $ "Seed set to " ++ (show seed) ]


-- ----------------------------------------------------------------------------------------- --
-- torxakis core main api -- data manipulation, evalution, solving


txsEval :: TxsDefs.VExpr -> IOC.IOC TxsDefs.Const
txsEval vexp  =  do
     let frees = FreeVar.freeVars vexp
      in if  not $ null frees
           then do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                 $ "Value expression not closed: " ++ (TxsShow.fshow frees) ]
                   return $ TxsDefs.Cerror ""
           else do envb         <- filterEnvCtoEnvB
                   (wal',envb') <- lift $ runStateT (Eval.eval vexp) envb
                   writeEnvBtoEnvC envb'
                   return $ wal'

-- ----------------------------------------------------------------------------------------- --

txsSolve :: TxsDefs.VExpr -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
txsSolve vexp  =  do
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
           SolveDefs.Solved sol    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE "sat" ]
                                         return sol
           SolveDefs.Unsolvable    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE "unsat" ]
                                         return Map.empty
           SolveDefs.UnableToSolve -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE "unknown" ]
                                         return Map.empty

                                         
-- ----------------------------------------------------------------------------------------- --

txsUniSolve :: TxsDefs.VExpr -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
txsUniSolve vexp  =  do
     if  SortOf.sortOf vexp /= SortOf.sortId_Bool
       then do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Value expression for solve shall be Bool" ]
         return Map.empty
       else do
         frees         <- return $ FreeVar.freeVars vexp
         assertions    <- return $ Solve.add vexp Solve.empty
         smtEnv        <- IOC.getSMT "current"
         (sat,smtEnv') <- lift $ runStateT (Solve.uniSolve frees assertions) smtEnv
         IOC.putSMT "current" smtEnv'
         case sat of
           SolveDefs.Solved sol    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE "sat" ]
                                         return sol
           SolveDefs.Unsolvable    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE "unsat" ]
                                         return Map.empty
           SolveDefs.UnableToSolve -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE "unknown" ]
                                         return Map.empty

-- ----------------------------------------------------------------------------------------- --

txsRanSolve :: TxsDefs.VExpr -> IOC.IOC (TxsDefs.WEnv TxsDefs.VarId)
txsRanSolve vexp  =  do
     if  SortOf.sortOf vexp /= SortOf.sortId_Bool
       then do
         IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Value expression for solve shall be Bool" ]
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
           SolveDefs.Solved sol    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE "sat" ]
                                         return sol
           SolveDefs.Unsolvable    -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE "unsat" ]
                                         return Map.empty
           SolveDefs.UnableToSolve -> do IOC.putMsgs [ EnvData.TXS_CORE_RESPONSE "unknown" ]
                                         return Map.empty


-- ----------------------------------------------------------------------------------------- --
-- torxakis core main api -- modus transition general -- set Tester, Simulator, Stepper

    
txsSetTest :: (TxsDDefs.Action -> IOC.IOC TxsDDefs.Action) ->
              IOC.IOC TxsDDefs.Action ->
              TxsDefs.ModelDef -> Maybe TxsDefs.MapperDef -> Maybe TxsDefs.PurpDef ->
              IOC.IOC ()
txsSetTest putToW getFroW moddef mapdef purpdef  =  do
     envc <- get
     case envc of
     { IOC.Noning params unid
         -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Tester started without model file" ]
     ; IOC.Initing smts tdefs params unid putmsgs
         -> do put $ IOC.Testing { IOC.smts      = smts
                                 , IOC.tdefs     = tdefs
                                 , IOC.modeldef  = moddef
                                 , IOC.mapperdef = mapdef
                                 , IOC.purpdef   = purpdef
                                 , IOC.puttow    = putToW
                                 , IOC.getfrow   = getFroW
                                 , IOC.behtrie   = []
                                 , IOC.inistate  = (-1)
                                 , IOC.curstate  = (-1)
                                 , IOC.nexstate  = (-1)
                                 , IOC.maxstate  = (-1)
                                 , IOC.modsts    = Map.empty
                                 , IOC.mapsts    = Map.empty
                                 , IOC.purpsts   = Map.empty
                                 , IOC.params    = params
                                 , IOC.unid      = unid
                                 , IOC.putmsgs   = putmsgs
                                 }
               if isConsistSetTest moddef mapdef purpdef
                 then do testInit
                         IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Tester started" ]
                 else do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Inconsistent definitions" ]
     ; _ -> do TxsCore.txsStop                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Simulation/Testing/Stepping stopped" ]
               TxsCore.txsSetTest putToW getFroW moddef mapdef purpdef
     }


isConsistSetTest :: TxsDefs.ModelDef -> Maybe TxsDefs.MapperDef -> Maybe TxsDefs.PurpDef -> Bool

isConsistSetTest (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
                 Nothing
                 Nothing
  =  True

isConsistSetTest (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
                 (Just (TxsDefs.MapperDef achins achouts asyncsets abexp))
                 Nothing
  =  let { mins   = Set.unions minsyncs
         ; mouts  = Set.unions moutsyncs
         ; ains   = Set.fromList $ achins
         ; aouts  = Set.fromList $ achouts
         }
      in    mins  `Set.isSubsetOf` ains
         && mouts `Set.isSubsetOf` aouts
 
isConsistSetTest (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
                 Nothing
                 (Just (TxsDefs.PurpDef pinsyncs poutsyncs psplsyncs pbexp))
  =  let { mins   = Set.unions minsyncs
         ; mouts  = Set.unions moutsyncs
         ; pins   = Set.unions pinsyncs
         ; pouts  = Set.unions poutsyncs
         }
      in    ( (pins  == Set.empty) || (pins  == mins)  )
         && ( (pouts == Set.empty) || (pouts == mouts) )

isConsistSetTest (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
                 (Just (TxsDefs.MapperDef achins achouts asyncsets abexp))
                 (Just (TxsDefs.PurpDef pinsyncs poutsyncs psplsyncs pbexp))
  =  let { mins   = Set.unions minsyncs
         ; mouts  = Set.unions moutsyncs
         ; ains   = Set.fromList $ achins
         ; aouts  = Set.fromList $ achouts
         ; pins   = Set.unions pinsyncs
         ; pouts  = Set.unions poutsyncs
         }
      in    ( (pins  == Set.empty) || (pins  == mins)  )
         && ( (pouts == Set.empty) || (pouts == mouts) )
         && mins  `Set.isSubsetOf` ains
         && mouts `Set.isSubsetOf` aouts

-- ----------------------------------------------------------------------------------------- --

txsSetSim :: (TxsDDefs.Action -> IOC.IOC TxsDDefs.Action) ->
             IOC.IOC TxsDDefs.Action ->
             TxsDefs.ModelDef -> Maybe TxsDefs.MapperDef ->
             IOC.IOC ()
txsSetSim putToW getFroW moddef mapdef  =  do
     envc    <- get
     case envc of
     { IOC.Noning params unid
         -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Simulator started without model file" ]
     ; IOC.Initing smts tdefs params unid putmsgs
         -> do put $ IOC.Simuling { IOC.smts      = smts
                                  , IOC.tdefs     = tdefs
                                  , IOC.modeldef  = moddef
                                  , IOC.mapperdef = mapdef
                                  , IOC.puttow    = putToW
                                  , IOC.getfrow   = getFroW
                                  , IOC.behtrie   = []
                                  , IOC.inistate  = (-1)
                                  , IOC.curstate  = (-1)
                                  , IOC.nexstate  = (-1)
                                  , IOC.maxstate  = (-1)
                                  , IOC.modsts    = Map.empty
                                  , IOC.mapsts    = Map.empty
                                  , IOC.params    = params
                                  , IOC.unid      = unid
                                  , IOC.putmsgs   = putmsgs
                                  }
               if isConsistSetSim moddef mapdef
                 then do simInit
                         IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Simulator started" ]
                 else do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Inconsistent definitions" ]
     ; _ -> do TxsCore.txsStop                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Simulation/Testing/Stepping stopped" ]
               TxsCore.txsSetSim putToW getFroW moddef mapdef
     }


isConsistSetSim :: TxsDefs.ModelDef -> Maybe TxsDefs.MapperDef -> Bool

isConsistSetSim (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
                Nothing
  =  True

isConsistSetSim (TxsDefs.ModelDef minsyncs moutsyncs msplsyncs mbexp)
                (Just (TxsDefs.MapperDef achins achouts asyncsets abexp))
  =  let { mins   = Set.unions minsyncs
         ; mouts  = Set.unions moutsyncs
         ; ains   = Set.fromList $ achins
         ; aouts  = Set.fromList $ achouts
         }
      in    mouts `Set.isSubsetOf` ains
         && mins  `Set.isSubsetOf` aouts

-- ----------------------------------------------------------------------------------------- --

txsSetStep :: TxsDefs.ModelDef -> IOC.IOC ()
txsSetStep moddef  =  do
     envc    <- get
     case envc of
     { IOC.Noning params unid
         -> IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Stepper started without model file" ]
     ; IOC.Initing smts tdefs params unid putmsgs
         -> do put $ IOC.Stepping { IOC.smts      = smts
                                  , IOC.tdefs     = tdefs
                                  , IOC.modeldef  = moddef
                                  , IOC.behtrie   = []
                                  , IOC.inistate  = (-1)
                                  , IOC.curstate  = (-1)
                                  , IOC.nexstate  = (-1)
                                  , IOC.maxstate  = (-1)
                                  , IOC.modsts    = Map.empty
                                  , IOC.params    = params
                                  , IOC.unid      = unid
                                  , IOC.putmsgs   = putmsgs
                                  }
               stepInit
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Stepper started" ]
     ; _ -> do TxsCore.txsStop                    -- IOC.Testing, IOC.Simuling, IOC.Stepping --
               IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO "Simulation/Testing/Stepping stopped" ]
               TxsCore.txsSetStep moddef
     }
   

-- ----------------------------------------------------------------------------------------- --


txsTestIn :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Verdict
txsTestIn act  =  do
     envc    <- get
     case envc of
     { IOC.Testing _ _ modeldef mapperdef Nothing _ _ _ _ _ _ _ _ _ _ _ _ _
         -> do Test.testIn act 1
     ; IOC.Testing _ _ modeldef mapperdef purpdef       _ _ _ _ _ _ _ _ _ _ _ _ _
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "No test action with test purpose" ]
               return $ TxsDDefs.NoVerdict
     ; _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Tester mode" ]
               return $ TxsDDefs.NoVerdict
     }


txsTestOut :: IOC.IOC TxsDDefs.Verdict
txsTestOut  =  do
     envc    <- get
     case envc of
     { IOC.Testing _ _ modeldef mapperdef Nothing _ _ _ _ _ _ _ _ _ _ _ _ _
         -> do Test.testOut 1
     ; IOC.Testing _ _ modeldef mapperdef purpdef       _ _ _ _ _ _ _ _ _ _ _ _ _
         -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "No test output with test purpose" ]
               return $ TxsDDefs.NoVerdict
     ; _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Tester mode" ]
               return $ TxsDDefs.NoVerdict
     }


txsTestN :: Int -> IOC.IOC TxsDDefs.Verdict
txsTestN depth  =  do  
     envc    <- get
     case envc of
     { IOC.Testing _ _ modeldef mapperdef purpdef _ _ _ _ _ _ _ _ _ _ _ _ _
         -> do Test.testN depth 1
     ; _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Tester mode" ]
               return $ TxsDDefs.NoVerdict
     }


-- ----------------------------------------------------------------------------------------- --


txsSimN :: Int -> IOC.IOC TxsDDefs.Verdict
txsSimN depth  =  do
     envc    <- get
     case envc of
     { IOC.Simuling _ _ modeldef mapperdef _ _ _ _ _ _ _ _ _ _ _ _
         -> do Sim.simN depth 1
     ; _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Simulator mode" ]
               return $ TxsDDefs.NoVerdict
     }


-- ----------------------------------------------------------------------------------------- --


txsStepN :: Int -> IOC.IOC TxsDDefs.Verdict
txsStepN depth  =  do
     envc    <- get
     case envc of
     { IOC.Stepping _ _ modeldef _ _ _ _ _ _ _ _ _
         -> do Step.stepN depth 1
     ; _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Stepper mode" ]
               return $ TxsDDefs.NoVerdict
     }


txsStepA :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Verdict
txsStepA act  =  do
     envc    <- get
     case envc of
     { IOC.Stepping _ _ modeldef _ _ _ _ _ _ _ _ _
         -> do Step.stepA act
     ; _ -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR "Not in Stepper mode" ]
               return $ TxsDDefs.NoVerdict
     }


-- ----------------------------------------------------------------------------------------- --


txsShow :: String -> IOC.IOC String
txsShow item  =  do
     envc <- get
     case item of
     { "tdefs"  -> do return $ show (IOC.tdefs envc)
     ; "state"  -> do return $ show (IOC.curstate envc)
     ; "model"  -> do curState <- return $ IOC.curstate envc
                      case Map.lookup curState (IOC.modsts envc) of 
                      { Nothing -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                                    $ "no current state" ]
                                      return $ "\n"
                      ; Just bt -> return $ TxsShow.fshow bt
                      }
     ; "mapper" -> do curState <- return $ IOC.curstate envc
                      case Map.lookup curState (IOC.mapsts envc) of 
                      { Nothing -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                                    $ "no current state" ]
                                      return $ "\n"
                      ; Just mt -> return $ TxsShow.fshow mt
                      }
     ; "purp"   -> do curState <- return $ IOC.curstate envc
                      case Map.lookup curState (IOC.purpsts envc) of    
                      { Nothing -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR 
                                                    $ "no current state" ]
                                      return $ "\n"
                      ; Just tp -> return $ TxsShow.fshow tp
                      } 
     ; _        -> do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR $ "nothing to be shown" ]
                      return $ "\n"
     }
  

-- ----------------------------------------------------------------------------------------- --


txsGoTo :: EnvData.StateNr -> IOC.IOC ()
txsGoTo stateNr  =  do
     if  stateNr >= 0
       then do modSts <- gets IOC.modsts
               case Map.lookup stateNr modSts of
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


-- ----------------------------------------------------------------------------------------- --

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

-- ----------------------------------------------------------------------------------------- --

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

-- ----------------------------------------------------------------------------------------- --

txsMapper :: TxsDDefs.Action -> IOC.IOC TxsDDefs.Action
txsMapper act  =  do
     mapperMap act


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

