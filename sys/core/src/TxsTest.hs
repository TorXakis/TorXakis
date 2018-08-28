{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TxsTest
-- Copyright   :  TNO and Radboud University
-- License     :  BSD3
-- Maintainer  :  jan.tretmans
-- Stability   :  experimental
--
-- Core Module TorXakis API:
-- Testing Mode
-----------------------------------------------------------------------------

-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ViewPatterns        #-}

module TxsTest

(
  -- ** set testing mode
  txsSetTest       -- :: IOC.EWorld ew
                   -- => D.ModelDef
                   -- -> Maybe D.MapperDef
                   -- -> ew
                   -- -> IOC.IOC (Either Error ())

  -- ** shut testing mode
, txsShutTest      -- :: IOC.IOC (Either Error ())

  -- ** start testing
, txsStartTest     -- :: Maybe D.PurpDef
                   -- -> IOC.IOC (Either Error ())

  -- ** stop testing
, txsStopTest      -- :: IOC.IOC (Either Error ())

  -- ** test sut with the provided input action
, txsTestActIn     -- :: DD.Action
                   -- -> IOC.IOC (Either Error DD.Verdict)

  -- ** test sut by observing output action
, txsTestActOut    -- :: IOC.IOC (Either Error DD.Verdict)

  -- ** test sut with action according to offer-pattern in the model
, txsTestOfferIn   -- :: D.Offer
                   -- -> IOC.IOC (Either Error DD.Verdict)

  -- ** test sut with the provided number of actions
, txsTestRun       -- :: Int
                   -- -> IOC.IOC (Either Error DD.Verdict)

  -- ** give the input menu, i.e., all possible input offers, in the model
, txsTestMenuIn    --  :: IOC.IOC (Either Error BTree.Menu)

  -- ** give the output menu, i.e., all possible output offers, in the model
, txsTestMenuOut   --  :: IOC.IOC (Either Error BTree.Menu)

  -- ** give current state number
, txsTestStateNr   --  :: IOC.IOC (Either Error EnvData.StateNr)

  -- ** give current state
, txsTestState     --  :: IOC.IOC (Either Error BTree.BTree)

  -- ** give trace from initial state to current state
, txsTestTrace     --  :: IOC.IOC (Either Error [DD.Action])

)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Arrow
import           Control.Monad.State
import qualified Data.List           as List
import qualified Data.Set            as Set

-- import from local
import           CoreUtils
import           Test

-- import from behave(defs)
import qualified Behave
import qualified BTree

-- import from coreenv
import qualified EnvCore             as IOC
import qualified EnvData

-- import from defs
import qualified TxsDDefs            as DD
import qualified TxsDefs             as D
import qualified TxsShow


-- ----------------------------------------------------------------------------------------- --
-- | Set Testing Mode.
--
--   Only possible when in Initing Mode.
txsSetTest :: IOC.EWorld ew
           => D.ModelDef                           -- ^ model definition.
           -> Maybe D.MapperDef                    -- ^ optional mapper definition.
           -> ew                                   -- ^ external world.
           -> IOC.IOC (Either Error ())
txsSetTest moddef mapdef eworld  =  do
     envc <- get
     case IOC.state envc of
       IOC.Initing { IOC.smts      = smts
                   , IOC.tdefs     = tdefs
                   , IOC.sigs      = sigs
                   , IOC.putmsgs  = putmsgs
                   }
         -> do IOC.putCS IOC.TestSet { IOC.smts      = smts
                                     , IOC.tdefs     = tdefs
                                     , IOC.sigs      = sigs
                                     , IOC.modeldef  = moddef
                                     , IOC.mapperdef = mapdef
                                     , IOC.eworld    = eworld
                                     , IOC.putmsgs   = putmsgs
                                     }
               Right <$> putmsgs [ EnvData.TXS_CORE_USER_INFO
                                   "Testing Mode set" ]
       _ -> return $ Left $ Error "Testing Mode must be set from Initing mode"

-- ----------------------------------------------------------------------------------------- --
-- | Shut Testing Mode.
--
--   Only possible when in TestSet Mode.
txsShutTest :: IOC.IOC (Either Error ())
txsShutTest  =  do
     envc <- get
     case IOC.state envc of
       IOC.TestSet { IOC.smts      = smts
                   , IOC.tdefs     = tdefs
                   , IOC.sigs      = sigs
                   , IOC.modeldef  = _moddef
                   , IOC.mapperdef = _mapdef
                   , IOC.eworld    = _eworld
                   , IOC.putmsgs   = putmsgs
                   }
         -> do IOC.putCS IOC.Initing { IOC.smts     = smts
                                     , IOC.tdefs    = tdefs
                                     , IOC.sigs     = sigs
                                     , IOC.putmsgs  = putmsgs
                                     }
               Right <$> putmsgs [ EnvData.TXS_CORE_USER_INFO
                                   "Testing Mode shut" ]
       _ -> return $ Left $ Error "Testing Mode must be shut from TestSet Mode"

-- ----------------------------------------------------------------------------------------- --
-- | Start testing.
--
--   Only possible when in TestSet Mode.
txsStartTest :: Maybe D.PurpDef                    -- ^ optional test purpose definition.
             -> IOC.IOC (Either Error ())
txsStartTest purpdef  =  do
     envc <- get
     case IOC.state envc of
       IOC.TestSet { IOC.smts      = smts
                   , IOC.tdefs     = tdefs
                   , IOC.sigs      = sigs
                   , IOC.modeldef  = moddef
                   , IOC.mapperdef = mapdef
                   , IOC.eworld    = eworld
                   , IOC.putmsgs   = putmsgs
                   }
         -> do IOC.putCS IOC.Testing { IOC.smts      = smts
                                     , IOC.tdefs     = tdefs
                                     , IOC.sigs      = sigs
                                     , IOC.modeldef  = moddef
                                     , IOC.mapperdef = mapdef
                                     , IOC.purpdef   = purpdef
                                     , IOC.eworld    = eworld
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
                 Nothing
                   -> Right <$> putmsgs [ EnvData.TXS_CORE_SYSTEM_INFO
                                          "Starting Testing Mode failed" ]
                 Just bt
                   -> do eworld' <- IOC.startW eworld
                         IOC.putCS IOC.Testing { IOC.smts      = smts
                                               , IOC.tdefs     = tdefs
                                               , IOC.sigs      = sigs
                                               , IOC.modeldef  = moddef
                                               , IOC.mapperdef = mapdef
                                               , IOC.purpdef   = purpdef
                                               , IOC.eworld    = eworld'
                                               , IOC.behtrie   = []
                                               , IOC.inistate  = 0
                                               , IOC.curstate  = 0
                                               , IOC.modsts    = bt
                                               , IOC.mapsts    = mt
                                               , IOC.purpsts   = fmap (second Left) gls
                                               , IOC.putmsgs   = putmsgs
                                               }
                         unless
                           (null gls)
                           (putmsgs [ EnvData.TXS_CORE_USER_INFO $ "Goals: " ++
                                      List.intercalate "," (TxsShow.fshow . fst <$> gls) ])
                         Right <$> putmsgs [ EnvData.TXS_CORE_USER_INFO
                                             "Testing Mode started" ]
       _ -> return $ Left $ Error "Testing Mode must be started from TestSet Mode"


startTester :: D.ModelDef
            -> Maybe D.MapperDef
            -> Maybe D.PurpDef
            -> IOC.IOC ( Maybe BTree.BTree, BTree.BTree, [(D.GoalId,BTree.BTree)] )

startTester (D.ModelDef minsyncs moutsyncs msplsyncs mbexp)
            Nothing
            Nothing  =
     let allSyncs = minsyncs ++ moutsyncs ++ msplsyncs
     in do
       envb            <- filterEnvCtoEnvB
       (maybt', envb') <- lift $ runStateT (Behave.behInit allSyncs mbexp) envb
       writeEnvBtoEnvC envb'
       return ( maybt', [], [] )

startTester (D.ModelDef  minsyncs moutsyncs msplsyncs mbexp)
            (Just (D.MapperDef achins achouts asyncsets abexp))
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

startTester (D.ModelDef minsyncs moutsyncs msplsyncs mbexp)
            Nothing
            (Just (D.PurpDef pinsyncs poutsyncs psplsyncs goals))  =
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

startTester (D.ModelDef  minsyncs moutsyncs msplsyncs mbexp)
            (Just (D.MapperDef achins achouts asyncsets abexp))
            (Just (D.PurpDef pinsyncs poutsyncs psplsyncs goals))  =
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

goalInit :: [ Set.Set D.ChanId ]
         -> ( D.GoalId, D.BExpr )
         -> IOC.IOC [ (D.GoalId, BTree.BTree) ]
goalInit chsets (gid,bexp)  =  do
     envb           <- filterEnvCtoEnvB
     (maypt',envb') <- lift $ runStateT (Behave.behInit chsets bexp) envb
     writeEnvBtoEnvC envb'
     return $ case maypt' of
              { Nothing  -> []
              ; Just pt' -> [ (gid, pt') ]
              }

-- ----------------------------------------------------------------------------------------- --
-- | Stop testing.
--
--   Only possible when Testing Mode.
txsStopTest :: IOC.IOC (Either Error ())
txsStopTest  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing { IOC.smts      = smts
                   , IOC.tdefs     = tdefs
                   , IOC.sigs      = sigs
                   , IOC.modeldef  = moddef
                   , IOC.mapperdef = mapdef
                   , IOC.purpdef   = _purpdef
                   , IOC.eworld    = eworld
                   , IOC.behtrie   = _behtrie
                   , IOC.inistate  = _inistate
                   , IOC.curstate  = _curstate
                   , IOC.modsts    = _modsts
                   , IOC.mapsts    = _mapsts
                   , IOC.purpsts   = _purpsts
                   , IOC.putmsgs   = putmsgs
                   }
         -> do eworld' <- IOC.stopW eworld
               IOC.putCS IOC.TestSet { IOC.smts      = smts
                                     , IOC.tdefs     = tdefs
                                     , IOC.sigs      = sigs
                                     , IOC.modeldef  = moddef
                                     , IOC.mapperdef = mapdef
                                     , IOC.eworld    = eworld'
                                     , IOC.putmsgs   = putmsgs
                                     }
               Right <$> putmsgs [ EnvData.TXS_CORE_USER_INFO
                                   "Testing Mode stopped" ]
       _ -> return $ Left $ Error "Testing Mode must be stopped from Testing Mode"

-- ----------------------------------------------------------------------------------------- --
-- | Test SUT with the provided input action.
--
--   Only possible when Testing
txsTestActIn :: DD.Action                                 -- ^ input action to SUT.
             -> IOC.IOC (Either Error DD.Verdict)   -- ^ verdict of test.
txsTestActIn act  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing { IOC.purpdef = Nothing }
         -> do (_,verdict) <- Test.testIn act 1
               return $ Right verdict
       _ -> return $ Left
                   $ Error "Testing with action only in Testing Mode (without Test Purpose)"

-- ----------------------------------------------------------------------------------------- --
-- | Test SUT by observing output action.
--
--   Only possible when Testing
txsTestActOut :: IOC.IOC (Either Error DD.Verdict)
txsTestActOut  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing { IOC.purpdef = Nothing }
         -> do (_, verdict) <- Test.testOut 1
               return $ Right verdict
       _ -> return $ Left
                   $ Error "Testing observation only in Testing Mode (without Test Purpose)"

-- ----------------------------------------------------------------------------------------- --
-- | Test SUT with action according to offer-pattern in the model.
-- 
--   Only possible when Testing
txsTestOfferIn :: D.Offer                         -- ^ Offer-pattern for test input action.
               -> IOC.IOC (Either Error DD.Verdict)
txsTestOfferIn offer  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing { IOC.purpdef = Nothing
                   , IOC.putmsgs = putmsgs
                   }
         -> do mact <- randOff2Act offer
               case mact of
                 Nothing
                   -> do putmsgs [ EnvData.TXS_CORE_USER_INFO
                                   "Could not generate action for testing with input" ]
                         return $ Right DD.NoVerdict
                 Just act
                   -> do (_,verdict) <- Test.testIn act 1
                         return $ Right verdict
       _ -> return $ Left
                   $ Error "Testing with offer only in Testing Mode (without Test Purpose)"

-- ----------------------------------------------------------------------------------------- --
-- | Test SUT with the provided number of actions.
--
--   Only possible when Testing
txsTestRun :: Int                                       -- ^ number of actions to test the SUT
           -> IOC.IOC (Either Error DD.Verdict)   -- ^ verdict after test run
txsTestRun nrsteps  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing {}
         -> Right <$> Test.testN nrsteps 1
       _ -> do return $ Left $ Error "Testing Run only in Testing Mode"

-- ----------------------------------------------------------------------------------------- --
-- | Give the input menu, i.e., all possible input offers, in the model.
--
--   Only possible when Testing
txsTestMenuIn :: IOC.IOC (Either Error BTree.Menu)
txsTestMenuIn  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing {}
         -> Right <$> Test.testModelMenuIn
       _ -> return $ Left $ Error "Testing MenuIn only in Testing Mode"
 
-- ----------------------------------------------------------------------------------------- --
-- | Give the output menu, i.e., all possible output offers, in the model.
--
--   Only possible when Testing
txsTestMenuOut :: IOC.IOC (Either Error BTree.Menu)
txsTestMenuOut  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing {}
         -> Right <$> Test.testModelMenuOut
       _ -> return $ Left $ Error "Testing MenuOut only in Testing Mode"
 
-- ----------------------------------------------------------------------------------------- --
-- | Give current state number
-- 
--   Only possible when Testing
txsTestStateNr :: IOC.IOC (Either Error EnvData.StateNr)
txsTestStateNr  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing { IOC.curstate = curstate }
         -> return $ Right curstate
       _ -> return $ Left $ Error "Current state of testing only in Testing Mode"

-- ----------------------------------------------------------------------------------------- --
-- | Give current state.
--
--   Only possible when Testing
txsTestState :: IOC.IOC (Either Error BTree.BTree)
txsTestState  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing { IOC.modsts = modsts }
         -> return $ Right modsts
       _ -> return $ Left $ Error "Current state of testing only in Testing Mode"
 
-- ----------------------------------------------------------------------------------------- --
-- | Give trace from initial state to current state.
--
--   Only possible when Testing
txsTestTrace :: IOC.IOC (Either Error [DD.Action])
txsTestTrace  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing { IOC.behtrie  = behtrie
                   , IOC.inistate = inistate
                   , IOC.curstate = curstate
                   }
         -> case trace behtrie inistate curstate of
              Nothing -> return $ Left $ EnvData.TXS_CORE_SYSTEM_ERROR
                                         "Path error: Behaviour Trie is not a tree"
              Just t  -> return $ Right t
       _ -> return $ Left $ Error "Trace of testing only in Testing Mode"
  where
     trace :: [(EnvData.StateNr, DD.Action, EnvData.StateNr)]
           -> EnvData.StateNr
           -> EnvData.StateNr
           -> (Maybe [DD.Action])
     trace _behtrie from to    | from >  to  =  Nothing
     trace _behtrie from to    | from == to  =  Just []
     trace  behtrie from to -- | from <  to
       =  case [ (s1,a,s2) | (s1,a,s2) <- behtrie, s2 == to ] of
            [(s1,a,_s2)] -> case trace behtrie from s1 of
                              Nothing -> Nothing
                              Just t  -> Just $ t ++ [a]
            _            -> Nothing


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

