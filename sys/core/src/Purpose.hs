{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE OverloadedStrings #-}
module Purpose

-- ----------------------------------------------------------------------------------------- --
--
-- Test Primitives for a Test Purpose, built on Behave
--
-- ----------------------------------------------------------------------------------------- --
-- export


( goalMenu          -- :: String -> IOC.IOC BTree.Menu
, purpMenusIn       -- :: IOC.IOC [BTree.Menu]
, purpAfter         -- :: TxsDDefs.Action -> IOC.IOC (Bool,Bool)
, purpVerdict       -- :: IOC.IOC ()
)

-- these functions shoulds also work properly if there is no purpose

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad.State

import qualified Data.Set            as Set
import qualified Data.Text           as T

-- import from local
import           CoreUtils

import qualified Behave

-- import from behavedef
import qualified BTree

-- import from coreenv
import qualified EnvCore             as IOC
import qualified EnvData

-- import from defs
import qualified StdTDefs
import qualified TxsDDefs
import qualified TxsDefs
import qualified TxsShow
import qualified Utils


-- ----------------------------------------------------------------------------------------- --
-- assumes Testing modus and valid PurpDef

-- ----------------------------------------------------------------------------------------- --
-- goalMenu :  menu on current btree of goal with name

goalMenu :: String -> IOC.IOC BTree.Menu
goalMenu gnm = do
  envc <- get
  case IOC.state envc of
    IOC.Testing { IOC.purpdef = Just (TxsDefs.PurpDef pinsyncs poutsyncs psplsyncs _)
                , IOC.purpsts = purpsts
                } -> do
      let pAllSyncs = pinsyncs ++ poutsyncs ++ psplsyncs
      case [ (gid, gtree) | (gid@(TxsDefs.GoalId nm _), gtree) <- purpsts, nm == T.pack gnm ] of
        [(_, Left bt)] -> return $ Behave.behMayMenu pAllSyncs bt
        [(_, Right _)] -> return []
        _ -> do
          IOC.putMsgs [EnvData.TXS_CORE_SYSTEM_ERROR "no (unique) goal given"]
          return []
    _ -> do
      IOC.putMsgs [EnvData.TXS_CORE_USER_ERROR "goalMenu: no test purpose given"]
      return []

-- ----------------------------------------------------------------------------------------- --
-- purpMenuIn :  menu of input actions of test purpose


purpMenusIn :: IOC.IOC [BTree.Menu]
purpMenusIn  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing { IOC.purpdef = Just (TxsDefs.PurpDef pinsyncs poutsyncs psplsyncs _)
                   , IOC.purpsts = purpsts
                   }
         | not $ null pinsyncs -> do
            let pAllSyncs = pinsyncs ++ poutsyncs ++ psplsyncs
            mapM goalMenuIn [ (gid,btree) | (gid,Left btree) <- purpsts
                                          , not $ isHit  pAllSyncs btree
                                          , not $ isMiss pAllSyncs btree
                                          , not $ isHalt btree
                                          ]
       _ -> return []

goalMenuIn :: (TxsDefs.GoalId,BTree.BTree) -> IOC.IOC BTree.Menu
goalMenuIn (_,btree)  =  do
     envc <- get
     case IOC.state envc of
       IOC.Testing { IOC.purpdef = Just (TxsDefs.PurpDef pinsyncs poutsyncs psplsyncs _)
                   } -> do
            let pAllSyncs = pinsyncs ++ poutsyncs ++ psplsyncs
                chins     = Set.unions pinsyncs
            return [ (ctoffs, hvars, preds)
                   | (ctoffs, hvars, preds) <- Behave.behMayMenu pAllSyncs btree
                   , Set.map BTree.ctchan ctoffs `Set.isSubsetOf` chins
                   ]
       _ -> return []

-- ----------------------------------------------------------------------------------------- --
-- purpAfter :  after state for test purpose

purpAfter :: TxsDDefs.Action -> IOC.IOC Bool                                -- purpose ready --
purpAfter act  =  do
     envc  <- get
     isInp <- isInAct act
     case IOC.state envc of
       IOC.Testing { IOC.purpdef = Just (TxsDefs.PurpDef pinsyncs poutsyncs psplsyncs _)
                   , IOC.purpsts = purpsts
                   } -> do
            let pAllSyncs  =  pinsyncs ++ poutsyncs ++ psplsyncs
            case (isInp,pinsyncs,poutsyncs) of
              ( _   , [], []) -> return False
              (True , [], _ ) -> return False
              (False, _ , []) -> return False
              ( _   , _ , _ ) -> do
                   aftGoals  <- mapM (goalAfter pAllSyncs poutsyncs act)
                                     [ (gid,Left btree) | (gid,Left btree) <- purpsts
                                                   , not $ isHit  pAllSyncs btree
                                                   , not $ isMiss pAllSyncs btree
                                                   , not $ isHalt btree
                                     ]
                   let newGoals  = aftGoals ++ [ (gid,Left btree)
                                               | (gid,Left btree) <- purpsts
                                               , gid `notElem` map fst aftGoals
                                               ] ++ [verd | verd@(_,Right _) <- purpsts ]
                   IOC.modifyCS $ \st -> st { IOC.purpsts = newGoals }
                   purpVerdict
       _ -> do
            IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "purpAfter incorrectly used" ]
            return True

goalAfter :: [Set.Set TxsDefs.ChanId] -> [Set.Set TxsDefs.ChanId] -> TxsDDefs.Action ->
             (TxsDefs.GoalId,Either BTree.BTree TxsDDefs.PurpVerdict) ->
             IOC.IOC (TxsDefs.GoalId,Either BTree.BTree TxsDDefs.PurpVerdict)

goalAfter allsyncs _ (TxsDDefs.Act acts) (gid,Left btree)  =  do
     envb           <- filterEnvCtoEnvB
     (maybt',envb') <- lift $
       runStateT (Behave.behAfterAct allsyncs btree acts) envb
     writeEnvBtoEnvC envb'
     case maybt' of
      Nothing  -> return (gid,Left [])
      Just bt' -> return (gid,Left bt')
goalAfter allsyncs outsyncs TxsDDefs.ActQui (gid,Left btree)  =  do
     let qacts      = Set.singleton (StdTDefs.chanId_Qstep, [])
     envb           <- filterEnvCtoEnvB
     (maybt1,envb1) <- lift $
       runStateT (Behave.behAfterRef btree (Set.unions outsyncs)) envb
     (maybt2,envb2) <- lift $
       runStateT (Behave.behAfterAct allsyncs btree qacts) envb1
     writeEnvBtoEnvC envb2
     case (maybt1,maybt2) of
      (Nothing ,Nothing ) -> return (gid,Left [])
      (Just bt1,Nothing ) -> return (gid,Left bt1)
      (Nothing ,Just bt2) -> return (gid,Left bt2)
      (Just bt1,Just bt2) -> return (gid,Left (bt1++bt2))
goalAfter _ _ _ goal@(_, Right _) = return goal
      

-- ----------------------------------------------------------------------------------------- --
-- purpVerdict :  output of  hit/miss verdicts

purpVerdict :: IOC.IOC Bool -- did any goal hit?
purpVerdict = do
  envc <- get
  case IOC.state envc of
    IOC.Testing { IOC.purpsts = purpsts } -> do
      hitsAndNewPurps <- mapM goalVerdict purpsts
      let (hasHit, newPurps) = unzip3to2 hitsAndNewPurps
      IOC.modifyCS (\s -> s {IOC.purpsts = newPurps})
      return $ or hasHit
    _ -> do
      IOC.putMsgs [EnvData.TXS_CORE_SYSTEM_ERROR "purpVerdict incorrectly used"]
      return False
  where
    unzip3to2 = foldr (\(x, y, z) (xs, yzs) -> (x:xs, (y, z):yzs)) ([], [])

goalVerdict :: (TxsDefs.GoalId, Either BTree.BTree TxsDDefs.PurpVerdict)
    -> IOC.IOC (Bool, TxsDefs.GoalId, Either BTree.BTree TxsDDefs.PurpVerdict)
    -- (did the goal hit, updated goal/verdict)
goalVerdict (gid, Right goal) = return (False, gid, Right goal)
goalVerdict (gid, Left btree) = do
     envc <- get
     case IOC.state envc of
       IOC.Testing { IOC.purpdef = Just (TxsDefs.PurpDef pinsyncs poutsyncs psplsyncs _)
                   }  -> do
            let pAllSyncs = pinsyncs ++ poutsyncs ++ psplsyncs
            let (hasHit, newGoal) = case (isHit pAllSyncs btree, isMiss pAllSyncs btree, isHalt btree) of
                          (False,False,False) -> (False, Just $ Left btree)
                          (True ,False,False) -> (True, Just $ Right TxsDDefs.PurpHit)
                          (False,True ,False) -> (False, Just $ Right TxsDDefs.PurpMiss)
                          (False,False,True ) -> (False, Just $ Right TxsDDefs.PurpHalted)
                          (_    ,_    ,_    ) -> (False, Nothing)
            case newGoal of
              Nothing -> do
                IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "goalVerdict incorrectly used" ]
                return (hasHit, gid, Right TxsDDefs.PurpHalted)
              Just (Left beh) -> return (hasHit, gid, Left beh)
              Just (Right verd) -> do
                IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO $ "Goal " ++ TxsShow.fshow gid ++ ": " ++ TxsShow.fshow verd]
                return (hasHit, gid, Right verd)
       _ -> do
            IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "goalVerdict incorrectly used" ]
            return (True, gid, Right TxsDDefs.PurpHalted)

-- ----------------------------------------------------------------------------------------- --
--  hit, miss
isHit :: [ Set.Set TxsDefs.ChanId ] -> BTree.BTree -> Bool
isHit allsyncs btree
  =  let menu = Behave.behMayMenu allsyncs btree
         chanids = Set.map BTree.ctchan (Set.unions (map Utils.frst menu))
      in StdTDefs.chanId_Hit `Set.member` chanids

isMiss :: [ Set.Set TxsDefs.ChanId ] -> BTree.BTree -> Bool
isMiss allsyncs btree
  =  let menu = Behave.behMayMenu allsyncs btree
         chanids = Set.map BTree.ctchan (Set.unions (map Utils.frst menu))
      in StdTDefs.chanId_Miss `Set.member` chanids

isHalt :: BTree.BTree -> Bool
isHalt = null
