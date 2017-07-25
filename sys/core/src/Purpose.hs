{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Purpose

-- ----------------------------------------------------------------------------------------- --
-- 
-- Test Primitives for a Test Purpose, built on Behave
-- 
-- ----------------------------------------------------------------------------------------- --
-- export


( goalMenu          -- :: String -> IOC.IOC BTree.Menu
, purpMenuIn        -- :: IOC.IOC BTree.Menu
, purpAfter         -- :: TxsDDefs.Action -> IOC.IOC (Bool,Bool)
, purpVerdict       -- :: IOC.IOC ()
)

-- ----------------------------------------------------------------------------------------- --
-- import 

where

import System.IO
import BTShow

import Control.Monad.State

import qualified Data.Set  as Set
import qualified Data.Map  as Map

-- import from local
import CoreUtils

import qualified Behave    as Behave

-- import from behavedef
import qualified BTree     as BTree

-- import from coreenv
import qualified EnvCore   as IOC
import qualified EnvData   as EnvData

-- import from defs
import qualified TxsDefs   as TxsDefs
import qualified TxsDDefs  as TxsDDefs
import qualified StdTDefs  as StdTDefs
import qualified TxsShow   as TxsShow
import qualified Utils     as Utils


-- ----------------------------------------------------------------------------------------- --
-- assumes Testing modus and valid PurpDef

-- ----------------------------------------------------------------------------------------- --
-- goalMenu :  menu on current btree of goal with name

getPurpdef :: StateT IOC.EnvC IO (Maybe TxsDefs.PurpDef)
getPurpdef = gets (IOC.purpdef . IOC.state)

getPupsts :: StateT IOC.EnvC IO  [(TxsDefs.GoalId, BTree.BTree)]
getPupsts = gets (IOC.purpsts . IOC.state)

goalMenu :: String -> IOC.IOC BTree.Menu
goalMenu gnm  =  do
     Just (TxsDefs.PurpDef insyncs outsyncs splsyncs goals) <- getPurpdef
     allSyncs <- return $ insyncs ++ outsyncs ++ splsyncs
     purpSts  <- getPupsts
     case [ (gid,gtree) | (gid@(TxsDefs.GoalId nm uid), gtree) <- purpSts, nm == gnm ] of
       [(gid,bt)] -> do return $ Behave.behMayMenu allSyncs bt
       _          -> do IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "no (unique) goal given" ]
                        return []


-- ----------------------------------------------------------------------------------------- --
-- purpMenuIn :  menu of input actions of test purpose


purpMenuIn :: IOC.IOC BTree.Menu
purpMenuIn  =  do
     Just (TxsDefs.PurpDef insyncs outsyncs splsyncs goals) <- getPurpdef
     allSyncs <- return $ insyncs ++ outsyncs ++ splsyncs
     purpSts  <- getPupsts
     menus    <- mapM goalMenuIn [ (gid,btree)
                                 | (gid,btree) <- purpSts
                                 , not $ isHit  allSyncs btree
                                 , not $ isMiss allSyncs btree
                                 , not $ isHalt btree
                                 ]
     return $ menuConjuncts menus


goalMenuIn :: (TxsDefs.GoalId,BTree.BTree) -> IOC.IOC BTree.Menu
goalMenuIn (gid,btree)  =  do
     Just (TxsDefs.PurpDef insyncs outsyncs splsyncs goals) <- getPurpdef
     allSyncs <- return $ insyncs ++ outsyncs ++ splsyncs
     chins    <- return $ Set.unions insyncs
     return $ [ (ctoffs, hvars, preds)
              | (ctoffs, hvars, preds) <- Behave.behMayMenu allSyncs btree
              , (Set.map BTree.ctchan ctoffs) `Set.isSubsetOf` chins
              ]


-- ----------------------------------------------------------------------------------------- --
-- purpAfter :  after state for test purpose


purpAfter :: TxsDDefs.Action -> IOC.IOC (Bool,Bool)
purpAfter act  =  do
     Just (TxsDefs.PurpDef insyncs outsyncs splsyncs goals) <- getPurpdef
     allSyncs <- return $ insyncs ++ outsyncs ++ splsyncs
     purpSts  <- getPupsts
     aftGoals <- mapM (goalAfter allSyncs outsyncs act) [ (gid,btree)
                                                        | (gid,btree) <- purpSts
                                                        , not $ isHit  allSyncs btree
                                                        , not $ isMiss allSyncs btree
                                                        , not $ isHalt btree
                                                        ]
     newGoals <- return $ aftGoals ++
                   [ (gid,btree) | (gid,btree) <- purpSts, gid `notElem` (map fst aftGoals) ]
     modify $ \envc -> envc { IOC.state = (IOC.state envc) { IOC.purpsts = newGoals } }
     return $ ( and [ isHit  allSyncs btree | (gid,btree) <- newGoals ]
              , and [ isMiss allSyncs btree | (gid,btree) <- newGoals ]
              )


goalAfter :: [Set.Set TxsDefs.ChanId] -> [Set.Set TxsDefs.ChanId] -> TxsDDefs.Action ->
             (TxsDefs.GoalId,BTree.BTree) -> IOC.IOC (TxsDefs.GoalId,BTree.BTree)

goalAfter allsyncs outsyncs (TxsDDefs.Act acts) (gid,btree)  =  do 
     envb           <- filterEnvCtoEnvB
     (maybt',envb') <- lift $ runStateT (Behave.behAfterAct allsyncs btree acts) envb
     writeEnvBtoEnvC envb'
     case maybt' of
     { Nothing  -> do return $ (gid,[])
     ; Just bt' -> do return $ (gid,bt')
     }

goalAfter allsyncs outsyncs (TxsDDefs.ActQui) (gid,btree)  =  do
     qacts          <- return $ Set.singleton (StdTDefs.chanId_Qstep,[])
     envb           <- filterEnvCtoEnvB
     (maybt1,envb1) <- lift $ runStateT (Behave.behAfterRef btree (Set.unions outsyncs)) envb
     (maybt2,envb2) <- lift $ runStateT (Behave.behAfterAct allsyncs btree qacts) envb1
     writeEnvBtoEnvC envb2
     case (maybt1,maybt2) of
     { (Nothing ,Nothing ) -> do return $ (gid,[])
     ; (Just bt1,Nothing ) -> do return $ (gid,bt1)
     ; (Nothing ,Just bt2) -> do return $ (gid,bt2)
     ; (Just bt1,Just bt2) -> do return $ (gid,bt1++bt2)
     }


-- ----------------------------------------------------------------------------------------- --
-- purpVerdict :  output of  hit/miss verdicts


purpVerdict :: IOC.IOC ()
purpVerdict  =  do
     purpSts <- getPupsts
     mapM_ goalVerdict purpSts


goalVerdict :: (TxsDefs.GoalId,BTree.BTree) -> IOC.IOC ()
goalVerdict (gid,btree)  =  do
     Just (TxsDefs.PurpDef insyncs outsyncs splsyncs goals) <- getPurpdef
     allSyncs <- return $ insyncs ++ outsyncs ++ splsyncs
     IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                   $ "Goal " ++ (TxsShow.fshow gid) ++ " : " ++
                     ( case (isHit allSyncs btree, isMiss allSyncs btree, isHalt btree) of
                       { (False,False,False) -> "still active"
                       ; (True ,False,False) -> "Hit"
                       ; (False,True ,False) -> "Miss"
                       ; (False,False,True ) -> "halted"
                       ; (True ,True ,False) -> "Hit and Miss: should not occur"
                       ; (_    ,_    ,_    ) -> "Hit/Miss/Halted: ???"
                       }
                 )   ] 


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
isHalt btree
  =  null btree


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

