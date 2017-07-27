{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}
{-# LANGUAGE RecordWildCards #-}

-- ----------------------------------------------------------------------------------------- --

module CoreUtils

-- ----------------------------------------------------------------------------------------- --
-- 
-- Utilities for Core
-- 
-- ----------------------------------------------------------------------------------------- --
-- export

( filterEnvCtoEnvB  -- :: IOC.IOC IOB.EnvB
, writeEnvBtoEnvC   -- :: IOB.EnvB -> IOC.IOC ()
, isInCTOffers      -- :: Set.Set BTree.CTOffer -> IOC.IOC Bool
, isOutCTOffers     -- :: Set.Set BTree.CTOffer -> IOC.IOC Bool
, isInAct           -- :: TxsDDefs.Action -> IOC.IOC Bool
, randMenu          -- :: BTree.Menu -> IOC.IOC (Maybe TxsDDefs.Action)
, menuConjunct      -- :: BTree.Menu -> BTree.Menu -> BTree.Menu
, menuConjuncts     -- :: [BTree.Menu] -> BTree.Menu
)

-- ----------------------------------------------------------------------------------------- --
-- import 

where

import System.IO
import Control.Monad.State

import qualified Data.Set  as Set
import qualified Data.Map  as Map

-- import from behavedef
import qualified BTree     as BTree

-- import from behaveenv
import qualified EnvCore   as IOC
import qualified EnvBTree  as IOB

-- import from defs
import qualified TxsDefs
import qualified TxsDDefs
import qualified Utils
import qualified Sigs
import qualified SolveDefs
import qualified Solve


-- ----------------------------------------------------------------------------------------- --
-- filterEnvCtoEnvB


filterEnvCtoEnvB :: IOC.IOC IOB.EnvB
filterEnvCtoEnvB  =  do
     envc <- get
     case IOC.state envc of
     { IOC.Noning
         -> return $ IOB.EnvB { IOB.smts     = Map.empty
                              , IOB.tdefs    = TxsDefs.empty
                              , IOB.sigs     = Sigs.empty
                              , IOB.stateid  = (-1)
                              , IOB.params   = IOC.params envc
                              , IOB.unid     = IOC.unid envc
                              , IOB.msgs     = []
                              }
     ; IOC.Initing {..}
         -> return $ IOB.EnvB { IOB.smts     = smts
                              , IOB.tdefs    = tdefs
                              , IOB.sigs     = sigs
                              , IOB.stateid  = (-1)
                              , IOB.params   = IOC.params envc
                              , IOB.unid     = IOC.unid envc
                              , IOB.msgs     = []
                              }
     ; IOC.Testing {..}
         -> return $ IOB.EnvB { IOB.smts     = smts
                              , IOB.tdefs    = tdefs
                              , IOB.sigs     = sigs
                              , IOB.stateid  = curstate
                              , IOB.params   = IOC.params envc
                              , IOB.unid     = IOC.unid envc
                              , IOB.msgs     = []
                              }
     ; IOC.Simuling {..}
         -> return $ IOB.EnvB { IOB.smts     = smts
                              , IOB.tdefs    = tdefs
                              , IOB.sigs     = sigs
                              , IOB.stateid  = curstate
                              , IOB.params   = IOC.params envc
                              , IOB.unid     = IOC.unid envc
                              , IOB.msgs     = []
                              }
     ; IOC.Stepping {..}
         -> return $ IOB.EnvB { IOB.smts     = smts
                              , IOB.tdefs    = tdefs
                              , IOB.sigs     = sigs
                              , IOB.stateid  = curstate
                              , IOB.params   = IOC.params envc
                              , IOB.unid     = IOC.unid envc
                              , IOB.msgs     = []
                              }
     }


-- ----------------------------------------------------------------------------------------- --
-- filterEnvCtoEnvB


writeEnvBtoEnvC :: IOB.EnvB -> IOC.IOC ()
writeEnvBtoEnvC envb  =  do
  putMsgs <- gets (IOC.putmsgs . IOC.state)
  putMsgs $ IOB.msgs envb
  modify $ \env -> env { IOC.unid = IOB.unid envb }


-- ----------------------------------------------------------------------------------------- --
-- isInCTOffers  :  is input offer?


isInCTOffers :: Set.Set BTree.CTOffer -> IOC.IOC Bool
isInCTOffers ctoffers  =  do
     TxsDefs.ModelDef insyncs outsyncs splsyncs bexp <- gets (IOC.modeldef . IOC.state)
     chinset  <- return $ Set.unions insyncs
     choutset <- return $ Set.unions outsyncs
     chanids  <- return $ Set.map BTree.ctchan ctoffers
     return $    not (Set.null (chanids `Set.intersection` chinset))
              &&     (Set.null (chanids `Set.intersection` choutset))


isInAct :: TxsDDefs.Action -> IOC.IOC Bool

isInAct (TxsDDefs.Act acts)  =  do
     TxsDefs.ModelDef insyncs outsyncs splsyncs bexp <- gets (IOC.modeldef . IOC.state)
     chinset  <- return $ Set.unions insyncs
     choutset <- return $ Set.unions outsyncs
     chanids  <- return $ Set.map fst acts
     return $    not (Set.null (chanids `Set.intersection` chinset))
              &&     (Set.null (chanids `Set.intersection` choutset))

isInAct (TxsDDefs.ActQui)  =  do
     return $ False


-- ----------------------------------------------------------------------------------------- --
-- isOutCTOffers :  is output offer?


isOutCTOffers :: Set.Set BTree.CTOffer -> IOC.IOC Bool
isOutCTOffers ctoffers  =  do
     TxsDefs.ModelDef insyncs outsyncs splsyncs bexp <- gets (IOC.modeldef . IOC.state)
     chinset  <- return $ Set.unions insyncs
     choutset <- return $ Set.unions outsyncs
     chanids  <- return $ Set.map BTree.ctchan ctoffers
     return $        (Set.null (chanids `Set.intersection` chinset))
              && not (Set.null (chanids `Set.intersection` choutset))


-- ----------------------------------------------------------------------------------------- --
-- randMenu :  menu randomization
                                                                                           --

randMenu :: BTree.Menu -> IOC.IOC (Maybe TxsDDefs.Action)
randMenu menu  =  do
     if null menu
       then do
         return $ Nothing
       else do
         ( (ctoffs, hvars, preds) : menu' ) <- lift $ Utils.randOrder menu
         vvars <- return $ concat ( map BTree.ctchoffers (Set.toList ctoffs) )
         ivars <- return $ vvars ++ hvars
         let assertions = foldr Solve.add Solve.empty preds in do
             smtEnv <- IOC.getSMT "current"
             parammap <- gets IOC.params
             let p = Solve.toRandParam parammap
             (sat,smtEnv') <- lift $ runStateT (Solve.randSolve p ivars assertions) smtEnv
             IOC.putSMT "current" smtEnv'
             case sat of
             { SolveDefs.Solved sol    -> do return $ Just $ TxsDDefs.Act
                                                        (Set.map (instantCTOffer sol) ctoffs)
             ; SolveDefs.Unsolvable    -> do randMenu menu'
             ; SolveDefs.UnableToSolve -> do randMenu menu'
             }


instantCTOffer :: (Map.Map BTree.IVar TxsDefs.Const) -> BTree.CTOffer ->
                  (TxsDefs.ChanId, [TxsDefs.Const])
instantCTOffer sol (BTree.CToffer chan choffs)
  =  ( chan, map (instantIVar sol) choffs )


instantIVar :: (Map.Map BTree.IVar TxsDefs.Const) -> BTree.IVar -> TxsDefs.Const
instantIVar sol ivar
  =  case Map.lookup ivar sol of
     { Nothing  -> error $ "TXS Test ranMenuIn: No value for interaction variable\n"
     ; Just wal -> wal
     }


-- ----------------------------------------------------------------------------------------- --
-- conjunction of menus


menuConjunct :: BTree.Menu -> BTree.Menu -> BTree.Menu
menuConjunct menu1 menu2
  =  [ ( ctoffs1, hvars1 ++ hvars2, preds1 ++ preds2 )
     | (ctoffs1,hvars1,preds1) <- menu1
     , (ctoffs2,hvars2,preds2) <- menu2
     , ctoffs1 == ctoffs2
     ]


menuConjuncts :: [BTree.Menu] -> BTree.Menu
menuConjuncts []            =  []
menuConjuncts [menu]        =  menu
menuConjuncts (menu:menus)  =  menuConjunct menu (menuConjuncts menus)


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

