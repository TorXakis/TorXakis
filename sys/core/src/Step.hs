{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Step

-- ----------------------------------------------------------------------------------------- --
-- 
-- Visibly stepping through an STS
--
-- ----------------------------------------------------------------------------------------- --

where

import System.IO
-- import System.Random
import Control.Monad.State
import Debug.Trace

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import TxsDefs
import TxsDDefs
import TxsEnv
import CTree
import SolveDefs
import Solve

import TxsShow
import Utils
import Primer

import ServerIf


-- ----------------------------------------------------------------------------------------- --
-- stepN :  make 'depth' random steps (visible) from current mstate


stepN :: Int -> Int -> IOE Verdict
stepN depth step  =  do
     if  depth == 0
       then do
         return $ Pass
       else do
         inmenu  <- menuIn
         outmenu <- menuOut
         mact    <- randMenu (inmenu ++ outmenu)
         case mact of
         { Nothing  -> do mack "STEP" $ "probably deadlock"
                          return $ Fail ActQui
         ; Just act -> do pack "STEP" $ (showN step 5) ++ ": " ++ (fshow act)
                          done <- doAfter act
                          if  done
                            then do stepN (depth-1) (step+1)
                            else do mack "STEP" $ "selected action cannot be done"
                                    return $ Fail ActQui
         }


-- ----------------------------------------------------------------------------------------- --
-- randMenu :  random action from a menu


randMenu :: [ (Set.Set CTOffer, [IVar], [ValExpr IVar]) ] -> IOE (Maybe Action)
randMenu menu  =  do
     if null menu 
       then do
         return $ Nothing
       else do
         ( (ctoffs, hvars, preds) : menu' ) <- lift $ randOrder menu
         vvars <- return $ concat ( map ctchoffers (Set.toList ctoffs) )
         ivars <- return $ vvars ++ hvars
         let assertions = foldr add Solve.empty preds in do
             smtEnv <- getSMT "current"
             (sat,smtEnv') <- lift $ runStateT (solve ivars assertions) smtEnv
             putSMT "current" smtEnv'
             case sat of
             { Solved sol    -> do return $ Just $ Act ( Set.map (instantCTOffer sol) ctoffs )
             ; Unsolvable    -> do randMenu menu'
             ; UnableToSolve -> do randMenu menu'
             }


instantCTOffer :: (Map.Map IVar Const) -> CTOffer -> (ChanId, [Const])
instantCTOffer sol (CToffer chan choffs)
  =  ( chan, map (instantIVar sol) choffs )


instantIVar :: (Map.Map IVar Const) -> IVar -> Const
instantIVar sol ivar
  =  case Map.lookup ivar sol of
     { Nothing  -> error $ "TXS Test ranMenuIn: No value for interaction variable\n"
     ; Just wal -> wal
     }


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

