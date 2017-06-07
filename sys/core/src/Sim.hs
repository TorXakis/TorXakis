{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Sim

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- Simulation of a Model
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
-- export

( simN     --  simN :: Int -> Handle -> IOE ()
           --  simN depth :  simulation of depth>=0 steps,
           --                or infinitely if depth<0,
           --                outputting on hout
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import System.IO
import System.Random
import Control.Monad.State
import GHC.Conc
-- import Debug.Trace

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import Params
import TxsDefs
import TxsDDefs
import TxsEnv
import TxsShow
import Cnect
import Primer
import Step
import CTree
import ServerIf


-- ----------------------------------------------------------------------------------------- --
-- simN depth :  simulation of depth>0 steps, or infinitely if depth<0


simN :: Int -> Int -> IOE Verdict
simN depth step  =  do
     param_InputCompletion <- getParam "param_InputCompletion"
     case (read param_InputCompletion) of
     { ANGELIC  -> do simA depth step
--   ; DEMONIC  -> do simD depth step
--   ; BUFFERED -> do simB depth step
     }


-- ----------------------------------------------------------------------------------------- --
-- simA depth :  angelic simulation of depth>0 steps, or infinitely if depth<0


simA :: Int -> Int -> IOE Verdict
simA depth step  =  do
     if  depth == 0
       then do return $ Pass
       else do iochoice <- lift $ randomRIO (True, False)
               if  iochoice
                 then do simAFroW depth step
                 else do simAToW  depth step


simAFroW :: Int -> Int -> IOE Verdict
simAFroW depth step  =  do
     simin  <- getFroW                                    -- get next input or qui from world
     case simin of
     { Act acts -> do                                     -- world provided input
         act <- doSimIn (Act acts)                        -- do input in model (via taus)
         pack "SIM" $ (showN step 5) ++ ":  IN:  "++ (fshow act)
         simA (depth-1) (step+1)
     ; ActQui -> do                                       -- world did not produce input
         simAToW depth step                               -- so continue with output to world
     }


simAToW :: Int -> Int -> IOE Verdict
simAToW depth step  =  do
     simout  <- proposeSimOut                             -- propose output or qui (via taus)
     case simout of
     { Act acts -> do                                     -- real output
         simact  <- putToW simout                         -- output to world
         if simact == simout
           then do                                        -- real output done to world
             act <- doSimOut (Act acts)                   -- do output in model (not via taus)
             pack "SIM" $ (showN step 5) ++ ": OUT: " ++ (fshow simout)
             simA (depth-1) (step+1)
           else do                                        -- input from world was faster
             case simact of
             { Act acts -> do                             -- world produced real input
                 act <- doSimIn (Act acts)                -- do input in model (via tau-steps)
                 pack "SIM" $ (showN step 5) ++ ":  IN:  " ++ (fshow act)
                 simA (depth-1) (step+1)
             ; _ -> do     
                 mack "SIM" $ "input is not input ..."
                 return $ Fail simact
             }
     ; ActQui -> do
         simAFroW depth step
     }


-- ----------------------------------------------------------------------------------------- --
-- proposeSimOut :  propose simulation output or qui (probably via tau-steps)


proposeSimOut :: IOE Action
proposeSimOut  =  do
     curbtree     <- getBTree
     taubranches  <- return $ [ b | b@(BTtau _) <- curbtree ]
     outbranches  <- filterM (isOutCTOffers.btoffers) [ b | b@(BTpref _ _ _ _) <- curbtree ]
     case ( taubranches, outbranches ) of 
     { ( [], [] ) -> do
         return $ ActQui
     ; ( (tb:tbs), [] ) -> do
         random      <- lift $ randomRIO ( 0, length(tb:tbs) - 1 )
         BTtau btree <- return $ (tb:tbs)!!random
         modify ( \env ->
                   env { envs2bt = Map.insert (envcurs env) btree (envs2bt env) }
                )
         proposeSimOut
     ; ( [], (ob:obs) ) -> do
         actout <- ranMenuOut
         case actout of
         { Nothing     -> do return $ ActQui
         ; Just simout -> do return $ simout
         }
     ; ( (tb:tbs), (ob:obs) ) -> do
         random <- lift $ randomRIO ( 0, length(tb:tbs) + length(ob:obs) - 1 )
         if random < length(tb:tbs)
           then do
             BTtau btree <- return $ (tb:tbs)!!random
             modify ( \env ->
                       env { envs2bt = Map.insert (envcurs env) btree (envs2bt env) }
                    )
             proposeSimOut
           else do
             actout <- ranMenuOut
             case actout of
             { Nothing     -> do
                 modify ( \env ->
                           env { envs2bt = Map.insert (envnrst env) (tb:tbs) (envs2bt env) }
                        ) 
                 proposeSimOut
             ; Just simout -> do return $ simout
             }
     }


-- ----------------------------------------------------------------------------------------- --
-- doSimIn act :  if  act = Act acts  then do the input (via tau-steps) else do nothing


doSimIn :: Action -> IOE Action

doSimIn act@(Act acts)  =  do
     curbtree     <- getBTree
     taubranches  <- return $ [ b | b@(BTtau _) <- curbtree ]
     inbranches   <- filterM (isInCTOffers.btoffers) [ b | b@(BTpref _ _ _ _) <- curbtree ]
     inafters     <- sequence [ liftP2 ( ib, afterBBranch acts ib ) | ib <- inbranches ]
     execbranches <- return $ [ ib | ( ib, aft ) <- inafters, not $ null aft ]
     case ( taubranches, execbranches ) of
     { ( [], [] ) -> do
         modify ( \env ->
                   env { envnrst = (envnrst env) + 1
                       , envcurs = (envnrst env)
                       , envtrie = (envcurs env, Act acts, envnrst env) : (envtrie env)
                       , envs2bt = Map.insert (envnrst env) curbtree (envs2bt env)
                       }
                )
         return $ act
     ; ( (tb:tbs), [] ) -> do
         random      <- lift $ randomRIO ( 0, length(tb:tbs) - 1 )
         BTtau btree <- return $ (tb:tbs)!!random
         modify ( \env ->
                   env { envs2bt = Map.insert (envcurs env) btree (envs2bt env) }
                )
         doSimIn act
     ; ( [], (eb:ebs) ) -> do
         random          <- lift $ randomRIO ( 0, length(eb:ebs) - 1 )
         (newbtree:nbts) <- afterBBranch acts ((eb:ebs)!!random)
         modify ( \env ->
                   env { envnrst = (envnrst env) + 1
                       , envcurs = (envnrst env)
                       , envtrie = (envcurs env, Act acts, envnrst env) : (envtrie env)
                       , envs2bt = Map.insert (envnrst env) newbtree (envs2bt env)
                       }
                )
         return $ act
     ; ( (tb:tbs), (eb:ebs) ) -> do
         random <- lift $ randomRIO ( 0, length(tb:tbs) + length(eb:ebs) - 1 )
         if random < length(tb:tbs)
           then do
             BTtau btree <- return $ (tb:tbs)!!random
             modify ( \env ->
                       env { envs2bt = Map.insert (envcurs env) btree (envs2bt env) }
                    )
             doSimIn act
           else do
             (newbtree:nbts) <- afterBBranch acts ((eb:ebs)!!(random - length(tb:tbs)))
             modify ( \env ->
                       env { envnrst = (envnrst env) + 1
                           , envcurs = (envnrst env)
                           , envtrie = (envcurs env, Act acts, envnrst env) : (envtrie env)
                           , envs2bt = Map.insert (envnrst env) newbtree (envs2bt env)
                           }
                    )
             return $ act
     }

doSimIn actQui  =  do
     return $ actQui


-- ----------------------------------------------------------------------------------------- --
-- doSimOut act :  if  act = actOut acts  then do the output (not via taus) else do nothing


doSimOut :: Action -> IOE Action
doSimOut act@(Act acts)  =  do
     curbtree      <- getBTree
     outbranches   <- filterM (isOutCTOffers.btoffers) [ b | b@(BTpref _ _ _ _) <- curbtree ]
     outafters     <- sequence [ liftP2 ( ob, afterBBranch acts ob ) | ob <- outbranches ]
     execbranches  <- return $ [ ob | ( ob, aft ) <- outafters, not $ null aft ]
     case execbranches of 
     { [] -> do
         mack "SIM" $ "empty out branches: should not occur"
         return $ act
     ; (eb:ebs) -> do
         random          <- lift $ randomRIO ( 0, length(eb:ebs) - 1 )
         (newbtree:nbts) <- afterBBranch acts ((eb:ebs)!!random)
         modify ( \env ->
                   env { envnrst = (envnrst env) + 1
                       , envcurs = (envnrst env)
                       , envtrie = (envcurs env, Act acts, envnrst env) : (envtrie env)
                       , envs2bt = Map.insert (envnrst env) newbtree (envs2bt env)
                       }
                )
         return $ act
     }

doSimOut act  =  do
     return $ act


-- ----------------------------------------------------------------------------------------- --
-- randMenuOut :  random output action from menuOut


ranMenuOut :: IOE (Maybe Action)
ranMenuOut  =  do
     menu     <- menuOut
     act      <- randMenu menu
     case act of
     { Nothing         -> return $ Nothing
     ; Just (Act acts) -> return $ Just (Act acts)
     }


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

