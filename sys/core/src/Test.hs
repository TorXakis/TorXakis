{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --

module Test

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- Running an IOCO Test
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
-- export

( testOut  --   testOut :: Handle -> IOE ()
, testIn   --   testIn :: Action -> Handle -> IOE ()
, testN    --   testN :: Int -> Handle -> IOE ()
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import System.IO
import System.Random
import Control.Monad.State
import Debug.Trace

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Data.Map  as Map

import Params
import TxsDefs
import TxsDDefs
import TxsEnv
import Eval
import Utils
import TxsShow
import Cnect
import Primer
import Step
import CTree

import SolveDefs
import Solve

import CTShow
import ServerIf


-- ----------------------------------------------------------------------------------------- --
-- testOut :  observe output, and give new environment
--         :  result is whether output was successful, ie. conforming


testOut :: Int -> IOE Verdict
testOut step  =  do
     actout <- getFroW                                       -- get next output or quiescence
     pack "TEST" $ (showN step 5) ++ ":  OUT: " ++ (fshow actout)
     pass   <- doAfter actout                                -- next btree after output
     if  pass
       then do return $ Pass                                 -- output `elem` menuOut
       else do expected
               return $ Fail actout                          -- output `notElem` menuOut


-- ----------------------------------------------------------------------------------------- --
-- testIn :  try to give input Act acts, and give new environment
--        :  result is whether input was successful, or faster output was successful/conforming


testIn :: Action -> Int -> IOE Verdict
testIn act@(Act acts) step  =  do
     act' <- putToW act                                     -- do input on sut, always
     if act == act'
       then do                                              -- input done on sut
         pack "TEST" $ (showN step 5) ++ ":  IN:  " ++ (fshow act)
         pass <- doAfter act
         if  pass
           then do return $ Pass                            -- input done on sut and on btree
           else do expected
                   return $ Fail act                        -- input done on sut, not on btree
       else do                                              -- output was faster
         pack "TEST" $ (showN step 5) ++ ":  OUT: " ++ (fshow act')
         pass <- doAfter act'
         if  pass
           then do return $ Pass                            -- output `Elem` menuOut
           else do expected
                   return $ Fail act'                       -- output `notElem` menuOut
    
testIn act step  =  do                                      -- otherwise
     mack "TEST" $ "testIn only called with Act acts"
     return $ Fail act


-- ----------------------------------------------------------------------------------------- --
-- testN :  make 'depth' random test steps from current btree, and give new environment


testN :: Int -> Int -> IOE Verdict
testN depth step  =  do
     param_ImpRel <- getParam "param_ImpRel"
     case (read param_ImpRel) of
     { IOCO -> do testIOCO depth False step
     }


-- ----------------------------------------------------------------------------------------- --
-- testIOCO :  IOCO test run;
-- eag: input eagerness:
-- 0: standard ioco
-- 1: eager input, i.e. do always input if no output expected
-- 2: super eager input, i.e., do always input if delta can be expected,
--                       i.e., no output check if delta in outputs
-- 3: extra super eager input, i.e., do always input if input specified

-- parameters 
-- ioRand dependent on eag


testIOCO :: Int -> Bool -> Int -> IOE Verdict
testIOCO depth lastDelta step  =  do
     if  depth == 0
       then do
         return $ Pass
       else do
         ioRand   <- lift $ randomRIO (False,True)                  -- random for in- or output
         input    <- ranMenuIn                                      -- random input action
         param_Test_inputEager <- getParam "param_Test_inputEager"
         iochoice <- return $ case (read param_Test_inputEager) of  -- input (True) or output
                              { 0 -> ( input /= Nothing ) && ( lastDelta || ioRand )
                              ; 1 -> error $ "TXS: undefined input-eagerness level\n"
                              ; 2 -> error $ "TXS: undefined input-eagerness level\n"
                              ; 3 -> ( input /= Nothing )
                              ; _ -> error $ "TXS: undefined input-eagerness level\n"
                              }
         if  iochoice
           then do                                                -- try input, input/=Nothing
             Just inp <- return $ input
             verdict  <- testIn inp step
             case verdict of
             { Pass     -> do testIOCO (depth-1) False (step+1)
             ; Fail act -> do return $ Fail act
             }
           else do
             if  (not lastDelta)
               then do                                            -- observe output
                 verdict <- testOut step
                 case verdict of
                 { Pass     -> do testIOCO (depth-1) False (step+1)
                 ; Fail act -> do return $ Fail act
                 }
               else do                                            -- lastDelta and no inputs
                 pack "TEST" $ "no more actions"
                 return $ Pass


-- ----------------------------------------------------------------------------------------- --
-- randMenuIn  :  random input action from menuIn


expected :: IOE ()
expected  =  do
     menu  <- menuOut
     delta <- isQui
     pack "TEST" $ "Expected:\n"
     pack "TEST" $ (fshow menu)
     pack "TEST" $ if delta then "No Output (Quiescence)" else ""
     

-- ----------------------------------------------------------------------------------------- --
-- randMenuIn  :  random input action from menuIn


ranMenuIn :: IOE (Maybe Action)
ranMenuIn  =  do
     menu     <- menuIn
     randMenu menu

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

