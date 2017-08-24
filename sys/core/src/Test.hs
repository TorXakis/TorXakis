{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE RecordWildCards #-}

-- ----------------------------------------------------------------------------------------- --

module Test

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- Running an IOCO Test
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
-- export

( testIn
, testOut
, testN
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import System.Random
import Control.Monad.State

-- local
import Ioco
import Mapper
import Purpose
import CoreUtils

-- import from coreeenv
import qualified EnvCore     as IOC
import qualified ParamCore
import qualified EnvData

import qualified TxsDefs
import qualified TxsDDefs
import qualified TxsShow
import BTShow()

-- ----------------------------------------------------------------------------------------- --
-- testIn :  try to give input (Act acts), and give new environment
--        :  result is whether input was successful, or faster output was successful/conforming


testIn :: TxsDDefs.Action -> Int -> IOC.IOC (TxsDDefs.Action, TxsDDefs.Verdict)
testIn act@TxsDDefs.Act{} step  =  do
     putToW <- gets (IOC.puttow . IOC.state)
     mact   <- mapperMap act                                -- map action
     mact'  <- putToW mact                                  -- try to do input on sut
     if mact == mact'
       then do                                              -- input done on sut
         IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                       $ (TxsShow.showN step 6) ++ ":  IN:  " ++ (TxsShow.fshow act) ]
         done <- iocoModelAfter act
         if  done
           then do return $ (act,  TxsDDefs.Pass)
           else do IOC.putMsgs [ EnvData.TXS_CORE_USER_ERROR
                                 $ "proposed input not possible on model" ]
                   return $ (act, TxsDDefs.Fail act)        -- input done on sut, not on btree
       else do                                              -- output was faster
         act' <- mapperMap mact'                            -- map output to model action
         IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                       $ (TxsShow.showN step 6) ++ ":  OUT: " ++ (TxsShow.fshow act') ]
         done <- iocoModelAfter act
         if  done
           then do return $ (act', TxsDDefs.Pass)           -- output act' `Elem` menuOut
           else do expected
                   return $ (act', TxsDDefs.Fail act')      -- output act' `notElem` menuOut
 
testIn TxsDDefs.ActQui _  =  do
     IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                   $ "testIn cannot be done with Quiescence" ]
     return $ (TxsDDefs.ActQui, TxsDDefs.Fail TxsDDefs.ActQui)


-- ----------------------------------------------------------------------------------------- --
-- testOut :  observe output, and give new environment
--         :  result is whether output was successful, ie. conforming


testOut :: Int -> IOC.IOC (TxsDDefs.Action, TxsDDefs.Verdict)
testOut step  =  do
     getFroW <- gets (IOC.getfrow . IOC.state)
     mact    <- getFroW                                     -- get next output or quiescence
     act     <- mapperMap mact                              -- map output to model action
     IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                   $ (TxsShow.showN step 6) ++ ":  OUT: " ++ (TxsShow.fshow act) ]
     done <- iocoModelAfter act
     if  done
       then do return $ (act, TxsDDefs.Pass)                -- output act `elem` menuOut
       else do expected
               return $ (act, TxsDDefs.Fail act)            -- output act `notElem` menuOut


-- ----------------------------------------------------------------------------------------- --
-- expected :  expected outputs after fail


expected :: IOC.IOC ()
expected  =  do
     menu <- iocoModelMenuOut
     qui  <- iocoModelIsQui
     IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO $ "Expected:" ]
     IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO $ TxsShow.fshow menu ]
     IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO $ if qui then "No Output (Quiescence)" else "" ]


-- ----------------------------------------------------------------------------------------- --
-- testN :  make 'depth' random test steps from current btree, and give new environment


testN :: Int -> Int -> IOC.IOC TxsDDefs.Verdict
testN depth step = do
  [(_, parval)] <- IOC.getParams ["param_ImpRel"]
  envc          <- get
  case (read parval, IOC.state envc) of
    (ParamCore.IOCO, IOC.Testing {IOC.purpdef = purpdef}) -> continue purpdef
    (_, _) -> do  -- something went wrong --
      IOC.putMsgs [EnvData.TXS_CORE_SYSTEM_ERROR $ "testing could not start"]
      return TxsDDefs.NoVerdict
  where
    -- No test purpose.    
    continue Nothing =
      testIOCO depth False step 
    continue (Just (TxsDefs.PurpDef [] [] _ _)) =
      testIOCO depth False step
    -- Test purpose with only outputs.    
    continue (Just (TxsDefs.PurpDef [] _ _ _)) =
      testIOCOoutPurp depth False step
    -- Test purpose with only inputs.      
    continue (Just (TxsDefs.PurpDef _ [] _ _)) =
      testIOCOinPurp depth False step
    -- Test purpose with inputs and outputs.      
    continue (Just (TxsDefs.PurpDef _ _ _ _)) =
      testIOCOfullPurp depth False step


-- ----------------------------------------------------------------------------------------- --
-- testIOCO :  IOCO test run;  WHAT TO DO WITH EAGERNESS NOW !!!???
-- eag: input eagerness:
-- 0: standard ioco
-- 1: eager input, i.e. do always input if no output expected
-- 2: super eager input, i.e., do always input if delta can be expected,
--                       i.e., no output check if delta in outputs
-- 3: extra super eager input, i.e., do always input if input specified

-- parameters 
-- ioRand dependent on eag


testIOCO :: Int -> Bool -> Int -> IOC.IOC TxsDDefs.Verdict
testIOCO depth lastDelta step  =  do
     if  depth == 0
       then do
         return $ TxsDDefs.Pass
       else do
         ioRand   <- lift $ randomRIO (False,True)                  -- random for in- or output
         modMenu  <- iocoModelMenuIn
         input    <- randMenu modMenu
         [(_,parval)] <- IOC.getParams ["param_Test_inputEager"]
         iochoice <- return $ case (read parval::Integer) of                 -- input (True) or output
                              { 0 -> ( input /= Nothing ) && ( lastDelta || ioRand )
                              ; 1 -> error $ "TXS: undefined input-eagerness level\n"
                              ; 2 -> error $ "TXS: undefined input-eagerness level\n"
                              ; 3 -> ( input /= Nothing )
                              ; _ -> error $ "TXS: undefined input-eagerness level\n"
                              }
         if  iochoice
           then do                                                -- try input, input/=Nothing
             Just inp      <- return $ input
             (act,verdict) <- testIn inp step
             case verdict of
             { TxsDDefs.Pass      -> do testIOCO (depth-1) (act==TxsDDefs.ActQui) (step+1)
             ; TxsDDefs.Fail act' -> do return $ TxsDDefs.Fail act'
             ; _                  -> error "testIOCO - NoVerdict"
             }
           else do
             if  (not lastDelta)
               then do                                            -- observe output
                 (act,verdict) <- testOut step
                 case verdict of
                 { TxsDDefs.Pass      -> do testIOCO (depth-1) (act==TxsDDefs.ActQui) (step+1)
                 ; TxsDDefs.Fail act' -> do return $ TxsDDefs.Fail act'
                 ; _                  -> error "testIOCO - NoVerdict"
                 }
               else do                                            -- lastDelta and no inputs
                 IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO $ "no more actions" ]
                 return $ TxsDDefs.Pass


-- ----------------------------------------------------------------------------------------- --
-- testing with test purposes
{-
-- | testPin :  try to give input (Act acts), used with test purpose
testPin :: TxsDDefs.Action -> Int -> IOC.IOC TxsDDefs.Action
testPin act@(TxsDDefs.Act acts) step  =  do
     putToW                      <- gets (IOC.puttow . IOC.state)
     mact @(TxsDDefs.Act macts ) <- mapperMap act
     mact'@(TxsDDefs.Act macts') <- putToW mact             -- do input on sut, always
     if mact == mact'
       then do                                              -- input done on sut
         IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                       $ (TxsShow.showN step 6) ++ ":  IN:  " ++ (TxsShow.fshow mact) ]
         return $ mact                                      -- input done on sut, not on btree
       else do                                              -- output was faster
         IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                       $ (TxsShow.showN step 6) ++ ":  OUT: " ++ (TxsShow.fshow mact') ]
         return $ mact'                                     -- output `notElem` menuOut


testPin TxsDDefs.ActQui step  =  do                                   -- otherwise
     IOC.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                   $ "testIn can only be called with (TxsDDefs.Act acts)" ]
     return $ TxsDDefs.ActQui
-}

{-
-- | testPout :  observe output, for use with test purposes
testPout :: Int -> IOC.IOC TxsDDefs.Action
testPout step  =  do
     getFroW <- gets (IOC.getfrow . IOC.state)
     act     <- getFroW                                      -- get next output or quiescence
     mact    <- mapperMap act
     IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
               $ (TxsShow.showN step 6) ++ ":  OUT: " ++ (TxsShow.fshow mact) ]
     return $ mact
-}

-- | testIOCOinPurp :  test with test puposes, only on inputs

testIOCOinPurp :: Int -> Bool -> Int -> IOC.IOC TxsDDefs.Verdict
testIOCOinPurp _ _ _  =  do
       IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                     $ "test purpose with only inputs not supported yet" ]
       return $ TxsDDefs.NoVerdict

-- ----------------------------------------------------------------------------------------- --
-- testIOCOoutPurp :  test with test puposes, only on outputs


testIOCOoutPurp :: Int -> Bool -> Int -> IOC.IOC TxsDDefs.Verdict
testIOCOoutPurp _ _ _  =  do
       IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO
                     $ "test purpose with only outputs not supported yet" ]
       return $ TxsDDefs.NoVerdict

-- ----------------------------------------------------------------------------------------- --
-- testIOCOfullPurp :  test with test puposes, on inputs and outputs


testIOCOfullPurp :: Int -> Bool -> Int -> IOC.IOC TxsDDefs.Verdict
testIOCOfullPurp depth lastDelta step  =  do
     if  depth == 0
       then do
         return $ TxsDDefs.Pass
       else do
         ioRand    <- lift $ randomRIO (False,True)                 -- random for in- or output
         modMenu   <- iocoModelMenuIn
         purpMenus <- purpMenusIn
         -- lift $ hPutStrLn stderr $ "\n***modMenu: " ++ (TxsShow.fshow modMenu)
         -- lift $ hPutStrLn stderr $ "\n***purpMenus: " ++ (TxsShow.fshow purpMenus)
         input     <- randPurpMenu modMenu purpMenus
         -- lift $ hPutStrLn stderr $ "\n***input: " ++ (TxsShow.fshow input)
         iochoice <- return $ (input /= Nothing) && (lastDelta || ioRand)
         -- lift $ hPutStrLn stderr $ "\n***iochoice: " ++ (show iochoice)
         if  iochoice
           then do                                                  -- try input, input/=Nothing
             Just inp       <- return $ input
             -- lift $ hPutStrLn stderr $ "\n***inp: " ++ (show inp)
             (act, verdict) <- testIn inp step                      -- act can input or output
             purpReady      <- purpAfter act
             nextBehTrie act
             -- lift $ hPutStrLn stderr $ "\n***purpReady: " ++ (show purpReady)
             case (verdict, purpReady) of
             { (TxsDDefs.Pass    , False) -> do testIOCOfullPurp
                                                      (depth-1) (act==TxsDDefs.ActQui) (step+1)
             ; (TxsDDefs.Pass    , True ) -> do purpVerdict 
                                                return $ TxsDDefs.Pass
             ; (TxsDDefs.Fail act', _   ) -> do purpVerdict
                                                expected
                                                return $ TxsDDefs.Fail act'
             ; _                          -> error "testIOCOfullPurp - should not happen"
             }
           else do
             if  (not lastDelta)
               then do                                              -- observe output
                 (act,verdict) <- testOut step
                 purpReady     <- purpAfter act
                 nextBehTrie act
                 case (verdict, purpReady) of
                 { (TxsDDefs.Pass    , False) -> do testIOCOfullPurp
                                                      (depth-1) (act==TxsDDefs.ActQui) (step+1)
                 ; (TxsDDefs.Pass    , True ) -> do purpVerdict
                                                    return $ TxsDDefs.Pass
                 ; (TxsDDefs.Fail act', _   ) -> do purpVerdict
                                                    expected
                                                    return $ TxsDDefs.Fail act'
                 ; _                          -> error "testIOCOfullPurp - should not happen"
                 }
               else do                                              -- lastDelta and no inputs
                 IOC.putMsgs [ EnvData.TXS_CORE_USER_INFO $ "no more actions" ]
                 purpVerdict
                 return $ TxsDDefs.Pass


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
