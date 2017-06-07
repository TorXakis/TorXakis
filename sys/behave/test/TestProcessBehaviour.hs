{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

module TestProcessBehaviour
(
testProcessBehaviourList
)
where
-- test specific Haskell imports
-- import qualified Debug.Trace as Trace
import Test.HUnit

-- test specific TorXakis imports


-- generic Haskell imports
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

-- generic TorXakis imports
import CTree
import Expand
import Primer
import Reduce
import StdTDefs
import TxsDefs
import TxsEnv
import Unfold

-- ----------------------------------------------------------------------------    
testProcessBehaviourList  = TestList $ map (\e -> TestLabel (fst e) $ TestCase $ evalStateT (snd e) envIdle ) ioeTestList

ioeTestList = [
    ("Stop",        testStop),
    -- ("Exit",        testExit),
    ("Guard False", testGuardFalse),
    ("Guard True",  testGuardTrue),
    ("Choice",      testChoice)
    ]

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------
testStop :: IOE()
testStop = do
    let bnode = BNbexpr Map.empty Stop
    btree <- unfold bnode
    modify ( \env -> env { envs2bt = Map.singleton 0 btree  } )
    actual <- isQui
    lift $ assertEqual "stop process" True actual
    
    next <- expand bnode
    lift $ assertEqual "expand exit" [] next
    
testExit :: IOE()
testExit = do
    let bnode = BNbexpr Map.empty (ActionPref (ActOffer (Set.singleton (Offer chanId_Exit []) ) [] ) Stop )
    next <- expand bnode
    lift $ assertEqual "expand exit" [] next

testGuardFalse :: IOE()
testGuardFalse = do
    let bnode = BNbexpr Map.empty (Guard [ cstrConst (Cbool False) ] Stop )
    next <- expand bnode
    lift $ assertEqual "expand guard false" [] next

testGuardTrue :: IOE()
testGuardTrue = do
    let aBExpr = ActionPref (ActOffer (Set.singleton (Offer chanId_Exit []) ) [] ) Stop   
    let bnode = BNbexpr Map.empty aBExpr
    nextExpected <- expand bnode
    
    let bnodeGuard = BNbexpr Map.empty (Guard [ cstrConst (Cbool True) ] aBExpr )
    nextActual <- expand bnodeGuard
    lift $ assertEqual "expand guard true" nextExpected nextActual
    
testChoice :: IOE()
testChoice = do
    let aBExpr = ActionPref (ActOffer (Set.singleton (Offer chanId_Exit []) ) [] ) Stop    
    let bnode = BNbexpr Map.empty aBExpr
    nextExpected <- unfold bnode
    
    let bnodeChoice = BNbexpr Map.empty (Choice [ aBExpr, aBExpr ])
    nextActual <- unfold bnodeChoice
    
    lift $ assertEqual "unfold choice" nextExpected nextActual
