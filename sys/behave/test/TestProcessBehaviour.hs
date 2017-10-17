{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module TestProcessBehaviour
(
testProcessBehaviourList
)
where
-- test specific Haskell imports
import Test.HUnit

-- test specific TorXakis imports


-- generic Haskell imports
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

-- generic TorXakis imports
import BTree
import EnvBTree
import Expand
import StdTDefs
import TxsDefs
import Unfold
import Sigs

-- ----------------------------------------------------------------------------
-- additional variables

envbIdle     :: EnvB
envbIdle     =  EnvB { EnvBTree.smts     = Map.empty
                     , EnvBTree.tdefs    = TxsDefs.empty
                     , EnvBTree.sigs     = Sigs.empty
                     , EnvBTree.stateid  = -1
                     , EnvBTree.params   = Map.empty
                                -- Map.union Params.initParams SolveDefs.Params.initParams ???
                     , EnvBTree.unid     = 1000
                     , EnvBTree.msgs     = []
                     }

-- ----------------------------------------------------------------------------
testProcessBehaviourList :: Test
testProcessBehaviourList  = TestList $ map (\e -> TestLabel (fst e) $ TestCase $ evalStateT (snd e) envbIdle ) ioeTestList

ioeTestList :: [(String, IOB())]
ioeTestList = [
    ("Stop",        testStop),
    ("Guard False", testGuardFalse),
    ("Guard True",  testGuardTrue),
    ("Choice",      testChoice)
    ]

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------

testStop :: IOB()
testStop = do
    let bnode = BNbexpr Map.empty Stop
    _bt <- unfold [] bnode
    -- modify ( \env -> env { envs2bt = Map.singleton 0 bt  } )
    -- actual <- isQui
    -- lift $ assertEqual "stop process" True actual
    
    next <- expand [] bnode
    lift $ assertEqual "expand stop" [] next

testGuardFalse :: IOB()
testGuardFalse = do
    let bnode = BNbexpr Map.empty (Guard (cstrConst (Cbool False)) Stop )
    next <- expand [] bnode
    lift $ assertEqual "expand guard false" [] next

testGuardTrue :: IOB()
testGuardTrue = do
    let aBExpr = ActionPref (ActOffer (Set.singleton (Offer chanId_Exit []) ) (cstrConst (Cbool True)) ) Stop
    let bnode = BNbexpr Map.empty aBExpr
    nextExpected <- expand [] bnode
    
    let bnodeGuard = BNbexpr Map.empty (Guard (cstrConst (Cbool True)) aBExpr )
    nextActual <- expand [] bnodeGuard
    lift $ assertEqual "expand guard true" nextExpected nextActual
    
testChoice :: IOB()
testChoice = do
    let aBExpr = ActionPref (ActOffer (Set.singleton (Offer chanId_Exit []) ) (cstrConst (Cbool True)) ) Stop
    let bnode = BNbexpr Map.empty aBExpr
    nextExpected <- unfold [] bnode
    
    let bnodeChoice = BNbexpr Map.empty (Choice [ aBExpr, aBExpr ])
    nextActual <- unfold [] bnodeChoice
    
    lift $ assertEqual "unfold choice" nextExpected nextActual
