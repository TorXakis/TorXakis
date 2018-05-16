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

-- generic TorXakis imports
import BTree
import EnvBTree
import Expand
import Sigs
import TxsDefs
import Unfold
import VarId

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
    ("Stop",        testStop)
    ]

---------------------------------------------------------------------------
-- Tests
---------------------------------------------------------------------------

testStop :: IOB()
testStop = do
    let bnode :: BNode (WEnv VarId)
        bnode = BNbexpr Map.empty stop
    _bt <- unfold [] bnode
    -- modify ( \env -> env { envs2bt = Map.singleton 0 bt  } )
    -- actual <- isQui
    -- lift $ assertEqual "stop process" True actual
    
    next <- expand [] bnode
    lift $ assertEqual "expand stop" [] next
