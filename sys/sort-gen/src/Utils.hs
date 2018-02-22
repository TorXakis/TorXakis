-- | Utility functions.

module Utils where

import           Test.QuickCheck (Gen, generate)
import           QuickCheck.GenT (GenT, runGenT)
import           Control.Monad.State (State, evalState)

import           GenState (GenState, emptyState)
    
fromEmptyState :: GenT (State GenState) a -> IO a
fromEmptyState g = do
    stA <- generate $ runGenT g
    return $ evalState stA emptyState

