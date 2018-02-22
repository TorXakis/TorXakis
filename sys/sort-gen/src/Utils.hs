-- | Utility functions.

module Utils where

import           Test.QuickCheck (Gen, generate)
import           QuickCheck.GenT (GenT, runGenT, resize)
import           Control.Monad.State (State, evalState)

import           GenState (GenState, emptyState)

fromEmptyState = fromEmptyStateWithSize 4
    
fromEmptyStateWithSize :: Int -> GenT (State GenState) a -> IO a
fromEmptyStateWithSize n g = do
    stA <- generate $ runGenT (resize n g)
    return $ evalState stA emptyState

