-- |
module Sort.ADTDefsGen where

import           Control.Monad.State
import           Test.QuickCheck
import           QuickCheck.GenT
import           Data.Set (Set)
import qualified Data.Set as Set

import           Name
import           Sort

import           GenState
import           NameGen
import           Sort.ConstructorDefsGen

arbitraryADTDefList :: Gen [ADTDef Name]
arbitraryADTDefList = do
    mas <- runGenT $ QuickCheck.GenT.listOf arbitraryADTDefST
    return $ evalState mas emptyState

arbitraryADTDefST :: GenT (State GenState) (ADTDef Name)
arbitraryADTDefST = do
    -- Get the available ADT's from the state.
    as <- adtNames <$> lift get
    n  <- liftGen $ arbitraryReadableName as
    cd <- arbitraryConstructorDefs n
    let a = ADTDef n cd
    -- Add the ADT to the set of constructed ADT's.
    lift $ modify (addADT n)
    return a
