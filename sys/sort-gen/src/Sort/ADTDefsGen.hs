-- |
module Sort.ADTDefsGen where

import           Control.Monad.State
import           Test.QuickCheck (Gen)
import           QuickCheck.GenT (listOf, GenT, runGenT, liftGen)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Name
import           Sort

import           GenState
import           NameGen
import           Sort.ConstructorDefsGen

arbitraryADTDefs :: Gen ADTDefs
arbitraryADTDefs = do
    ds <- arbitraryADTDefList
    let Right defs = addADTDefs ds emptyADTDefs
    return defs

arbitraryADTDefList :: Gen [ADTDef Name]
arbitraryADTDefList = do
    mas <- runGenT $ listOf arbitraryADTDef
    return $ evalState mas emptyState

arbitraryADTDef :: GenT (State GenState) (ADTDef Name)
arbitraryADTDef = do
    -- Get the available ADT's from the state.
    as <- adtNames <$> lift get
    n  <- liftGen $ arbitraryReadableName as
    cd <- arbitraryConstructorDefs n
    -- Clear the generated constructors.
    lift $ modify clearConstructors
    let a = ADTDef n cd
    -- Add the ADT to the set of constructed ADT's.
    lift $ modify (addADT n)
    return a
