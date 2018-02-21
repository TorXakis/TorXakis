-- | 

module Sort.ADTDefsGen where

import           Control.Monad.State
import           Test.QuickCheck
import           QuickCheck.GenT

import           Name
import           Sort

import           NameGen
import           Sort.ConstructorDefsGen

arbitraryADTDefList :: Gen [ADTDef Name]
arbitraryADTDefList = do
    mas <- runGenT $ QuickCheck.GenT.listOf arbitraryADTDefST
    return $ evalState mas []

arbitraryADTDefST :: GenT (State [ADTDef Name]) (ADTDef Name)
-- TODO: replace the list of generated ADT's by a **set** of generated ADT's.
arbitraryADTDefST = do
    as <- lift get  -- Get the available ADT's from the state.
    ArbitraryName n <- liftGen arbitrary
    if n `elem` (getName <$> as)
        then arbitraryADTDefST -- Try again, by generating a different name.
        else do
            cs <- liftGen $ arbitraryConstructorDefList n as
            let a = ADTDef n undefined
            lift $ put (a:as) -- Add the ADT to the list of constructed ADT's.
            return a
