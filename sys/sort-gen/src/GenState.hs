-- | State of the generator

module GenState where

import           Data.Set (Set)
import qualified Data.Set as Set

import           Name

data GenState = GenState
    { adtNames         :: Set Name
    , constructorNames :: Set Name
    , fieldNames       :: Set Name
    }

emptyState :: GenState
emptyState = GenState Set.empty Set.empty Set.empty

addADT :: Name -> GenState -> GenState
addADT n GenState{adtNames = ns} = GenState { adtNames = Set.insert n ns }

addConstructor :: Name -> GenState -> GenState
addConstructor = undefined

addField :: Name -> GenState -> GenState
addField = undefined

clearADTs :: GenState -> GenState
clearADTs = undefined

clearConstructors :: GenState -> GenState
clearConstructors = undefined

clearFields :: GenState -> GenState
clearFields = undefined
