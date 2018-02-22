-- | State of the generator

module GenState where

import           Data.Set (Set)
import qualified Data.Set as Set

import           Sort
import           Name

data GenState = GenState
    { adtNames         :: !(Set Name)
    , constructorNames :: !(Set Name)
    , fieldNames       :: !(Set Name)
    }

primitiveSortNamesSet :: Set Name
primitiveSortNamesSet = Set.fromList primitiveSortNames

emptyState :: GenState
emptyState = GenState primitiveSortNamesSet Set.empty Set.empty

-- TODO: stop this madness and start using lenses.
addADT :: Name -> GenState -> GenState
addADT n s = s {adtNames = Set.insert n (adtNames s)}

addConstructor :: Name -> GenState -> GenState
addConstructor n s = s {constructorNames = Set.insert n (constructorNames s)}

addField :: Name -> GenState -> GenState
addField n s = s {fieldNames = Set.insert n (fieldNames s)}

clearADTs :: GenState -> GenState
clearADTs s = s{ adtNames = primitiveSortNamesSet }

clearConstructors :: GenState -> GenState
clearConstructors s = s{ constructorNames = Set.empty }

clearFields :: GenState -> GenState
clearFields s = s{ fieldNames = Set.empty }

