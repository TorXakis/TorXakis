-- |

module Sort.ConstructorDefsGen where

import           Control.Monad.State
import           Test.QuickCheck (Gen)
import           QuickCheck.GenT (GenT, listOf, listOf1, liftGen, runGenT)
import           Control.Arrow   (second)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Name
import           Sort

import           GenState
import           NameGen
import           Sort.FieldDefsGen

arbitraryConstructorDefs :: Name -- ^ ADT that contains the constructor defs.
                         -> GenT (State GenState) (ConstructorDefs Name)
arbitraryConstructorDefs a = do
    -- The first list of constructors must contain at least one element, and
    -- may not use the type of the containing ADT.
    cs0 <- listOf1 $ arbitraryConstructorDef Nothing
    -- The second list of constructors may be empty and may use the containing
    -- ADT (with name 'a') as type for the fields.
    cs1 <- listOf $ arbitraryConstructorDef (Just a)
    -- Here we will get an error if the smart constructor return left, however
    -- this should be OK because:
    --
    -- - the generator should always return valid data.
    --
    -- - if there is an error in the generator we want to catch it a soon as
    --   possible.
    --
    -- - it makes it more convenient to work with generators.
    --
    let Right cd = constructorDefs (cs0 ++ cs1)
    -- Clear the field names.
    lift $ modify clearFields
    return cd

arbitraryConstructorDef :: Maybe Name -- ^ Maybe the ADT name that contains this constructor.
                        -> GenT (State GenState) (ConstructorDef Name)
arbitraryConstructorDef a = do
    cn <- lift $ gets constructorNames
    n  <- liftGen $ arbitraryReadableName cn
    fd <- arbitraryFieldDefs
    lift $ modify (addConstructor n)
    return $ ConstructorDef n fd

