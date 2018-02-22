-- |

module Sort.ConstructorDefsGen where

import           Control.Monad.State
import           Test.QuickCheck (Gen)
import           QuickCheck.GenT (GenT, listOf, listOf1)

import           Name
import           Sort

import           Sort.FieldDefsGen

arbitraryConstructorDefList :: Name          -- ^ Name of the ADT to which this constructor belongs to.
                                             --   Might be useful to prevent ADT declarations like:
                                             --
                                             -- >  TYPEDEF Inf ::= Inf { x :: Inf }
                                             --
                            -> [ADTDef Name] -- ^ Set of available ADTs
                            -> Gen [ConstructorDef Name]
arbitraryConstructorDefList a as = undefined
    -- do
    -- mcs <- runGenT $ QuickCheck.GenT.listOf arbitraryConstructorDefST
    -- return $ evalState mcs (a, as, [])

arbitraryConstructorDefsST :: Name   -- ^ ADT that contains the constructor defs.
                           -> [Name] -- ^ Available types.
                           -> GenT (State [ConstructorDef Name]) (ConstructorDefs Name)
-- TODO: document the state of this monad. Why do we use these elements? What
-- do they represent?
arbitraryConstructorDefsST a as = do
    -- The first list of constructors must contain at least one element, and
    -- may not use the type of the containing ADT.
    cs0 <- listOf1 $ arbitraryConstructorDef as
    -- The second list of constructors may be empty and may use the containing
    -- ADT (with name 'a') as type for the fields.
    cs1 <- listOf  $ arbitraryConstructorDef (a:as)
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
    let Right cds = constructorDefs (cs0 ++ cs1)
    return cds
    

arbitraryConstructorDef :: [Name] -- ^ Name of the types that are available as
                                  -- types to be used in the fields.
                        -> GenT (State [ConstructorDef Name]) (ConstructorDef Name)
arbitraryConstructorDef = undefined
