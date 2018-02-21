-- |

module Sort.ConstructorDefsGen where

import           Control.Monad.State
import           Test.QuickCheck
import           QuickCheck.GenT

import           Name
import           Sort


arbitraryConstructorDefList :: Name          -- ^ Name of the ADT to which this constructor belongs to.
                                             --   Might be useful to prevent ADT declarations like:
                                             --
                                             -- >  TYPEDEF Inf ::= Inf { x :: Inf }
                                             --
                            -> [ADTDef Name] -- ^ Set of available ADTs
                            -> Gen [ConstructorDef Name]
arbitraryConstructorDefList a as = do
    mcs <- runGenT $ QuickCheck.GenT.listOf arbitraryConstructorDefST
    -- TODO: with certain probability generate a list of constructors that use
    -- the ADT name of the ADT that contains this constructor. Or, check
    -- whether the list of constructors contains only one constructor with only
    -- one field whose type is 'a', i.e. we want to prevent:
    --
    -- > TYPEDEF X ::= SomeStr { someField :: X }
    --
    let check = do
            cs <- mcs
            if typeIsInfinite a cs
               then lift $ arbitraryConstructorDefList a as
               else return cs
    return $ evalState check (as, [])

typeIsInfinite :: Name -> [ConstructorDef Name] -> Bool
typeIsInfinite = undefined
    
arbitraryConstructorDefST :: GenT (State ([ADTDef Name], [ConstructorDef Name])) (ConstructorDef Name)
-- TODO: document the state of this monad. Why do we use these elements? What do they represent?
arbitraryConstructorDefST = undefined
