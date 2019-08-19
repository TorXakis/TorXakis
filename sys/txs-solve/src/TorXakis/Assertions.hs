{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Assertions
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the assertions to be solved.
-----------------------------------------------------------------------------
module TorXakis.Assertions
( Assertions
, AssertionsView(..)
, view
, empty
, add
)
where

import qualified Data.Set  as Set

import           TorXakis.Error
import           TorXakis.Sort
import qualified TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.VarContext
-- ----------------------------------------------------------------------------------------- --
-- | View on Assertions.
data AssertionsView = AssertFalse
                    | AssertSet ( Set.Set TorXakis.ValExpr.ValExpression )
     deriving (Eq, Ord, Read, Show)

-- | Assertions type.
newtype Assertions = Assertions { -- | view of Assertions
                                  view :: AssertionsView }
     deriving (Eq, Ord, Read, Show)

-- | constructor for empty collection of assertions.
empty :: Assertions
empty = Assertions (AssertSet Set.empty)

-- | operator to add boolean value expression to assertions.
add :: VarContext c => c -> TorXakis.ValExpr.ValExpression -> Assertions -> Either Error Assertions
add c e a | getSort c e == SortBool = Right $ addView (view a)
          | otherwise               = Left $ Error ("Add - Can not add non-boolean expression to assertions: " ++ show e)
    where
        addView :: AssertionsView -> Assertions
        addView AssertFalse                                                                       = Assertions AssertFalse
        addView v             | TorXakis.ValExpr.view e == TorXakis.ValExpr.Vconst (Cbool True)   = Assertions v
        addView _             | TorXakis.ValExpr.view e == TorXakis.ValExpr.Vconst (Cbool False)  = Assertions AssertFalse
        addView (AssertSet s)                                                                     = Assertions ( AssertSet (Set.insert e s) )

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
