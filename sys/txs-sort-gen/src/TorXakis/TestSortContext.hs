{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.TestSortContext
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  pierre.vandelaar@tno.nl (ESI)
-- Stability   :  experimental
-- Portability :  portable
--
-- Sort Context for Test:
-- Additional functionality to ensure termination for QuickCheck
-----------------------------------------------------------------------------
module TorXakis.TestSortContext
(-- * Test Sort Context
  TestSortContext (..)
  -- dependencies, yet part of interface
, module TorXakis.SortContext
, ConstructorDef
, RefByName
)
where
import           TorXakis.Name
import           TorXakis.Sort
import           TorXakis.SortContext

-- | A TestSortContext instance contains all definitions to work with sort and reference thereof for test purposes
class SortContext c => TestSortContext c where
    -- | Sort Size
    --   The size of the provided 'TorXakis.Sort' is returned.
    --   The size is a measurement of complexity and is indicated by an 'Int'.
    --   Note that the function should crash when the context does not contain the 'TorXakis.Sort' reference.
    sortSize :: Sort -> c -> Int

    -- |  adt Size
    --   The size of the provided reference to 'TorXakis.ADTDef' is returned.
    --   The size is a measurement of complexity and is indicated by an 'Int'.
    --   Note that the function should crash when the context does not contain the 'TorXakis.ADTDef' and any related 'TorXakis.Sort' references.
    adtSize :: RefByName ADTDef -> c -> Int

    -- |  constructor Size
    --   The size of the provided constructor as specified by the references to 'TorXakis.ADTDef' and 'TorXakis.ConstructorDef' is returned.
    --   The size is a measurement of complexity and is indicated by an 'Int'.
    --   Note that the function should crash when the context does not contain the 'TorXakis.ADTDef', 'TorXakis.ConstructorDef' and any related 'TorXakis.Sort' references.
    constructorSize :: RefByName ADTDef -> RefByName ConstructorDef -> c -> Int
