{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

module Sort.FieldDefsGen where

import           Control.Monad.State
import           QuickCheck.GenT (GenT, listOf, liftGen, elements)
import qualified Data.Set as Set

import           Name
import           Sort

import           GenState
import           NameGen

arbitraryFieldDefs :: GenT (State GenState) (FieldDefs Name)
arbitraryFieldDefs = do
    fs <- listOf arbitraryFieldDef
    let Right fd = fieldDefs fs
    return fd
    
arbitraryFieldDef :: GenT (State GenState) (FieldDef Name)
arbitraryFieldDef = do
    fs <- lift $ gets fieldNames
    n  <- liftGen $ arbitraryReadableName fs
    ts <- lift $ gets adtNames
    t  <- elements (Set.toList ts)
    let f = FieldDef n t "" -- WARNING: we don't generate metadata yet.
    lift $ modify (addField n)
    return f
