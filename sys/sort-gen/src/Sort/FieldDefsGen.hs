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

arbitraryFieldDefs :: Maybe Name -- ^ Whether we can use the name of the ADT
                                 -- that contains this fields as type.
                   -> GenT (State GenState) (FieldDefs Name)
arbitraryFieldDefs mn = do
    fs <- listOf $ arbitraryFieldDef mn
    let Right fd = fieldDefs fs
    return fd
    
arbitraryFieldDef :: Maybe Name
                  -> GenT (State GenState) (FieldDef Name)
arbitraryFieldDef mn = do
    fs <- lift $ gets fieldNames
    n  <- liftGen $ arbitraryReadableName fs
    ts <- lift $ gets adtNames
    let ts' = Set.toList ts
    t  <- elements $ maybe ts' (:ts') mn
    let f = FieldDef n t "" -- WARNING: we don't generate metadata yet.
    lift $ modify (addField n)
    return f
