{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.ValExpr.ADTDef
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation functions related to 'TorXakis' Adt definitions.
--------------------------------------------------------------------------------
module TorXakis.Compiler.ValExpr.ADTDef
    (compileToADTDefs)
where

import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Text                        (Text)

import           TorXakis.Sort                    (Sort, 
                                                   ADTDef, mkADTDef,
                                                   ConstructorDef, mkConstructorDef,
                                                   FieldDef (FieldDef))

import           TorXakis.Compiler.Data           (CompilerM)
import           TorXakis.Compiler.MapsTo         (MapsTo, lookupM)
import           TorXakis.Parser.Data             (ADTDecl, adtName,
                                                   CstrDecl, cstrName,
                                                   FieldDecl, fieldName, fieldSort,
                                                   CstrE,
                                                   Loc, constructors,
                                                   cstrFields, getLoc)

-- | Compile a list of ADT declarations into a list of ADT Definitions
compileToADTDefs :: ( MapsTo Text        Sort mm )
                  => mm -> [ADTDecl] -> CompilerM [ADTDef]
compileToADTDefs mm ds =
    traverse (adtToADTDef mm) ds

-- | Compile an ADT declaration into a list constructor id's and
-- constructor definition pairs.
adtToADTDef :: ( MapsTo Text        Sort mm )
               => mm -> ADTDecl -> CompilerM ADTDef
adtToADTDef mm a =
    mkADTDef (adtName a) <$> traverse (cstrToADTDef mm) (constructors a)

-- | Compile a constructor declaration into a constructor definition
cstrToADTDef :: ( MapsTo Text        Sort mm )
               => mm -> CstrDecl -> CompilerM ConstructorDef
cstrToADTDef mm c =
    mkConstructorDef (cstrName c) <$> traverse (fieldToFieldDef mm) (cstrFields c)

fieldToFieldDef :: ( MapsTo Text        Sort mm )
               => mm -> FieldDecl -> CompilerM FieldDef
fieldToFieldDef mm f = do
    s <- lookup (fieldSort f) mm
    return $ FieldDef (fieldName f) s