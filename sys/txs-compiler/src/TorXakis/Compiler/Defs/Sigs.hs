{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.Defs.Sigs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation functions related to 'TorXakis' signatures.
--------------------------------------------------------------------------------
module TorXakis.Compiler.Defs.Sigs
    ( adtDeclsToSigs
    , funDeclsToSigs
    , sortsToSigs
    )
where

import           Data.Map                          (Map)
import           Data.Text                         (Text)

import           CstrId                            (CstrId)
import           FuncId                            (FuncId)
import           Sigs                              (Sigs, empty, func, sort)
import           SortId                            (SortId)
import           VarId                             (VarId)

import           TorXakis.Compiler.Data            (CompilerM)
import           TorXakis.Compiler.Defs.FuncTable  (adtsToFuncTable,
                                                    funcDeclsToFuncTable)
import           TorXakis.Compiler.MapsTo          (MapsTo)
import           TorXakis.Compiler.ValExpr.FuncDef (FuncDefInfo)
import           TorXakis.Parser.Data              (ADTDecl, CstrE, FuncDecl,
                                                    FuncDeclE, Loc)

-- | Compile ADT declarations into 'TorXakis' signatures.
adtDeclsToSigs :: ( MapsTo Text SortId mm
                  , MapsTo (Loc CstrE) CstrId mm )
               => mm -> [ADTDecl] -> CompilerM (Sigs VarId)
adtDeclsToSigs mm ds = do
    ft <- adtsToFuncTable mm ds
    return $ empty { func = ft }

-- | Compile function declarations 'TorXakis' signatures.
funDeclsToSigs :: ( MapsTo Text SortId mm
                  , MapsTo (Loc FuncDeclE) FuncId mm
                  , MapsTo FuncId FuncDefInfo mm )
               => mm -> [FuncDecl] -> CompilerM (Sigs VarId)
funDeclsToSigs mm ds = do
    ft <- funcDeclsToFuncTable mm ds
    return $ empty { func = ft }

-- | Compile a map from 'Text' to @SortId@ into 'TorXakis' signatures.
sortsToSigs :: Map Text SortId -> Sigs VarId
sortsToSigs sm = empty { sort = sm }
