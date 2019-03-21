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
    ( compileToADTDefs
    , compileToSorts)
where

import           Control.Monad.Except              (throwError)
import           Data.Text                         (Text, pack, concat)
import qualified Data.Map                          as Map

import           TorXakis.Name
import           TorXakis.Sort                     (Sort (SortADT),
                                                    ADTDef, mkADTDef,
                                                    ConstructorDef, mkConstructorDef,
                                                    FieldDef (FieldDef))
import           TorXakis.Compiler.Data            (CompilerM)
import           TorXakis.Compiler.Error           (Error (..), ErrorType (..), getErrorLoc)
import           TorXakis.Compiler.MapsTo          (MapsTo, lookupM)
import           TorXakis.Parser.Data              (ADTDecl, adtName,
                                                    CstrDecl, cstrName,
                                                    FieldDecl, fieldName, fieldSort,
                                                    constructors,
                                                    cstrFields)

toAdtName :: ADTDecl -> CompilerM Name
toAdtName a = case mkName (adtName a) of
                   Left e -> throwError $ Error InvalidExpression
                                                (getErrorLoc a)
                                                ( Data.Text.concat [ pack "Unable to make adt name from "
                                                                   , adtName a
                                                                   , pack (" due to " ++ show e)
                                                                   ] )
                   Right n -> return n

-- | Compile a map of Text to 'TorXakis.Sort' for the list of ADT declarations.
compileToSorts :: [ADTDecl]
                 -> CompilerM (Map.Map Text Sort)
compileToSorts ds = do
                    l <- mapM compileToSort ds
                    return $ Map.fromList l
    where
        compileToSort :: ADTDecl -> CompilerM (Text, Sort)
        compileToSort a = do
            n <- toAdtName a
            return (adtName a, SortADT (RefByName n))
                            
-- | Compile a list of ADT declarations into a list of 'TorXakis.ADTDef's.
compileToADTDefs :: MapsTo Text Sort mm
                 => mm
                 -> [ADTDecl]
                 -> CompilerM [ADTDef]
compileToADTDefs mm ds =
    traverse (adtToADTDef mm) ds

-- | Compile an ADT declaration into an 'TorXakis.ADTDef'.
adtToADTDef :: MapsTo Text Sort mm
            => mm
            -> ADTDecl
            -> CompilerM ADTDef
adtToADTDef mm a = do
    n <- toAdtName a
    cs <- traverse (cstrToADTDef mm) (constructors a)
    case mkADTDef n cs of
         Left e -> throwError $ Error InvalidExpression
                                      (getErrorLoc a)
                                      ( pack ("Unable to make adt due to " ++ show e ) )
         Right aDef -> return aDef

-- | Compile a constructor declaration into a 'TorXakis.ConstructorDef'.
cstrToADTDef :: MapsTo Text        Sort mm
             => mm
             -> CstrDecl
             -> CompilerM ConstructorDef
cstrToADTDef mm c = case mkName (cstrName c) of
                        Left e -> throwError $ Error InvalidExpression
                                                     (getErrorLoc c)
                                                     ( Data.Text.concat [ pack "Unable to make constructor name from "
                                                                        , cstrName c
                                                                        , pack (" due to " ++ show e)
                                                                        ] )
                        Right n -> do
                                        cs <- traverse (fieldToFieldDef mm) (cstrFields c)
                                        case mkConstructorDef n cs of
                                             Left e -> throwError $ Error InvalidExpression
                                                                          (getErrorLoc c)
                                                                          ( pack ("Unable to make constructor due to " ++ show e ) )
                                             Right cDef -> return cDef

-- | Compile a field declaration into a 'TorXakis.FieldDef'.
fieldToFieldDef :: MapsTo Text Sort mm
                => mm
                -> FieldDecl
                -> CompilerM FieldDef
fieldToFieldDef mm f =
        case mkName (fieldName f) of
             Left e -> throwError $ Error InvalidExpression
                                          (getErrorLoc f)
                                          ( Data.Text.concat [ pack "Unable to make field name from "
                                                             , fieldName f
                                                             , pack (" due to " ++ show e)
                                                             ] )
             Right n -> let fieldSortText = fst $ fieldSort f
                          in
                            FieldDef n <$> lookupM fieldSortText mm
