{-# LANGUAGE FlexibleContexts  #-}

module TorXakis.Compiler.Defs.Sigs where

import           Data.Map                         (Map)
import           Data.Text                        (Text)

import           Sigs                             (Sigs, empty, func, sort)
import           SortId                           (SortId)
import           VarId                            (VarId)
import           CstrId (CstrId)
    
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Defs.FuncTable
import           TorXakis.Parser.Data

adtDeclsToSigs :: ( MapsTo Text SortId mm
                  , MapsTo (Loc CstrE) CstrId mm )
               => mm -> [ADTDecl] -> CompilerM (Sigs VarId)
adtDeclsToSigs mm ds = do
    ft <- adtsToFuncTable mm ds
    return $ empty { func = ft }

funDeclsToSigs :: ( MapsTo Text SortId mm
                  , MapsTo (Loc CstrE) CstrId mm
                  , HasFuncIds e
                  , HasFuncDefs e)
               => mm -> e -> [FuncDecl] -> CompilerM (Sigs VarId)
funDeclsToSigs mm e ds = do
    ft <- funcDeclsToFuncTable mm e ds
    return $ empty { func = ft }

sortsToSigs :: Map Text SortId -> Sigs VarId
sortsToSigs sm = empty { sort = sm }
