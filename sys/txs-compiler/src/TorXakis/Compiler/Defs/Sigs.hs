{-# LANGUAGE FlexibleContexts  #-}

module TorXakis.Compiler.Defs.Sigs where

import           Data.Map                         (Map)
import           Data.Text                        (Text)

import           Sigs                             (Sigs, empty, func, sort)
import           SortId                           (SortId)
import           VarId                            (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Defs.FuncTable
import           TorXakis.Parser.Data

adtDeclsToSigs :: (MapsTo Text SortId mm, HasFuncIds e, HasFuncDefs e, HasCstrIds e)
               => mm -> e -> [ADTDecl] -> CompilerM (Sigs VarId)
adtDeclsToSigs mm e ds = do
    ft <- adtsToFuncTable mm e ds
    return $ empty { func = ft }

funDeclsToSigs :: (MapsTo Text SortId mm, HasFuncIds e, HasFuncDefs e, HasCstrIds e)
               => mm -> e -> [FuncDecl] -> CompilerM (Sigs VarId)
funDeclsToSigs mm e ds = do
    ft <- funcDeclsToFuncTable mm e ds
    return $ empty { func = ft }

sortsToSigs :: Map Text SortId -> Sigs VarId
sortsToSigs sm = empty { sort = sm }
