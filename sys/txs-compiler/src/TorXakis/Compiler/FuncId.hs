{-# LANGUAGE OverloadedStrings          #-}
module TorXakis.Compiler.FuncId where

-- TODO: consider adding your own prelude.
import           Control.Monad.State  (State, MonadState, StateT)
import           Data.Semigroup ((<>))

import           Id (Id (Id))
import           SortId (SortId, sortIdBool)
import           CstrId (name, cstrsort, CstrId)
import           FuncId (FuncId (FuncId))

import           TorXakis.Parser.Data ( CstrDecl, nodeName, FieldDecl, child
                                      , FuncDecl, funcParams, funcRetType)
import           TorXakis.Compiler.Data (St, Env, getNextId, CompilerM, findSortM)
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.SortId
    
cstrToIsCstrFuncId :: CstrId -> CompilerM FuncId
cstrToIsCstrFuncId cId = do
    fId <- getNextId
    return $ FuncId ("is" <> name cId) (Id fId) [cstrsort cId] sortIdBool

-- | Define the accessor function for a constructor, using the given field.
cstrToAccFuncId :: Env
                -> CstrId    -- ^ Id of the containing constructor
                -> FieldDecl
                -> CompilerM FuncId
cstrToAccFuncId e cId f = do
    fId <- getNextId
    -- TODO: you might consider making the parse tree a bit more type-safer.
    -- Here we could be looking up the name of anything.
    sId <- findSortM e (nodeName . child $ f)
    return $ FuncId (nodeName f) (Id fId) [cstrsort cId] sId

funcDeclToFuncId :: Env -> FuncDecl -> CompilerM FuncId
funcDeclToFuncId  e f = do
    fId   <- getNextId
    aSids <- traverse (fieldSort e) (funcParams . child $ f)
    rSid  <- findSortM e (nodeName . funcRetType . child $ f)
    return $ FuncId (nodeName f) (Id fId) aSids rSid
