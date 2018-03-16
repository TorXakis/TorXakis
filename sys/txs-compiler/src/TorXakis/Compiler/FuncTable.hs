{-# LANGUAGE OverloadedStrings #-}
-- | This module defines functions to compile parsed definitions into
-- function tables ('FuncTable').

module TorXakis.Compiler.FuncTable where

import           Data.Text (Text)
import qualified Data.Map as Map
import           Data.Semigroup ((<>))
import           Data.List.Index (imapM)

import           SortId                        (SortId, sortIdBool)
import           CstrId                        (CstrId)
import           Sigs                          (Sigs)
import           VarId                         (VarId)
import           FuncTable                     ( FuncTable (FuncTable), SignHandler, Handler
                                               , Signature (Signature))
import           TorXakis.Sort.Name            (getName)
import           TorXakis.Sort.ADTDefs         (ADTDefs, Sort)
import           TorXakis.Sort.ConstructorDefs (ConstructorDef)
import           StdTDefs (iscstrHandler, accessHandler, cstrHandler)
import           ValExpr (cstrFunc)

import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error

-- | Make a function table.
compileToFuncTable :: Env -> [ADTDecl] -> Either Error (FuncTable VarId)
compileToFuncTable e ds =
    -- TODO: the `FuncTable` should be replaced by a better one that checks
    -- that there are no double definitions for instance. We could do this
    -- check here for now...
    FuncTable . Map.fromList . concat <$> traverse (adtToHandlers e) ds

adtToHandlers :: Env -> ADTDecl -> Either Error [(Text, SignHandler VarId)]
adtToHandlers e a = do
    sId <- findSort e (nodeName a)
    concat <$> traverse (cstrToHandlers e sId) (child a)

cstrToHandlers :: Env
               -> SortId
               -> CstrDecl
               -> Either Error [(Text, SignHandler VarId)]
cstrToHandlers e sId c = do
    cId  <- findCstr e c
    cTH  <- cstrToMkCstrHandler e sId c
    iTH  <- cstrToIsCstrHandler e sId c
    fTHs <- imapM (fieldToAccessCstrHandler e sId cId) (child c)    
    return $ cTH:iTH:fTHs

-- | Create a handler to create a constructor.
cstrToMkCstrHandler :: Env
                    -> SortId
                    -> CstrDecl
                    -> Either Error (Text, SignHandler VarId)
cstrToMkCstrHandler e sId c = do
    cId <- findCstr e c
    fSids <- traverse (fieldSort e) (child c)
    return (n, Map.singleton (Signature fSids sId) (cstrHandler cId))
    where
      n = nodeName c

fieldSort :: Env -> FieldDecl -> Either Error SortId
fieldSort e f = findSort e (nodeName . child $ f)    

-- | Create a "is-constructor"  function from a constructor.
cstrToIsCstrHandler :: Env 
                    -> SortId  -- ^ Sort id of the containing ADT.
                    -> CstrDecl
                    -> Either Error (Text, SignHandler VarId)
cstrToIsCstrHandler e sId c = do
    cId <- findCstr e c
    return ("is"<> n, Map.singleton sign (iscstrHandler cId))
    where
      n = nodeName c
      sign :: Signature
      sign = Signature [sId] sortIdBool

-- | Create an accessor handler from a field.
fieldToAccessCstrHandler :: Env 
                        -> SortId  -- ^ Sort id of the containing ADT.
                        -> CstrId  -- ^ Id of the containing constructor.
                        -> Int     -- ^ Position of the field in the constructor.
                        -> FieldDecl
                        -> Either Error (Text, SignHandler VarId)
fieldToAccessCstrHandler e sId cId p f = do
    fId <- findSort e n
    return (n, Map.singleton (Signature [sId] fId) (accessHandler cId p))
    where
      n = nodeName f

funcDeclsToFuncTable :: Env -> [FuncDecl] -> Either Error (FuncTable VarId)
funcDeclsToFuncTable e fs = FuncTable . Map.fromList <$>
    traverse (funcDeclToFuncTable e) fs
    

funcDeclToFuncTable :: Env -> FuncDecl -> Either Error (Text, SignHandler VarId)
funcDeclToFuncTable e f = do
    sId   <- findSort e (nodeName . funcRetType . child $ f)
    fSids <- traverse (fieldSort e) (funcParams . child $ f)
    hdlr  <- fBodyToHandler e f
    return (nodeName f, Map.singleton (Signature fSids sId) hdlr)

fBodyToHandler :: Env -> FuncDecl -> Either Error (Handler VarId)
fBodyToHandler e f = do
    fId  <- findFuncId e (uid . nodeMdata $ f)
    return $ cstrFunc (fDefMap e) fId
