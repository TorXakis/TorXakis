{-# LANGUAGE OverloadedStrings #-}
-- | This module defines functions to compile parsed definitions into
-- function tables ('FuncTable').

module TorXakis.Compiler.Defs.FuncTable where

import           Data.Text (Text)
import qualified Data.Map as Map
import           Data.Semigroup ((<>))
import           Data.List.Index (imapM)

import           SortId                        (SortId, sortIdBool)
import           CstrId                        (CstrId)
import           VarId                         (VarId)
import           FuncTable                     ( FuncTable (FuncTable), SignHandler, Handler
                                               , Signature (Signature))
import           StdTDefs (iscstrHandler, accessHandler, cstrHandler)
import           ValExpr (cstrFunc)

import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error

-- | Make a function table.
compileToFuncTable :: (HasSortIds e, HasCstrIds e)
                   => e -> [ADTDecl] -> Either Error (FuncTable VarId)
compileToFuncTable e ds =
    -- TODO: the `FuncTable` should be replaced by a better one that checks
    -- that there are no double definitions for instance. We could do this
    -- check here for now...
    FuncTable . Map.fromList . concat <$> traverse (adtToHandlers e) ds

adtToHandlers :: (HasSortIds e, HasCstrIds e)
              => e -> ADTDecl -> Either Error [(Text, SignHandler VarId)]
adtToHandlers e a = do
    sId <- findSortId e (adtName a, nodeMdata a)
    concat <$> traverse (cstrToHandlers e sId) (constructors a)

cstrToHandlers :: (HasSortIds e, HasCstrIds e)
               => e
               -> SortId
               -> CstrDecl
               -> Either Error [(Text, SignHandler VarId)]
cstrToHandlers e sId c = do
    cId  <- findCstrId e (getLoc c)
    cTH  <- cstrToMkCstrHandler e sId c
    iTH  <- cstrToIsCstrHandler e sId c
    fTHs <- imapM (fieldToAccessCstrHandler e sId cId) (cstrFields c)
    return $ cTH:iTH:fTHs

-- | Create a handler to create a constructor.
cstrToMkCstrHandler :: (HasSortIds e, HasCstrIds e)
                    => e
                    -> SortId
                    -> CstrDecl
                    -> Either Error (Text, SignHandler VarId)
cstrToMkCstrHandler e sId c = do
    cId <- findCstrId e (getLoc c)
    fSids <- traverse (findSortId e . fieldSort) (cstrFields c)
    return (cstrName c, Map.singleton (Signature fSids sId) (cstrHandler cId))

-- | Create a "is-constructor"  function from a constructor.
cstrToIsCstrHandler :: HasCstrIds e
                    => e
                    -> SortId  -- ^ Sort id of the containing ADT.
                    -> CstrDecl
                    -> Either Error (Text, SignHandler VarId)
cstrToIsCstrHandler e sId c = do
    cId <- findCstrId e (getLoc c)
    return ("is"<> cstrName c, Map.singleton sign (iscstrHandler cId))
    where
      sign :: Signature
      sign = Signature [sId] sortIdBool

-- | Create an accessor handler from a field.
fieldToAccessCstrHandler :: HasSortIds e
                         => e
                         -> SortId  -- ^ Sort id of the containing ADT.
                         -> CstrId  -- ^ Id of the containing constructor.
                         -> Int     -- ^ Position of the field in the constructor.
                         -> FieldDecl
                         -> Either Error (Text, SignHandler VarId)
fieldToAccessCstrHandler e sId cId p f = do
    fId <- findSortId e (fieldSort f)
    return ( fieldName f
           , Map.singleton (Signature [sId] fId) (accessHandler cId p))

funcDeclsToFuncTable :: (HasSortIds e, HasFuncIds e, HasFuncDefs e)
                     => e -> [FuncDecl] -> Either Error (FuncTable VarId)
funcDeclsToFuncTable e fs = FuncTable . Map.fromList <$>
    traverse (funcDeclToFuncTable e) fs

funcDeclToFuncTable :: (HasSortIds e, HasFuncIds e, HasFuncDefs e)
                    => e -> FuncDecl -> Either Error (Text, SignHandler VarId)
funcDeclToFuncTable e f = do
    sId   <- findSortId e (funcRetSort f)
    fSids <- traverse (findSortId e . varDeclSort) (funcParams f)
    hdlr  <- fBodyToHandler e f
    return (funcName f, Map.singleton (Signature fSids sId) hdlr)

fBodyToHandler :: (HasFuncIds e, HasFuncDefs e)
               => e -> FuncDecl -> Either Error (Handler VarId)
fBodyToHandler e f = do
    fId  <- findFuncId e (getLoc f)
    return $ cstrFunc (getFuncDefT e) fId
