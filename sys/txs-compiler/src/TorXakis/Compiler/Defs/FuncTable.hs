{-# LANGUAGE OverloadedStrings #-}
-- | This module defines functions to compile parsed definitions into
-- function tables ('FuncTable').

module TorXakis.Compiler.Defs.FuncTable where

import           Data.Text (Text)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Semigroup ((<>))
import           Data.List.Index (imapM)
import           Data.Foldable (foldl')
    
import           Id (Id(Id))-- TODO: remove the this import we need to change the either to a 'CompilerM'
import           SortId                        (SortId, sortIdBool, sortIdString)
import           CstrId                        (CstrId)
import           VarId                         (VarId)
import           FuncTable                     ( FuncTable (FuncTable), SignHandler, Handler
                                               , Signature (Signature))
import           StdTDefs ( iscstrHandler, accessHandler, cstrHandler
                          , eqName, equalHandler, neqName, notEqualHandler
                          , toStringName, fromStringName, toStringName, fromXmlName, toXmlName)
import           ValExpr (cstrFunc, PredefKind (AST, ASF, AXT, AXF), cstrPredef)

import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.ValExpr.FuncId

-- | Make a function table.
compileToFuncTable :: (HasSortIds e, HasCstrIds e)
                   => e -> [ADTDecl] -> CompilerM (FuncTable VarId)
compileToFuncTable e ds =
    -- TODO: the `FuncTable` should be replaced by a better one that checks
    -- that there are no double definitions for instance. We could do this
    -- check here for now...
    FuncTable <$> textToHandler
    where
      textToHandlers :: CompilerM [Map Text (SignHandler VarId)]
      textToHandlers = (Map.fromList <$>) <$> traverse (adtToHandlers e) ds
      textToHandler :: CompilerM (Map Text (SignHandler VarId))
      -- In case we have two functions with the same name (like @fromString@
      -- for instance), we cannot overwrite the handlers, but we have to take
      -- both handlers along.
      textToHandler = foldl' (Map.unionWith Map.union) Map.empty <$> textToHandlers
      
adtToHandlers :: (HasSortIds e, HasCstrIds e)
              => e -> ADTDecl -> CompilerM [(Text, SignHandler VarId)]
adtToHandlers e a = do
    sId <- findSortIdM e (adtName a, nodeMdata a)
    concat <$> traverse (cstrToHandlers e sId) (constructors a)

cstrToHandlers :: (HasSortIds e, HasCstrIds e)
               => e
               -> SortId
               -> CstrDecl
               -> CompilerM [(Text, SignHandler VarId)]
cstrToHandlers e sId c = do
    cId  <- findCstrIdM e (getLoc c)
    cTH  <- cstrToMkCstrHandler e sId c
    iTH  <- cstrToIsCstrHandler e sId c
    fTHs <- imapM (fieldToAccessCstrHandler e sId cId) (cstrFields c)
    -- Taken from TxsHappy#949:
    astFid <- sortToStringFuncId sId
    asfFid <- sortFromStringFuncId sId
    axtFid <- sortToStringFuncId sId
    axfFid <- sortFromStringFuncId sId    
    let stdHs =
            [ (eqName, Map.singleton
                       (Signature [sId, sId] sortIdBool) equalHandler)
            , (neqName, Map.singleton 
                        (Signature [sId,sId] sortIdBool) notEqualHandler)
            , (toStringName, Map.singleton
                             (Signature [sId] sortIdString)
                             (cstrPredef AST astFid))
            , (fromStringName, Map.singleton
                               (Signature [sortIdString] sId)
                               (cstrPredef ASF asfFid))
            , (toXmlName, Map.singleton
                          (Signature [sId] sortIdString)
                          (cstrPredef AXT  axtFid))
            , (fromXmlName, Map.singleton
                            (Signature [sortIdString] sId)
                            (cstrPredef AXF axfFid))
            ]
    return $ cTH:iTH:(fTHs++stdHs)

-- | Create a handler to create a constructor.
cstrToMkCstrHandler :: (HasSortIds e, HasCstrIds e)
                    => e
                    -> SortId
                    -> CstrDecl
                    -> CompilerM (Text, SignHandler VarId)
cstrToMkCstrHandler e sId c = do
    cId <- findCstrIdM e (getLoc c)
    fSids <- traverse (findSortIdM e . fieldSort) (cstrFields c)
    return (cstrName c, Map.singleton (Signature fSids sId) (cstrHandler cId))

-- | Create a "is-constructor"  function from a constructor.
cstrToIsCstrHandler :: HasCstrIds e
                    => e
                    -> SortId  -- ^ Sort id of the containing ADT.
                    -> CstrDecl
                    -> CompilerM (Text, SignHandler VarId)
cstrToIsCstrHandler e sId c = do
    cId <- findCstrIdM e (getLoc c)
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
                         -> CompilerM (Text, SignHandler VarId)
fieldToAccessCstrHandler e sId cId p f = do
    fId <- findSortIdM e (fieldSort f)
    return ( fieldName f
           , Map.singleton (Signature [sId] fId) (accessHandler cId p))

funcDeclsToFuncTable :: (HasSortIds e, HasFuncIds e, HasFuncDefs e)
                     => e -> [FuncDecl] -> CompilerM (FuncTable VarId)
funcDeclsToFuncTable e fs = FuncTable . Map.fromList <$>
    traverse (funcDeclToFuncTable e) fs

funcDeclToFuncTable :: (HasSortIds e, HasFuncIds e, HasFuncDefs e)
                    => e -> FuncDecl -> CompilerM (Text, SignHandler VarId)
funcDeclToFuncTable e f = do
    sId   <- findSortIdM e (funcRetSort f)
    fSids <- traverse (findSortIdM e . varDeclSort) (funcParams f)
    hdlr  <- fBodyToHandler e f
    return (funcName f, Map.singleton (Signature fSids sId) hdlr)

fBodyToHandler :: (HasFuncIds e, HasFuncDefs e)
               => e -> FuncDecl -> CompilerM (Handler VarId)
fBodyToHandler e f = do
    fId  <- findFuncIdM e (getLoc f)
    return $ cstrFunc (getFuncDefT e) fId
