{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module defines functions to compile parsed definitions into
-- function tables ('FuncTable').

module TorXakis.Compiler.Defs.FuncTable where

import           Control.Arrow                    (second)
import           Data.Foldable                    (foldl')
import           Data.List.Index                  (imapM)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Semigroup                   ((<>))
import           Data.Text                        (Text)

import           CstrId                           (CstrId)
import           FuncTable                        (FuncTable (FuncTable),
                                                   Handler, SignHandler,
                                                   Signature (Signature))
import           Id                               (Id (Id))
import           SortId                           (SortId, sortIdBool,
                                                   sortIdString)
import           StdTDefs                         (accessHandler, cstrHandler,
                                                   eqName, equalHandler,
                                                   fromStringName, fromXmlName,
                                                   iscstrHandler, neqName,
                                                   notEqualHandler,
                                                   toStringName, toStringName,
                                                   toXmlName)
import           ValExpr                          (PredefKind (ASF, AST, AXF, AXT),
                                                   cstrFunc, cstrPredef)
import           VarId                            (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.ValExpr.FuncId
import           TorXakis.Parser.Data

-- | Make a function table.
adtsToFuncTable :: (MapsTo Text SortId mm, HasCstrIds e)
                   => mm -> e -> [ADTDecl] -> CompilerM (FuncTable VarId)
adtsToFuncTable mm e ds =
    FuncTable <$> textToHandler
    where
      textToHandlers :: CompilerM [Map Text (SignHandler VarId)]
      textToHandlers = (Map.fromList <$>) <$> traverse (adtToHandlers mm e) ds
      textToHandler :: CompilerM (Map Text (SignHandler VarId))
      -- In case we have two functions with the same name (like @fromString@
      -- for instance), we cannot overwrite the handlers, but we have to take
      -- both handlers along.
      textToHandler = foldl' (Map.unionWith Map.union) Map.empty <$> textToHandlers

adtToHandlers :: (MapsTo Text SortId mm, HasCstrIds e)
              => mm -> e -> ADTDecl -> CompilerM [(Text, SignHandler VarId)]
adtToHandlers mm e a = do
    sId <- findSortIdM mm (adtName a, nodeLoc a)
    concat <$> traverse (cstrToHandlers mm e sId) (constructors a)

cstrToHandlers :: (MapsTo Text SortId mm, HasCstrIds e)
               => mm
               -> e
               -> SortId
               -> CstrDecl
               -> CompilerM [(Text, SignHandler VarId)]
cstrToHandlers mm e sId c = do
    cId  <- findCstrIdM e (getLoc c)
    cTH  <- cstrToMkCstrHandler mm e sId c
    iTH  <- cstrToIsCstrHandler e sId c
    fTHs <- imapM (fieldToAccessCstrHandler mm sId cId) (cstrFields c)
    -- Taken from TxsHappy@949:
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
cstrToMkCstrHandler :: (MapsTo Text SortId mm, HasCstrIds e)
                    => mm
                    -> e
                    -> SortId
                    -> CstrDecl
                    -> CompilerM (Text, SignHandler VarId)
cstrToMkCstrHandler mm e sId c = do
    cId <- findCstrIdM e (getLoc c)
    fSids <- traverse (findSortIdM mm . fieldSort) (cstrFields c)
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
fieldToAccessCstrHandler :: MapsTo Text SortId mm
                         => mm
                         -> SortId  -- ^ Sort id of the containing ADT.
                         -> CstrId  -- ^ Id of the containing constructor.
                         -> Int     -- ^ Position of the field in the constructor.
                         -> FieldDecl
                         -> CompilerM (Text, SignHandler VarId)
fieldToAccessCstrHandler mm sId cId p f = do
    fId <- findSortIdM mm (fieldSort f)
    return ( fieldName f
           , Map.singleton (Signature [sId] fId) (accessHandler cId p))

funcDeclsToFuncTable :: (MapsTo Text SortId mm, HasFuncIds e, HasFuncDefs e)
                     => mm -> e -> [FuncDecl] -> CompilerM (FuncTable VarId)
funcDeclsToFuncTable mm e fs = FuncTable . Map.fromListWith Map.union <$>
    traverse (funcDeclToFuncTable mm e) fs

funcDeclToFuncTable :: (MapsTo Text SortId mm, HasFuncIds e, HasFuncDefs e)
                    => mm -> e -> FuncDecl -> CompilerM (Text, SignHandler VarId)
funcDeclToFuncTable mm e f = do
    sId   <- findSortIdM mm (funcRetSort f)
    fSids <- traverse (findSortIdM mm . varDeclSort) (funcParams f)
    hdlr  <- fBodyToHandler e f
    return (funcName f, Map.singleton (Signature fSids sId) hdlr)

fBodyToHandler :: (HasFuncIds e, HasFuncDefs e)
               => e -> FuncDecl -> CompilerM (Handler VarId)
fBodyToHandler e f = do
    fId  <- findFuncIdForDeclM e (getLoc f)
    return $ cstrFunc (getFuncDefT e) fId
