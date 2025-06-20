{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.Defs.BehExprDefs
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation functions related to 'TorXakis' function tables.
--------------------------------------------------------------------------------
module TorXakis.Compiler.Defs.FuncTable
    ( adtsToFuncTable
    , funcDeclsToFuncTable
    , fLocToSignatureHandlers
    )
where

import           Control.Monad.Except              (throwError)
import           Data.Foldable                     (foldl')
import           Data.List.Index                   (imapM)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Text                         (Text)
import qualified Data.Text                         as T

import           CstrId                            (CstrId)
import           FuncId                            (FuncId, funcargs, funcsort)
import qualified FuncId
import           FuncTable                         (FuncTable (FuncTable),
                                                    Handler, SignHandler,
                                                    Signature (Signature),
                                                    signHandler, toMap)
import           SortId                            (SortId, sortIdBool,
                                                    sortIdString)
import           StdTDefs                          (accessHandler, cstrHandler,
                                                    eqName, equalHandler,
                                                    fromStringName, fromXmlName,
                                                    iscstrHandler, neqName,
                                                    notEqualHandler,
                                                    toStringName, toXmlName)
import           ValExpr                           (PredefKind (ASF, AST, AXF, AXT),
                                                    cstrPredef)
import           VarId                             (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error           (Entity (Function),
                                                    Error (Error),
                                                    ErrorLoc (NoErrorLoc),
                                                    ErrorType (Undefined),
                                                    _errorLoc, _errorMsg,
                                                    _errorType)
import           TorXakis.Compiler.Maps            (findSortIdM)
import           TorXakis.Compiler.MapsTo          (MapsTo, lookupM)
import           TorXakis.Compiler.ValExpr.FuncDef (FuncDefInfo, funcHandler)
import           TorXakis.Compiler.ValExpr.FuncId  (sortFromStringFuncId,
                                                    sortToStringFuncId)
import           TorXakis.Parser.Data              (ADTDecl, CstrDecl, CstrE,
                                                    FieldDecl, FuncDecl,
                                                    FuncDeclE, Loc, adtName,
                                                    constructors, cstrFields,
                                                    cstrName, fieldName,
                                                    fieldSort, funcName,
                                                    funcParams, funcRetSort,
                                                    getLoc, nodeLoc,
                                                    varDeclSort)

-- | Make a function table from a list of ADT's declarations.
adtsToFuncTable :: ( MapsTo Text        SortId mm
                   , MapsTo (Loc CstrE) CstrId mm)
                => mm -> [ADTDecl] -> CompilerM (FuncTable VarId)
adtsToFuncTable mm ds =
    FuncTable <$> textToHandler
    where
      textToHandlers :: CompilerM [Map Text (SignHandler VarId)]
      textToHandlers = (Map.fromList <$>) <$> traverse (adtToHandlers mm) ds
      textToHandler :: CompilerM (Map Text (SignHandler VarId))
      -- In case we have two functions with the same name (like @fromString@
      -- for instance), we cannot overwrite the handlers, but we have to take
      -- both handlers along.
      textToHandler = foldl' (Map.unionWith Map.union) Map.empty <$> textToHandlers

-- | Make a 'Text' to signature-handler pair from the ADT declaration.
adtToHandlers :: ( MapsTo Text        SortId mm
                 , MapsTo (Loc CstrE) CstrId mm)
              => mm -> ADTDecl -> CompilerM [(Text, SignHandler VarId)]
adtToHandlers mm a = do
    sId <- findSortIdM mm (adtName a, nodeLoc a)
    concat <$> traverse (cstrToHandlers mm sId) (constructors a)

-- | Make 'Text' to signature-handler pair from a constructor declaration.
cstrToHandlers :: ( MapsTo Text        SortId mm
                  , MapsTo (Loc CstrE) CstrId mm)
               => mm
               -> SortId
               -> CstrDecl
               -> CompilerM [(Text, SignHandler VarId)]
cstrToHandlers mm sId c = do
    cId  <- lookupM (getLoc c) mm
    cTH  <- cstrToMkCstrHandler mm sId c
    iTH  <- cstrToIsCstrHandler mm sId c
    fTHs <- imapM (\i f -> fieldToAccessCstrHandler mm sId cId (fieldName f) i f) (cstrFields c)
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
cstrToMkCstrHandler :: ( MapsTo Text        SortId mm
                       , MapsTo (Loc CstrE) CstrId mm)
                    => mm
                    -> SortId
                    -> CstrDecl
                    -> CompilerM (Text, SignHandler VarId)
cstrToMkCstrHandler mm sId c = do
    cId <- lookupM (getLoc c) mm
    fSids <- traverse (findSortIdM mm . fieldSort) (cstrFields c)
    return (cstrName c, Map.singleton (Signature fSids sId) (cstrHandler cId))

-- | Create a "is-constructor"  function from a constructor.
cstrToIsCstrHandler :: MapsTo (Loc CstrE) CstrId mm
                    => mm
                    -> SortId  -- ^ Sort id of the containing ADT.
                    -> CstrDecl
                    -> CompilerM (Text, SignHandler VarId)
cstrToIsCstrHandler mm sId c = do
    cId <- lookupM (getLoc c) mm
    return ("is"<> cstrName c, Map.singleton sign (iscstrHandler cId))
    where
      sign :: Signature
      sign = Signature [sId] sortIdBool

-- | Create an accessor handler from a field.
fieldToAccessCstrHandler :: MapsTo Text SortId mm
                         => mm
                         -> SortId  -- ^ Sort id of the containing ADT.
                         -> CstrId  -- ^ Id of the containing constructor.
                         -> Text    -- ^ Name of the field in the constructor.
                         -> Int     -- ^ Position of the field in the constructor.
                         -> FieldDecl
                         -> CompilerM (Text, SignHandler VarId)
fieldToAccessCstrHandler mm sId cId n p f = do
    fId <- findSortIdM mm (fieldSort f)
    return ( fieldName f
           , Map.singleton (Signature [sId] fId) (accessHandler cId n p))

-- | Create a function table from a list of function declarations.
funcDeclsToFuncTable :: ( MapsTo Text SortId mm
                        , MapsTo (Loc FuncDeclE) FuncId mm
                        , MapsTo FuncId FuncDefInfo mm )
                     => mm -> [FuncDecl] -> CompilerM (FuncTable VarId)
funcDeclsToFuncTable mm fs = FuncTable . Map.fromListWith Map.union <$>
    traverse (funcDeclToFuncTable mm) fs

-- | Create a 'Text' to signature-handler pair from a function declaration.
funcDeclToFuncTable :: ( MapsTo Text SortId mm
                        , MapsTo (Loc FuncDeclE) FuncId mm
                        , MapsTo FuncId FuncDefInfo mm )
                    => mm -> FuncDecl -> CompilerM (Text, SignHandler VarId)
funcDeclToFuncTable mm f = do
    sId   <- findSortIdM mm (funcRetSort f)
    fSids <- traverse (findSortIdM mm . varDeclSort) (funcParams f)
    hdlr  <- fBodyToHandler mm f
    return (funcName f, Map.singleton (Signature fSids sId) hdlr)

-- | Compile a function declaration into a handler.
fBodyToHandler :: ( MapsTo (Loc FuncDeclE) FuncId mm
                  , MapsTo FuncId FuncDefInfo mm )
               => mm -> FuncDecl -> CompilerM (Handler VarId)
fBodyToHandler mm f = do
    fId <- lookupM (getLoc f :: Loc FuncDeclE) mm :: CompilerM FuncId
    fdi <- lookupM fId mm
    return $ funcHandler fdi

-- | Make a map from location of function declarations to their signature and
-- handlers, using the given map and function table.
fLocToSignatureHandlers :: Map (Loc FuncDeclE) FuncId
                        -> FuncTable VarId
                        -> CompilerM (Map (Loc FuncDeclE)  (Signature, Handler VarId))
fLocToSignatureHandlers flocToFid ftable = Map.fromList . zip (Map.keys flocToFid) <$>
    traverse funcIdToSignatureHandler (Map.elems flocToFid)
    where
      funcIdToSignatureHandler :: FuncId
                               -> CompilerM (Signature, Handler VarId)
      funcIdToSignatureHandler fid =
          case Map.lookup sig sh of
              Nothing -> throwError Error
                  { _errorType = Undefined Function
                  , _errorLoc = NoErrorLoc
                  , _errorMsg = "Could not find " <> T.pack (show fid)
                                <> (T.pack . show . Map.keys . toMap $ ftable)
                  }
              Just h -> return (sig, h)
          where sh = signHandler (FuncId.name fid) ftable
                sig = Signature (funcargs fid) (funcsort fid)
