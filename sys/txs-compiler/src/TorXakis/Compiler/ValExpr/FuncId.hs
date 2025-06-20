{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.ValExpr.FuncId
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compilation functions related to 'TorXakis' function id's.
--------------------------------------------------------------------------------
module TorXakis.Compiler.ValExpr.FuncId
    ( cstrToAccFuncId
    , cstrToIsCstrFuncId
    , sortFromStringFuncId
    , sortToStringFuncId
    , adtsToFuncIds
    , funcIdAsSignature
    , funcDeclsToFuncIds
    , getStdFuncIds
    )
where

import qualified Data.Map                         as Map
import           Data.Text                        (Text)

import           CstrId                           (CstrId, cstrsort, name)
import           FuncId                           (FuncId (FuncId), funcargs,
                                                   funcsort)
import           FuncTable                        (SignHandler,
                                                   Signature (Signature), toMap)
import           Id                               (Id (Id))
import           SortId                           (SortId, sortIdBool,
                                                   sortIdString)
import           StdTDefs                         (eqName, fromStringName,
                                                   fromXmlName, neqName,
                                                   stdFuncTable, toStringName,
                                                   toXmlName)
import           VarId                            (VarId)

import           TorXakis.Compiler.Data           (CompilerM, getNextId)
import           TorXakis.Compiler.Maps           (findSortIdM)
import           TorXakis.Compiler.MapsTo         (MapsTo)
import           TorXakis.Compiler.ValExpr.SortId (sortIdOfVarDeclM)
import           TorXakis.Parser.Data             (ADTDecl, CstrDecl, FieldDecl,
                                                   FuncDecl, FuncDeclE,
                                                   Loc (PredefLoc), adtName,
                                                   constructors, cstrFields,
                                                   cstrName, fieldName,
                                                   fieldSort, funcName,
                                                   funcParams, funcRetSort,
                                                   getLoc, nodeLoc)

-- | Make a function id from the given constructor id.
cstrToIsCstrFuncId :: CstrId -> CompilerM FuncId
cstrToIsCstrFuncId cId = do
    fId <- getNextId
    return $ FuncId ("is" <> name cId) (Id fId) [cstrsort cId] sortIdBool

-- | Define the accessor function for a constructor, using the given field.
cstrToAccFuncId :: MapsTo Text SortId mm
                => mm
                -> CstrId    -- ^ Id of the containing constructor
                -> FieldDecl
                -> CompilerM FuncId
cstrToAccFuncId mm cId f = do
    fId <- getNextId
    sId <- findSortIdM mm (fieldSort f)
    return $ FuncId (fieldName f) (Id fId) [cstrsort cId] sId

-- | Compile a list of function declarations to a list of pairs consisting of
-- the function location and its id.
funcDeclsToFuncIds :: MapsTo Text SortId mm
                   => mm -> [FuncDecl] -> CompilerM [(Loc FuncDeclE, FuncId)]
funcDeclsToFuncIds mm fs = zip (getLoc <$> fs) <$> traverse (funcDeclToFuncId mm) fs

-- | Compile a function declaration into a function id.
funcDeclToFuncId :: MapsTo Text SortId mm
                 => mm -> FuncDecl -> CompilerM FuncId
funcDeclToFuncId mm f = do
    fId   <- getNextId
    aSids <- traverse (sortIdOfVarDeclM mm) (funcParams f)
    rSid  <- findSortIdM mm (funcRetSort f)
    return $ FuncId (funcName f) (Id fId) aSids rSid

-- | Make a 'FuncId' for a sort to string function.
sortToStringFuncId :: SortId -> CompilerM FuncId
sortToStringFuncId sId = do
    fId <- getNextId
    return $ FuncId toStringName (Id fId) [sId] sortIdString

-- | Make a 'FuncId' for a string to sort function.
sortFromStringFuncId :: SortId -> CompilerM FuncId
sortFromStringFuncId sId = do
    fId <- getNextId
    return $ FuncId fromStringName (Id fId) [sortIdString] sId

-- | Generate the function id's that correspond to the functions in the
-- standard function table (@stdFuncTable@)
getStdFuncIds :: CompilerM [(Loc FuncDeclE, FuncId)]
getStdFuncIds = concat <$>
    traverse signHandlerToFuncIds (Map.toList (toMap stdFuncTable))

signHandlerToFuncIds :: (Text, SignHandler VarId) -> CompilerM [(Loc FuncDeclE, FuncId)]
signHandlerToFuncIds (n, sh) =
    traverse (signatureToFuncId n) (Map.keys sh)

signatureToFuncId :: Text -> Signature -> CompilerM (Loc FuncDeclE, FuncId)
signatureToFuncId n (Signature aSids rSid) = do
    fId  <- getNextId
    return (PredefLoc n fId, FuncId n (Id fId) aSids rSid)

-- | Create the function id's that the ADT implicitly defines.
adtsToFuncIds :: (MapsTo Text SortId mm)
              => mm -> [ADTDecl] -> CompilerM [(Loc FuncDeclE, FuncId)]
adtsToFuncIds mm ds = concat <$> traverse (adtToFuncIds mm) ds

adtToFuncIds :: (MapsTo Text SortId mm)
             => mm -> ADTDecl -> CompilerM [(Loc FuncDeclE, FuncId)]
adtToFuncIds mm a = do
    sId <- findSortIdM mm (adtName a, nodeLoc a)
    cstrFIds <- concat <$> traverse (cstrToFuncIds mm sId) (constructors a)
    adtFIds  <- concat <$> sequence [ eqFdiFid sId
                                    , neqFdiFid sId
                                    , toStrFdiFid sId
                                    , fromStrFdiFid sId
                                    , toXMLFdiFid sId
                                    , fromXMLFdiFid sId
                                    ]
    return $ cstrFIds ++ adtFIds
    where
      eqFdiFid sId =
          mkPredef eqName [sId, sId] sortIdBool
      neqFdiFid sId =
          mkPredef neqName [sId, sId] sortIdBool
      toStrFdiFid sId =
          mkPredef toStringName [sId] sortIdString
      fromStrFdiFid =
          mkPredef fromStringName [sortIdString]
      toXMLFdiFid sId =
          mkPredef toXmlName [sId] sortIdString
      fromXMLFdiFid =
          mkPredef fromXmlName [sortIdString]

mkPredef :: Text -> [SortId] -> SortId -> CompilerM [(Loc FuncDeclE, FuncId)]
mkPredef n aSids rSid =
    (\i -> [(PredefLoc n i, FuncId n (Id i) aSids rSid)]) <$> getNextId

cstrToFuncIds :: (MapsTo Text SortId mm)
              => mm -> SortId -> CstrDecl -> CompilerM [(Loc FuncDeclE, FuncId)]
cstrToFuncIds mm sId c =
    concat <$> sequence [ mkCstrFdiFid
                        , isCstrFdiFid
                        , cstrAccessFdiFid
                        ]
    where
      cn = cstrName c
      mkCstrFdiFid = do
          mkCstrId <- getNextId
          fSids <- traverse (findSortIdM mm . fieldSort) (cstrFields c)
          -- Create a function id for the constructor function.
          -- NOTE: sharing the ID should be fine.
          return [(PredefLoc cn mkCstrId, FuncId cn (Id mkCstrId) fSids sId)]
      isCstrFdiFid = do
          let isN = "is" <> cn
          isCstrId <- getNextId
          -- Create a function id for the constructor function.
          -- NOTE: sharing the ID should be fine.
          return [(PredefLoc isN isCstrId, FuncId isN (Id isCstrId) [sId] sortIdBool)]
      cstrAccessFdiFid = traverse accessFdiFid (cstrFields c)
          where
            accessFdiFid f = do
                let accN = fieldName f
                accSid <- getNextId
                fSid   <- findSortIdM mm (fieldSort f)
                return (PredefLoc accN accSid, FuncId accN (Id accSid) [sId] fSid)

-- | Return the function id as a signature.
funcIdAsSignature :: FuncId -> Signature
funcIdAsSignature fid = Signature (funcargs fid) (funcsort fid)
