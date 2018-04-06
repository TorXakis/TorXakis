{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module TorXakis.Compiler.ValExpr.FuncId where

-- TODO: consider adding your own prelude.
import           Control.Arrow                    ((***))
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Semigroup                   ((<>))
import           Data.Text                        (Text)

import           CstrId                           (CstrId, cstrsort, name)
import           FuncId                           (FuncId (FuncId))
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

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Parser.Data

cstrToIsCstrFuncId :: CstrId -> CompilerM FuncId
cstrToIsCstrFuncId cId = do
    fId <- getNextId
    return $ FuncId ("is" <> name cId) (Id fId) [cstrsort cId] sortIdBool

-- | Define the accessor function for a constructor, using the given field.
cstrToAccFuncId :: HasSortIds e
                => e
                -> CstrId    -- ^ Id of the containing constructor
                -> FieldDecl
                -> CompilerM FuncId
cstrToAccFuncId e cId f = do
    fId <- getNextId
    sId <- findSortIdM e (fieldSort f)
    return $ FuncId (fieldName f) (Id fId) [cstrsort cId] sId

funcDeclsToFuncIds :: HasSortIds e
                   => e -> [FuncDecl] -> CompilerM (Map (Loc FuncDeclE) FuncId)
funcDeclsToFuncIds e fs = do
    fIds <- traverse (funcDeclToFuncId e) fs
    return $ Map.fromList $ zip (getLoc <$> fs) fIds

funcDeclToFuncId :: HasSortIds e
                 => e -> FuncDecl -> CompilerM FuncId
funcDeclToFuncId  e f = do
    fId   <- getNextId
    aSids <- traverse (sortIdOfVarDeclM e) (funcParams f)
    rSid  <- findSortIdM e (funcRetSort f)
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
getStdFuncIds :: CompilerM [(FuncDefInfo, FuncId)]
getStdFuncIds = concat <$>
    traverse signHandlerToFuncIds (Map.toList (toMap stdFuncTable))

signHandlerToFuncIds :: (Text, SignHandler VarId) -> CompilerM [(FuncDefInfo, FuncId)]
signHandlerToFuncIds (n, sh) =
    traverse (signatureToFuncId n) (Map.keys sh)

signatureToFuncId :: Text -> Signature -> CompilerM (FuncDefInfo, FuncId)
signatureToFuncId n (Signature aSids rSid) = do
    fId  <- getNextId
    return (IDefUid n fId, FuncId n (Id fId) aSids rSid)

-- | Create the function id's that the ADT implicitly define.
adtsToFuncIds :: (HasSortIds e)
              => e -> [ADTDecl] -> CompilerM [(FuncDefInfo, FuncId)]
adtsToFuncIds e ds = concat <$> traverse (adtToFuncIds e) ds

adtToFuncIds :: (HasSortIds e)
             => e -> ADTDecl -> CompilerM [(FuncDefInfo, FuncId)]
adtToFuncIds e a = do
    sId <- findSortIdM e (adtName a, nodeLoc a)
    concat <$> traverse (cstrToFuncIds e sId) (constructors a)

cstrToFuncIds :: (HasSortIds e)
              => e -> SortId -> CstrDecl -> CompilerM [(FuncDefInfo, FuncId)]
cstrToFuncIds e sId c =
    concat <$> sequence [ mkCstrFdiFid
                        , isCstrFdiFid
                        , cstrAccessFdiFid
                        , eqFdiFid
                        , neqFdiFid
                        , toStrFdiFid
                        , fromStrFdiFid
                        , toXMLFdiFid
                        , fromXMLFdiFid
                        ]
    where
      cn = cstrName c
      mkCstrFdiFid = do
          mkCstrId <- getNextId
          fSids <- traverse (findSortIdM e . fieldSort) (cstrFields c)
          -- Create a function id for the constructor function.
          -- TODO: sharing the ID should be fine.
          return [(IDefUid cn mkCstrId, FuncId cn (Id mkCstrId) fSids sId)]
      isCstrFdiFid = do
          let isN = "is" <> cn
          isCstrId <- getNextId
          -- Create a function id for the constructor function.
          -- TODO: sharing the ID should be fine.
          return [(IDefUid isN isCstrId, FuncId isN (Id isCstrId) [sId] sortIdBool)]
      cstrAccessFdiFid = traverse accessFdiFid (cstrFields c)
          where
            accessFdiFid f = do
                let accN = fieldName f
                accSid <- getNextId
                fSid   <- findSortIdM e (fieldSort f)
                return (IDefUid accN accSid, FuncId accN (Id accSid) [sId] fSid)
      eqFdiFid =
          mkPredef eqName [sId, sId] sortIdBool
      mkPredef n aSids rSid =
          (\i -> [(IDefUid n i, FuncId n (Id i) aSids rSid)]) <$> getNextId
      neqFdiFid =
          mkPredef neqName [sId, sId] sortIdBool
      toStrFdiFid =
          mkPredef toStringName [sId] sortIdString
      fromStrFdiFid =
          mkPredef fromStringName [sortIdString] sId
      toXMLFdiFid =
          mkPredef toXmlName [sId] sortIdString
      fromXMLFdiFid =
          mkPredef fromXmlName [sortIdString] sId

