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
import           StdTDefs                         (fromStringName, stdFuncTable,
                                                   toStringName)
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
getStdFuncIds :: CompilerM [(PredefName, FuncId)]
getStdFuncIds = concat <$>
    traverse signHandlerTofuncIds (Map.toList (toMap stdFuncTable))

signHandlerTofuncIds :: (Text, SignHandler VarId) -> CompilerM [(PredefName, FuncId)]
signHandlerTofuncIds (n, sh) = fmap (PredefName n,) <$>
    traverse (signatureToFuncId n) (Map.keys sh)


signatureToFuncId :: Text -> Signature -> CompilerM FuncId
signatureToFuncId n (Signature aSids rSid) = do
    fId <- getNextId
    return $ FuncId n (Id fId) aSids rSid
