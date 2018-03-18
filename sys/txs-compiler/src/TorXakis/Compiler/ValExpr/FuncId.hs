{-# LANGUAGE OverloadedStrings          #-}
module TorXakis.Compiler.ValExpr.FuncId where

-- TODO: consider adding your own prelude.
import           Data.Semigroup ((<>))

import           Id (Id (Id))
import           SortId (sortIdBool)
import           CstrId (name, cstrsort, CstrId)
import           FuncId (FuncId (FuncId))

import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.ValExpr.SortId
    
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
    -- TODO: you might consider making the parse tree a bit more type-safer.
    -- Here we could be looking up the name of anything.
    sId <- findSortIdM e (nodeNameT . child $ f)
    return $ FuncId (nodeNameT f) (Id fId) [cstrsort cId] sId

funcDeclToFuncId :: HasSortIds e
                 => e -> FuncDecl -> CompilerM FuncId
funcDeclToFuncId  e f = do
    fId   <- getNextId
    aSids <- traverse (sortIdOfFieldDeclM e) (funcParams . child $ f)
    rSid  <- findSortIdM e (nodeNameT . funcRetType . child $ f)
    return $ FuncId (nodeNameT f) (Id fId) aSids rSid
