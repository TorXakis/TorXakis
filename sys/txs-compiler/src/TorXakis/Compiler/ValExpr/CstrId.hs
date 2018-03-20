{-# LANGUAGE OverloadedStrings          #-}
module TorXakis.Compiler.ValExpr.CstrId where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Arrow (left)
import           Control.Monad.Error.Class (liftEither)
import           Data.Semigroup ((<>))

import           Id (Id (Id))
import           SortId (SortId)
import           CstrId (CstrId (CstrId))

import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error

compileToCstrId :: (HasSortIds e, HasSortIds e)
                => e -> [ADTDecl] -> CompilerM (Map (Loc CstrE) CstrId)
compileToCstrId e ds = Map.fromList . concat <$>
    traverse (adtToCstrId e) ds

adtToCstrId :: (HasSortIds e, HasSortIds e)
            => e
            -> ADTDecl
            -> CompilerM [(Loc CstrE, CstrId)]
adtToCstrId e a = do
    sId <- findSortIdM e (adtName a, nodeMdata a)
    traverse (cstrToCstrId e sId) (constructors a)

cstrToCstrId :: (HasSortIds e)
             => e
             -> SortId -- ^ SortId of the containing ADT.
             -> CstrDecl
             -> CompilerM (Loc CstrE, CstrId)
cstrToCstrId e sId c = do
    i <- getNextId
    aSids <- traverse (findSortIdM e . fieldSort) (cstrFields c)
    return (getLoc c, CstrId (cstrName c) (Id i) aSids sId)

cstrIdOfCstrDecl :: HasCstrIds e => e -> CstrDecl -> Either Error CstrId
cstrIdOfCstrDecl e c = left (const err) $ findCstrId e (getLoc c)
    where err = "Could not find constructor " <> cstrName c

cstrIdOfCstrDeclM :: HasCstrIds e => e -> CstrDecl -> CompilerM CstrId
cstrIdOfCstrDeclM e c = liftEither $ cstrIdOfCstrDecl e c
