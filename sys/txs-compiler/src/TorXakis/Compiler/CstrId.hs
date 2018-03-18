{-# LANGUAGE OverloadedStrings          #-}
module TorXakis.Compiler.CstrId where

import           Data.Text (Text)
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
import           TorXakis.Compiler.SortId
import           TorXakis.Compiler.Error

compileToCstrId :: (HasSortIds e, HasSortIds e)
                => e -> [ADTDecl] -> CompilerM (Map (Loc Cstr) CstrId)
compileToCstrId e ds = Map.fromList . concat <$>
    traverse (adtToCstrId e) ds

adtToCstrId :: (HasSortIds e, HasSortIds e)
            => e
            -> ADTDecl
            -> CompilerM [(Loc Cstr, CstrId)]
adtToCstrId e a = do
    sId <- findSortIdM e (nodeNameT a)
    traverse (cstrToCstrId e sId) (child a)

cstrToCstrId :: (HasSortIds e)
             => e
             -> SortId -- ^ SortId of the containing ADT.
             -> CstrDecl
             -> CompilerM (Loc Cstr, CstrId)
cstrToCstrId e sId c = do
    i <- getNextId
    aSids <- traverse (findSortIdM e . nodeNameT . child) (child c)
    return (getLoc c, CstrId (nodeNameT c) (Id i) aSids sId)

cstrIdOfCstrDecl :: HasCstrIds e => e -> CstrDecl -> Either Error CstrId
cstrIdOfCstrDecl e c = left (const err) $ findCstrId e (loc . nodeMdata $ c)
    where err = "Could not find constructor " <> nodeNameT c

cstrIdOfCstrDeclM :: HasCstrIds e => e -> CstrDecl -> CompilerM CstrId
cstrIdOfCstrDeclM e c = liftEither $ cstrIdOfCstrDecl e c
