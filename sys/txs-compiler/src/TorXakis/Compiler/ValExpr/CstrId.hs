{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module TorXakis.Compiler.ValExpr.CstrId where

import Prelude hiding (lookup)
import           Control.Arrow             (left)
import           Control.Monad.Error.Class (liftEither)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Semigroup            ((<>))
import Data.Text (Text)

import           CstrId                    (CstrId (CstrId))
import           Id                        (Id (Id))
import           SortId                    (SortId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Error
import           TorXakis.Parser.Data

compileToCstrId :: (MapsTo Text SortId mm)
                => mm -> [ADTDecl] -> CompilerM (Map (Loc CstrE) CstrId)
compileToCstrId mm ds = Map.fromList . concat <$>
    traverse (adtToCstrId mm) ds

adtToCstrId :: (MapsTo Text SortId mm)
            => mm
            -> ADTDecl
            -> CompilerM [(Loc CstrE, CstrId)]
adtToCstrId mm a = do
    sId <- findSortIdM mm (adtName a, nodeLoc a)
    traverse (cstrToCstrId mm sId) (constructors a)

cstrToCstrId :: (MapsTo Text SortId mm)
             => mm
             -> SortId -- ^ SortId of the containing ADT.
             -> CstrDecl
             -> CompilerM (Loc CstrE, CstrId)
cstrToCstrId mm sId c = do
    i <- getNextId
    aSids <- traverse (findSortIdM mm . fieldSort) (cstrFields c)
    return (getLoc c, CstrId (cstrName c) (Id i) aSids sId)

cstrIdOfCstrDecl :: MapsTo (Loc CstrE) CstrId mm => mm -> CstrDecl -> Either Error CstrId
cstrIdOfCstrDecl mm c = lookup (getLoc c) mm

cstrIdOfCstrDeclM :: MapsTo (Loc CstrE) CstrId mm => mm -> CstrDecl -> CompilerM CstrId
cstrIdOfCstrDeclM mm c = liftEither $ cstrIdOfCstrDecl mm c
