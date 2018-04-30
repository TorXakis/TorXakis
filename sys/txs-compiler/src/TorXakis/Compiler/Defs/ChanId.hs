{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings      #-}
module TorXakis.Compiler.Defs.ChanId where

import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Text              (Text)

import           ChanId                 (ChanId (ChanId), name)
import           Id                     (Id (Id))
import           SortId (SortId)
import           StdTDefs (chanIdExit, chanIdIstep, chanIdQstep, chanIdHit, chanIdMiss)

import           TorXakis.Compiler.Data
import           TorXakis.Parser.Data
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.ValExpr.SortId

predefinedChans :: [(Text, ChanId)]
predefinedChans =
    zip (name <$> cIds) cIds
    where cIds = [ chanIdIstep
                 , chanIdQstep
                 , chanIdExit
                 ]

class DeclaresChannels e where
    mkChanIds :: (MapsTo Text SortId mm) => mm -> e -> CompilerM [(Text, ChanId)]

instance DeclaresChannels ExitSortDecl where
    mkChanIds _ NoExitD = return []
    mkChanIds mm (ExitD xs) = do
        chId  <- getNextId
        eSids <- sortIds mm xs
        return [("EXIT", ChanId "EXIT" (Id chId) eSids)]
    mkChanIds _ HitD = return [ (name chanIdHit, chanIdHit)
                               , (name chanIdMiss, chanIdMiss)
                               ]

chRefsToChIdSet :: ( MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm 
                   , MapsTo (Loc ChanDeclE) ChanId mm )
                => mm -> Set ChanRef -> CompilerM (Set ChanId)
chRefsToChIdSet mm = fmap Set.fromList . chRefsToIds mm . Set.toList
