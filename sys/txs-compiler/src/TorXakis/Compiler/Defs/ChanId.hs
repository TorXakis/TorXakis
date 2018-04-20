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

-- | Create a mapping from channel names to channel id's.
chanDeclsToChanIds :: (MapsTo Text SortId mm)
                   => mm -> [ChanDecl] -> CompilerM [(Text, ChanId)]
chanDeclsToChanIds mm chs = do
    chIds <- traverse chanDeclToChanIds chs
    return $ zip (chanDeclName <$> chs) chIds
    where
      chanDeclToChanIds ch = do
          chId   <- getNextId
          chSids <- traverse (mm .@!!) (chanDeclSorts ch)
          return $ ChanId (chanDeclName ch) (Id chId) chSids

predefinedChans :: [(Text, ChanId)]
predefinedChans =
    zip (name <$> cIds) cIds
    where cIds = [ chanIdIstep
                 , chanIdQstep
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

chRefsToIds :: MapsTo Text ChanId mm 
            => mm -> [ChanRef] -> CompilerM [ChanId]
chRefsToIds mm chs = traverse (`lookupM` mm) (chanRefName <$> chs)

chRefsToChIdSet :: MapsTo Text ChanId mm 
                => mm -> Set ChanRef -> CompilerM (Set ChanId)
chRefsToChIdSet mm = fmap Set.fromList . chRefsToIds mm . Set.toList
