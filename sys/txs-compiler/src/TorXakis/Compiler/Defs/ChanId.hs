{-# LANGUAGE FlexibleContexts      #-}
module TorXakis.Compiler.Defs.ChanId where

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
    where cIds = [ chanIdExit
                 , chanIdIstep
                 , chanIdQstep
                 , chanIdHit
                 , chanIdMiss
                 ]
