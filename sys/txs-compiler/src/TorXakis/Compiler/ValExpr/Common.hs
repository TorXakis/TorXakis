{-# LANGUAGE FlexibleContexts  #-}

-- | This module was introduced due to a common dependency between SortId and
-- ChanId. The problem is that we have to introduce new channel ID's at several
-- places due to the use of the 'HIDE' operator. This module won't be needed
-- once we introduce two maps:
--
-- - Text -> Loc ChanDeclE
-- - Loc ChanDeclE -> ChanId
--
module TorXakis.Compiler.ValExpr.Common where

import           Data.Text                 (Text)

import           SortId (SortId)
import           ChanId (ChanId (ChanId))
import           Id                        (Id (Id))

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Maps
import           TorXakis.Parser.Data

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
