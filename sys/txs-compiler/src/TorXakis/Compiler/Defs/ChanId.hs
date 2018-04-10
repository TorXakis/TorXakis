module TorXakis.Compiler.Defs.ChanId where

import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Text              (Text)

import           ChanId                 (ChanId (ChanId))
import           Id                     (Id (Id))

import           TorXakis.Compiler.Data
import           TorXakis.Parser.Data

-- | Create a mapping from channel names to channel id's.
chanDeclsToChanIds :: (HasSortIds e) => e -> [ChanDecl] -> CompilerM (Map Text ChanId)
chanDeclsToChanIds e chs = do
    chIds <- traverse chanDeclToChanIds chs
    return $ Map.fromList $ zip (chanDeclName <$> chs) chIds
    where
      chanDeclToChanIds ch = do
          chId   <- getNextId
          chSids <- traverse (findSortIdM e) (chanDeclSorts ch)
          return $ ChanId (chanDeclName ch) (Id chId) chSids

