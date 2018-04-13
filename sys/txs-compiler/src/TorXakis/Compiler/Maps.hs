{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- | Compiler specific maps
module TorXakis.Compiler.Maps where

import           Control.Arrow             (left, (|||))
import           Control.Lens              (Lens', to, (%~), (.~), (^.))
import           Control.Monad.Error.Class (MonadError, liftEither)
import           Control.Monad.State       (MonadState, StateT, get, put)
import           Data.Either.Utils         (maybeToEither)
import           Data.List                 (find)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (catMaybes, fromMaybe)
import           Data.Monoid               (Monoid, (<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Exts                  (IsList, Item, fromList, toList)
import           Prelude                   hiding (lookup)

import           CstrId                    (CstrId)
import           FuncDef                   (FuncDef)
import           FuncId                    (FuncId, funcargs, funcsort)
import           SortId                    (SortId)
import           VarId                     (VarId)

import           TorXakis.Compiler.Data    (CompilerM)
import           TorXakis.Compiler.Error
import           TorXakis.Compiler.MapsTo
import           TorXakis.Parser.Data      hiding (St, nextId)

findSortId :: MapsTo Text SortId mm
           => mm -> (Text, Loc t) -> Either Error SortId
findSortId mm (t, l) = left (errorMsg .~ msg) $ lookupWithLoc (t, l) mm
    where
      msg = "Could not find sort " <> t

findSortIdM :: MapsTo Text SortId mm
           => mm -> (Text, Loc t) -> CompilerM SortId
findSortIdM mm (t, l) = liftEither $ findSortId mm (t, l)
