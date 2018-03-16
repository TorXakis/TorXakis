{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module TorXakis.Compiler.Data where

import           Control.Monad.Reader (ReaderT)
import           Control.Monad.State  (State, MonadState, StateT)
import           Data.Map             (Map)
import qualified Data.Map as Map
import           Data.Text            (Text)
import           Data.Semigroup ((<>))
import           Data.Either.Utils (maybeToEither)
import           Control.Monad.State (StateT, put, get)
import           Control.Monad.Error.Class (MonadError, throwError)

import           FuncId                        (FuncId)
import           FuncDef                        (FuncDef)
import           VarId                         (VarId)
import           Id                            (Id (Id))
import           SortId                        (SortId (SortId))
import           CstrId                        (CstrId (CstrId))
import           TorXakis.Sort.ADTDefs         (Sort)
import           TorXakis.Sort.ConstructorDefs (ConstructorDef)

import           TorXakis.Parser.Data (CstrDecl, uid, nodeMdata, nodeName, FieldDecl)
import           TorXakis.Compiler.Error

data Env = Env
    { sortsMap :: Map Text SortId
    , cstrsMap :: Map Int  CstrId
      -- | Map a variable-definition parser-location to a variable id.
      -- 
      -- TODO: make this type-safe!
      -- Something like Map (Loc Var) VarId
      -- So that you cannot use this with the location of anything else.
    , varDefMap :: Map Int  VarId
      -- | Map a variable usage parser-location to the variable-definition parse location.
    , varUseMap :: Map Int FieldDecl
      -- | Map a function definition location id to the function id.
    , fIdMap :: Map Int FuncId
    , fDefMap :: Map FuncId (FuncDef VarId)
    }

emptyEnv :: Env
emptyEnv = Env Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty
    
findSort :: Env -> Text -> Either Error SortId
findSort e s = maybeToEither err $
    Map.lookup s (sortsMap e)
    where err = "Could not find sort " <> s

findSortM :: Env -> Text -> CompilerM SortId
findSortM e s =
    case Map.lookup s (sortsMap e) of
        Nothing  -> throwError $ "Could not find sort " <> s
        Just sId -> return sId

findCstr :: Env -> CstrDecl -> Either Error CstrId
findCstr e c = maybeToEither err $
    Map.lookup (uid . nodeMdata $ c) (cstrsMap e)
    where err = "Could not find constructor " <> nodeName c

findCstrM :: Env -> CstrDecl -> CompilerM CstrId
findCstrM e c =
    case Map.lookup (uid . nodeMdata $ c) (cstrsMap e) of
        Nothing  -> throwError $ "Could not find constructor " <> nodeName c
        Just sId -> return sId

findVarDefM :: Env -> FieldDecl -> CompilerM VarId
findVarDefM e f =
    case Map.lookup (uid . nodeMdata $ f) (varDefMap e) of
        Nothing  -> throwError $ "Could not find variable " <> nodeName f
        Just vId -> return vId

findVarUseM :: Env -> Int -> CompilerM VarId
findVarUseM e i =
    case Map.lookup i (varUseMap e) of
        Nothing -> throwError "Could not find variable "
        Just f  -> findVarDefM e f

findFuncId :: Env -> Int -> Either Error FuncId
findFuncId e i = maybeToEither err $
    Map.lookup i (fIdMap e)
    where err =  "Could not find function id"

newtype St = St { nextId :: Int } deriving (Eq, Show)

newtype CompilerM a = CompilerM { runCompiler :: StateT St (Either Error) a }
    deriving (Functor, Applicative, Monad, MonadState St, MonadError Error)

getNextId :: CompilerM Int
getNextId = do
    i <- nextId <$> get
    put (St $ i + 1)
    return i
