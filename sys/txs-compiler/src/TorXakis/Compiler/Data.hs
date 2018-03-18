{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
module TorXakis.Compiler.Data where

import           Prelude hiding (lookup)
import           Control.Monad.State  (MonadState, StateT, put, get)
import           Data.Map             (Map)
import qualified Data.Map as Map
import           Data.Text            (Text)
import qualified Data.Text as T
import           Data.Semigroup ((<>))
import           Data.Either.Utils (maybeToEither)
import           Control.Monad.Error.Class (MonadError, liftEither)

import           FuncId                        (FuncId)
import           FuncDef                       (FuncDef)
import           VarId                         (VarId)
import           SortId                        (SortId)
import           CstrId                        (CstrId)

import           TorXakis.Parser.Data hiding (St, nextId)
import           TorXakis.Compiler.Error

-- | Incremental environment, to allow the compiler to fill in the environment
-- in several passes.
data IEnv f0 f1 f2 f3 f4 f5 = IEnv
    { sortIdT   :: f0
    , cstrIdT   :: f1
    , varIdT    :: f2
    , varDeclT   :: f3
    , funcIdT   :: f4
    , funcDefT  :: f5
    }

emptyEnv :: IEnv () () () () () ()
emptyEnv = IEnv () () () () () ()

class HasSortIds e where
    -- | Find the `SortId` that corresponds to the given name. This assumes
    -- that sort names are unique.
    findSortId  :: e -> (Text, Metadata t) -> Either Error SortId
    findSortIdM :: e -> (Text, Metadata t) -> CompilerM SortId
    findSortIdM e t = liftEither $ findSortId e t
    getSortIdMap :: e -> Map Text SortId
    allSortIds :: e -> [SortId]
    allSortIds e = Map.elems $ getSortIdMap e
class HasCstrIds e where
    -- | Find the `CstrId` that correspond to the parser location of a
    -- constructor declaration.
    -- TODO: make this type-safe!
    -- Something like Map (Loc Var) VarId
    -- So that you cannot use this with the location of anything else.
    findCstrId :: e -> Loc Cstr -> Either Error CstrId
    findCstrIdM :: e -> Loc Cstr -> CompilerM CstrId
    findCstrIdM e i = liftEither $ findCstrId e i

class HasVarIds e where
    -- | Find the variable id that corresponds to the given parser location.
    --
    -- For now only field id's can define new `VarId`s.
    findVarId :: e -> Loc Field -> Either Error VarId
    findVarIdM :: e -> Loc Field -> CompilerM VarId
    findVarIdM e i = liftEither $ findVarId e i

class HasVarDecls e where
    -- | Find the field declaration that corresponds to parser-location of a
    -- variable use.
    --
    -- For now variables only occur in expressions.
    findVarDecl :: e -> Loc Exp -> Either Error FieldDecl
    findVarDeclM :: e -> Loc Exp -> CompilerM  FieldDecl
    findVarDeclM e i = liftEither $ findVarDecl e i

class HasFuncIds e where
    -- | Find the function id that corresponds to the given parser location.
    --
    findFuncId :: e -> Loc Func -> Either Error FuncId

class HasFuncDefs e where
    -- | Find the function definition that corresponds with a given function id.
    findFuncDef :: e -> FuncId -> Either Error (FuncDef VarId)
    getFuncDefT :: e -> Map FuncId (FuncDef VarId)

instance HasSortIds (IEnv (Map Text SortId) f1 f2 f3 f4 f5) where
    findSortId IEnv{sortIdT = sm} (t, m) = maybeToEither err . Map.lookup t $ sm
        where err = (T.pack . show) m <> ": Could not find sort " <> t

    getSortIdMap IEnv{sortIdT = sm} = sm

lookup :: (Ord a, Show a) => a -> Map a b -> Text -> Either Error b
lookup a ab what = maybeToEither err . Map.lookup a $ ab
    where err = "Could not find " <> what <> "(" <> T.pack (show a) <> ")"

lookupM :: (Ord a, Show a) => a -> Map a b -> Text -> CompilerM b
lookupM a ab what = liftEither $ lookup a ab what
    
instance HasCstrIds (IEnv f0 (Map (Loc Cstr) CstrId) f2 f3 f4 f5) where
    findCstrId IEnv{cstrIdT = cm} i = lookup i cm "constructor by parser location id "

instance HasVarIds (IEnv f0 f1 (Map (Loc Field) VarId) f3 f4 f5) where
    findVarId IEnv{varIdT = vm} i = lookup i vm "variable by parser location id"

instance HasVarDecls (IEnv f0 f1 f2 (Map (Loc Exp) FieldDecl) f4 f5) where
    findVarDecl IEnv{varDeclT = vm} i = lookup i vm "variable declaration by parser location id" 
    
instance HasFuncIds (IEnv f0 f1 f2 f3 (Map (Loc Func) FuncId) f5) where
    findFuncId IEnv {funcIdT = fm} i = lookup i fm "function id by parser location id"

instance HasFuncDefs (IEnv f0 f1 f2 f3 f4 (Map FuncId (FuncDef VarId))) where
    findFuncDef IEnv{funcDefT = fm} i = lookup i fm "function declaration by function id"
    getFuncDefT IEnv{funcDefT = fm} = fm

newtype St = St { nextId :: Int } deriving (Eq, Show)

newState :: St
newState = St 1000

newtype CompilerM a = CompilerM { runCompiler :: StateT St (Either Error) a }
    deriving (Functor, Applicative, Monad, MonadState St, MonadError Error)

getNextId :: CompilerM Int
getNextId = do
    i <- nextId <$> get
    put (St $ i + 1)
    return i
