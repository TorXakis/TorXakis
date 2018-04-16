{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module TorXakis.Compiler.Data where

import           Control.Arrow             (left, (|||))
import           Control.Lens              ((&), (.~))
import           Control.Monad.Error.Class (MonadError, catchError, liftEither,
                                            throwError)
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

import           TorXakis.Compiler.Error
import           TorXakis.Parser.Data      hiding (St, nextId)

-- | Single environment
newtype SEnv t = SEnv { fromSEnv :: t}

-- | Incremental environment, to allow the compiler to fill in the environment
-- in several passes.
data IEnv f0 f1 f2 f3 f4 f5 f6 = IEnv
    { sortIdT    :: f0
    , cstrIdT    :: f1
    , varSortIdT :: f2
    , varIdT     :: f3
    , varDeclT   :: f4
    , funcIdT    :: f5
    , funcDefT   :: f6
    }

emptyEnv :: IEnv () () () () () () ()
emptyEnv = IEnv () () () () () () ()

class HasVarDecls e where
    -- | Find the variable declaration or function declaration that corresponds
    -- to parser-location of a variable reference.
    --
    -- An identifier "x" could refer to several functions (since 'TorXakis'
    -- allows operator overloading), and that's why this function can return a
    -- list of possible function declarations, as well as the location of a
    -- variable declarations.
    --
    -- Note also that constants define (nullary) functions.
    --
    -- For now variables only occur in expressions.
    --
    -- TODO: rename this to something like 'findIdentDecl' or something like that
    findVarDecl :: e -> Loc VarRefE
                -> Either Error (Loc VarDeclE :| [FuncDefInfo])

    findFuncDecl :: e -> Loc VarRefE -> Either Error [FuncDefInfo]
    findFuncDecl e l =
        Left ||| const (Left err) ||| Right $ findVarDecl e l
       where
         err = Error
             { _errorType = FunctionNotDefined
             , _errorLoc  = getErrorLoc l
             , _errorMsg  = "Could not function declaration."
             }

filterByReturnSort :: HasFuncIds e
                   => e
                   -> SortId
                   -> [FuncDefInfo]
                   -> [FuncDefInfo]
filterByReturnSort e sId fdis = filter (fidHasReturnSort e sId) fdis

fidHasReturnSort :: HasFuncIds e
                   => e
                   -> SortId
                   -> FuncDefInfo
                   -> Bool
fidHasReturnSort e sId fdi = const False ||| id $ do
    fId <- findFuncId e fdi
    return $ funcsort fId == sId


-- | Get the function definition if the given list is a singleton, return an
-- error otherwise.
getUniqueElement :: Show a => [a] -> Either Error a
getUniqueElement [fdi] = Right fdi
getUniqueElement [] = Left Error
    { _errorType = UndefinedRef
    , _errorLoc  = NoErrorLoc
    , _errorMsg  = "Could not find an element."
    }
getUniqueElement xs = Left Error
    { _errorType = UnresolvedIdentifier
    , _errorLoc  = NoErrorLoc
    , _errorMsg  = "Found multiple elements: " <> T.pack (show xs)
    }

-- | Select the function definitions that matches the given arguments and return
-- types.
determineF :: HasFuncIds e
           => e
           -> [FuncDefInfo]
           -> [SortId]
           -> Maybe SortId  -- ^ Return Sort, if known.
           -> [FuncDefInfo]
determineF e fdis aSids mRSid =
    filter funcMatches fdis
    where
      funcMatches :: FuncDefInfo -> Bool
      funcMatches fdi = const False ||| id $ do
          fId <- findFuncId e fdi
          return $ funcargs fId == aSids &&
                   fromMaybe True ((funcsort fId ==) <$> mRSid)
      -- err = Error
      --     { _errorType = UndefinedRef
      --     , _errorLoc  = NoErrorLoc
      --     , _errorMsg  = "Could not find a function with arguments of type "
      --                 <> T.pack (show aSids)
      --                 <> maybe "" ((" and return type of " <>) . T.pack . show) mRSid
      --     }

-- | Information about a function definition. Functions are defined either
-- explicitly by the user, or by the compiler.
--
-- Implicit functions include equality, arithmetic and boolean functions, as
-- well as one of the implicit definitions introduced by the constructor of an
-- ADT.
--
-- A constructor of an ADT introduces multiple functions:
--
-- - The constructor functions itself.
-- - The "is constructor" function.
-- - The accessor functions.
-- - The equal and not equal functions.
-- - The to/from String/XML functions.
--
data FuncDefInfo = IDefUid  Text Int        -- ^ Name and unique identifier of an implicitly defined function.
                 | FDefLoc  (Loc FuncDeclE) -- ^ Location of a function declaration.
                 deriving (Eq, Ord, Show)

fdiLoc :: FuncDefInfo -> Maybe (Loc FuncDeclE)
fdiLoc IDefUid {}  = Nothing
fdiLoc (FDefLoc l) = Just l

fdiName :: FuncDefInfo -> Maybe Text
fdiName (IDefUid t _) = Just t
fdiName (FDefLoc _)   = Nothing

-- TODO determine the right kind of precedence.
infixr 5 :|
 -- TODO: you might want to replace the 'Either's by ':|'.
type (:|) = Either

class HasFuncIds e where
    -- | Find the function id that corresponds to the given parser location,
    -- and expected arguments and return type.
    --
    -- A function is uniquely determined by its name and arguments and return
    -- types that are associated to it.
    --
    findFuncId :: e
               -> FuncDefInfo
               -> Either Error FuncId
    findFuncIdM :: e
                -> FuncDefInfo
                -> CompilerM FuncId
    findFuncIdM e i = liftEither $ findFuncId e i

    -- | Find the @FuncId@ for a function declaration.
    findFuncIdForDecl :: e
                      -> Loc FuncDeclE
                      -> Either Error FuncId

    findFuncIdForDeclM :: e
                       -> Loc FuncDeclE
                       -> CompilerM FuncId
    findFuncIdForDeclM e l = liftEither $ findFuncIdForDecl e l

    -- | Get a list of implicit definitions names.
    idefsNames :: e -> [Text]

class HasFuncDefs e where
    -- | Find the function definition that corresponds with a given function id.
    findFuncDef :: e -> FuncId -> Either Error (FuncDef VarId)
    getFuncDefT :: e -> Map FuncId (FuncDef VarId)

lookup :: (Ord a, Show a) => a -> Map a b -> Text -> Either Error b
lookup a ab what =  maybeToEither err . Map.lookup a $ ab
        where err = Error
                  { _errorType = UndefinedRef
                  , _errorLoc  = NoErrorLoc -- TODO: is it OK that we cannot give a location error here?
                  , _errorMsg  = "Could not find " <> what
                  }

lookupM :: (Ord a, Show a) => a -> Map a b -> Text -> CompilerM b
lookupM a ab what = liftEither $ lookup a ab what

lookupWithLoc :: (Ord a, Show a, HasErrorLoc a) => a -> Map a b -> Text -> Either Error b
lookupWithLoc a ab what = maybeToEither err . Map.lookup a $ ab
    where err = Error
              { _errorType = UndefinedRef
              , _errorLoc  = getErrorLoc a
              , _errorMsg  = "Could not find " <> what
              }

lookupWithLocM :: (Ord a, Show a, HasErrorLoc a) => a -> Map a b -> Text -> CompilerM b
lookupWithLocM a ab what = liftEither $ lookupWithLoc a ab what

instance HasVarDecls (IEnv f0 f1 f2 f3 (Map (Loc VarRefE) (Loc VarDeclE :| [FuncDefInfo])) f5 f6) where
    findVarDecl IEnv{varDeclT = vm} i = lookupWithLoc i vm "variable declaration by parser location id"

instance HasFuncIds (IEnv f0 f1 f2 f3 f4 (Map FuncDefInfo FuncId) f6) where
    findFuncId IEnv {funcIdT = fm} fdi = lookupWithLoc fdi fm "function id by parser location id"
    findFuncIdForDecl e@IEnv{funcIdT = fm} fl =
        case find ((Just fl ==) . fdiLoc) (Map.keys fm) of
            Nothing  -> Left Error
                { _errorType = UndefinedRef
                , _errorLoc  = getErrorLoc fl
                , _errorMsg  = "Could not find function definition for the given location"
                }
            Just fdi -> findFuncId e fdi
    idefsNames IEnv {funcIdT = fm} = catMaybes $ fdiName <$> Map.keys fm

findFuncSortIds :: HasFuncIds e => e -> [FuncDefInfo] -> Either Error [SortId]
findFuncSortIds e fdis = fmap funcsort <$> traverse (findFuncId e) fdis

instance HasFuncDefs (IEnv f0 f1 f2 f3 f4 f5 (Map FuncId (FuncDef VarId))) where
    findFuncDef IEnv{funcDefT = fm} = findFuncDef (SEnv fm)
    getFuncDefT IEnv{funcDefT = fm} = getFuncDefT (SEnv fm)

instance HasFuncDefs (SEnv (Map FuncId (FuncDef VarId))) where
    findFuncDef (SEnv fm) i = lookup i fm (T.pack . show $ i )
    getFuncDefT (SEnv fm) = fm

instance Ord a => Monoid (SEnv (Map a b)) where
    mempty = SEnv Map.empty
    SEnv fm0 `mappend` SEnv fm1 = SEnv (fm0 `mappend` fm1)

instance Ord a => IsList (SEnv (Map a b)) where
    type Item (SEnv (Map a b)) = (a, b)

    fromList = SEnv . fromList
    toList   = toList . fromSEnv

isMemberOf :: Ord a => a -> SEnv (Map a b) -> Bool
isMemberOf a (SEnv ab) = Map.member a ab

newtype St = St { nextId :: Int } deriving (Eq, Show)

newState :: St
-- '1000' was chosen for compatibility with the current TorXakis compiler.
newState = St 1000

newtype CompilerM a = CompilerM { runCompiler :: StateT St (Either Error) a }
    deriving (Functor, Applicative, Monad, MonadState St, MonadError Error)

getNextId :: CompilerM Int
getNextId = do
    i <- nextId <$> get
    put (St $ i + 1)
    return i

instance HasErrorLoc FuncDefInfo where
    getErrorLoc fdi = fromMaybe NoErrorLoc (getErrorLoc <$> fdiLoc fdi)

-- | Set the error location.
(<!>) :: HasErrorLoc l => Either Error a -> l -> Either Error a
(<!>) ea l = left (errorLoc .~ getErrorLoc l) ea

-- | Set the error location (monadic version).
(<!!>) :: HasErrorLoc l => CompilerM a -> l -> CompilerM a
m <!!> l = catchError m $ throwError . (errorLoc .~ getErrorLoc l)
