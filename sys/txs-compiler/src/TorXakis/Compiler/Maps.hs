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
import           Data.Typeable             (Typeable)
import           GHC.Exts                  (IsList, Item, fromList, toList)
import           Prelude                   hiding (lookup)

import           ChanId                    (ChanId (ChanId), name)
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
findSortId mm (t, l) = left (errorMsg .~ msg) $ lookup t mm <!> l
    where
      msg = "Could not find sort " <> t

findSortIdM :: MapsTo Text SortId mm
           => mm -> (Text, Loc t) -> CompilerM SortId
findSortIdM mm (t, l) = liftEither $ findSortId mm (t, l)

filterByReturnSort :: MapsTo FuncDefInfo FuncId mm
                   => mm
                   -> SortId
                   -> [FuncDefInfo]
                   -> [FuncDefInfo]
filterByReturnSort mm sId fdis = filter (fidHasReturnSort mm sId) fdis

fidHasReturnSort :: MapsTo FuncDefInfo FuncId mm
                   => mm
                   -> SortId
                   -> FuncDefInfo
                   -> Bool
fidHasReturnSort mm sId fdi = const False ||| id $ do
    fId <- lookup fdi mm
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
determineF :: MapsTo FuncDefInfo FuncId mm
           => mm
           -> [FuncDefInfo]
           -> [SortId]
           -> Maybe SortId  -- ^ Return Sort, if known.
           -> [FuncDefInfo]
determineF mm fdis aSids mRSid =
    filter funcMatches fdis
    where
      funcMatches :: FuncDefInfo -> Bool
      funcMatches fdi = const False ||| id $ do
          fId <- lookup fdi mm
          return $ funcargs fId == aSids &&
                   fromMaybe True ((funcsort fId ==) <$> mRSid)

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

findFuncDecl :: MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
             => mm -> Loc VarRefE -> Either Error [FuncDefInfo]
findFuncDecl mm l =
    Left ||| cErr ||| Right $ lookup l mm
    where
      cErr :: Loc VarDeclE -> Either Error a
      cErr _ = Left err
      err = Error
            { _errorType = FunctionNotDefined
            , _errorLoc  = getErrorLoc l
            , _errorMsg  = "Could not function declaration."
            }

findFuncSortIds :: MapsTo FuncDefInfo FuncId mm
                => mm -> [FuncDefInfo] -> Either Error [SortId]
findFuncSortIds mm fdis = fmap funcsort <$> traverse (`lookup` mm) fdis

instance HasErrorLoc FuncDefInfo where
    getErrorLoc fdi = fromMaybe NoErrorLoc (getErrorLoc <$> fdiLoc fdi)

findFuncIdForDecl :: MapsTo FuncDefInfo FuncId mm
                  => mm -> Loc FuncDeclE -> Either Error FuncId
findFuncIdForDecl mm fl =
    case find ((Just fl ==) . fdiLoc) (Map.keys im) of
        Nothing  -> Left Error
            { _errorType = UndefinedRef
            , _errorLoc  = getErrorLoc fl
            , _errorMsg  = "Could not find function definition for the given location"
            }
        Just fdi -> lookup fdi mm
    where
      im :: Map FuncDefInfo FuncId
      im = innerMap mm

findFuncIdForDeclM :: MapsTo FuncDefInfo FuncId mm
                  => mm -> Loc FuncDeclE -> CompilerM FuncId
findFuncIdForDeclM mm fl = liftEither $ findFuncIdForDecl mm fl

idefsNames :: MapsTo FuncDefInfo FuncId mm => mm -> [Text]
idefsNames mm = catMaybes $ fdiName <$> Map.keys fm
    where
      fm :: Map FuncDefInfo FuncId
      fm = innerMap mm

-- | Set the error location.
(<!>) :: HasErrorLoc l => Either Error a -> l -> Either Error a
(<!>) ea l = left (errorLoc .~ getErrorLoc l) ea

-- | Set the error location (monadic version).
(<!!>) :: HasErrorLoc l => CompilerM a -> l -> CompilerM a
m <!!> l = catchError m $ throwError . (errorLoc .~ getErrorLoc l)

(.@@) :: (HasErrorLoc k, MapsTo k v mm, Ord k, Show k
         , Typeable k, Typeable v)
      => mm -> k -> Either Error v
mm .@@ k = lookup k mm <!> k

(.@) :: ( HasErrorLoc k, MapsTo k v mm, Ord k, Show k
        , Typeable k, Typeable v)
     => mm -> k -> CompilerM v
mm .@ k = lookupM k mm <!!> k

(.@!!) :: ( HasErrorLoc l, MapsTo k v mm, Ord k, Show k
          , Typeable k, Typeable v )
     => mm -> (k, l) -> CompilerM v
mm .@!! (k, l) = lookupM k mm <!!> l

chRefsToIds :: MapsTo Text ChanId mm
            => mm -> [ChanRef] -> CompilerM [ChanId]
chRefsToIds mm chs = traverse (`lookupM` mm) (chanRefName <$> chs)
