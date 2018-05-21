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

filterByReturnSort :: MapsTo (Loc FuncDeclE) FuncId mm
                   => mm
                   -> SortId
                   -> [Loc FuncDeclE]
                   -> [Loc FuncDeclE]
filterByReturnSort mm sId = filter (fidHasReturnSort mm sId)

fidHasReturnSort :: MapsTo (Loc FuncDeclE) FuncId mm
                   => mm
                   -> SortId
                   -> (Loc FuncDeclE)
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
determineF :: MapsTo (Loc FuncDeclE) FuncId mm
           => mm
           -> [Loc FuncDeclE]
           -> [SortId]
           -> Maybe SortId  -- ^ Return Sort, if known.
           -> [Loc FuncDeclE]
determineF mm fdis aSids mRSid =
    filter funcMatches fdis
    where
      funcMatches :: Loc FuncDeclE -> Bool
      funcMatches fdi = const False ||| id $ do
          fId <- lookup fdi mm
          return $ funcargs fId == aSids &&
                   fromMaybe True ((funcsort fId ==) <$> mRSid)

-- | Get the name of the implicit function declaration, if any.
fdiName :: Loc FuncDeclE -> Maybe Text
fdiName Loc {}          = Nothing
fdiName ExtraAut {}     = Nothing
fdiName (PredefLoc n _) = Just n


-- TODO determine the right kind of precedence.
infixr 5 :|
 -- TODO: you might want to replace the 'Either's by ':|'.
type (:|) = Either

-- | Find the (nullary) function declaration that corresponds to a variable
-- reference.
findFuncDecl :: MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
             => mm -> Loc VarRefE -> Either Error [Loc FuncDeclE]
findFuncDecl mm l = Left ||| cErr ||| Right $ lookup l mm
    where
      cErr :: Loc VarDeclE -> Either Error a
      cErr _ = Left err
      err = Error
            { _errorType = FunctionNotDefined
            , _errorLoc  = getErrorLoc l
            , _errorMsg  = "Could not function declaration."
            }

-- | Find the variable declaration that corresponds to a variable reference.
findVarDecl ::  MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
            => mm -> Loc VarRefE -> Either Error (Loc VarDeclE)
findVarDecl mm l = Left ||| Right ||| cErr $ lookup l mm
    where
      cErr :: [Loc FuncDeclE] -> Either Error a
      cErr _ = Left err
      err = Error
            { _errorType = UnresolvedIdentifier
            , _errorLoc  = getErrorLoc l
            , _errorMsg  = "Could not variable declaration."
            }

-- | Find the variable id that corresponds to a given variable reference.
findVarId :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
             , MapsTo (Loc VarDeclE) VarId mm )
          => mm -> Loc VarRefE -> Either Error VarId
findVarId mm vr = do
    vD  <- findVarDecl mm vr
    lookup vD mm

-- | Monadic version of @findVarId@.
findVarIdM :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
             , MapsTo (Loc VarDeclE) VarId mm )
          => mm -> Loc VarRefE -> CompilerM VarId
findVarIdM mm vr = liftEither $ findVarId mm vr

findFuncSortIds :: MapsTo (Loc FuncDeclE) FuncId mm
                => mm -> [Loc FuncDeclE] -> Either Error [SortId]
findFuncSortIds mm fdis = fmap funcsort <$> traverse (`lookup` mm) fdis

idefsNames :: MapsTo (Loc FuncDeclE) FuncId mm => mm -> [Text]
idefsNames mm = catMaybes $ fdiName <$> Map.keys fm
    where
      fm :: Map (Loc FuncDeclE) FuncId
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

lookupChId :: ( MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
              , MapsTo (Loc ChanDeclE) ChanId mm )
           => mm -> Loc ChanRefE -> CompilerM ChanId
lookupChId mm cr = do
    cd <- mm .@ cr :: CompilerM (Loc ChanDeclE)
    mm .@ cd

(.@!!) :: ( HasErrorLoc l, MapsTo k v mm, Ord k, Show k
          , Typeable k, Typeable v )
     => mm -> (k, l) -> CompilerM v
mm .@!! (k, l) = lookupM k mm <!!> l

chRefsToIds :: ( MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
               , MapsTo (Loc ChanDeclE) ChanId mm )
            => mm -> [ChanRef] -> CompilerM [ChanId]
chRefsToIds mm chs = traverse (lookupChId mm) (getLoc <$> chs)

closure2 :: (Ord k, Ord v) => Map k v -> Map v w -> Map k w
closure2 m0 m1 = foldl maybeAddPair Map.empty (Map.toList m0)
    where
      maybeAddPair acc (k, v) =
          maybe acc (\w -> Map.insert k w acc) (Map.lookup v m1)

-- | Channels referred in the model
usedChIdMap :: ( MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
               , MapsTo (Loc ChanDeclE) ChanId mm )
            => mm -> Map (Loc ChanRefE) ChanId
usedChIdMap mm =  closure2 (innerMap mm :: Map (Loc ChanRefE) (Loc ChanDeclE)) (innerMap mm)
