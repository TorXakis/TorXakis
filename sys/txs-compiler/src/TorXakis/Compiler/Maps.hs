{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
import           FuncTable                 (Handler, Signature, sortArgs,
                                            sortRet)
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
determineF :: MapsTo (Loc FuncDeclE) Signature mm
           => mm
           -> [Loc FuncDeclE]
           -> [SortId]        -- ^ Arguments SortId
           -> Maybe SortId    -- ^ Return Sort, if known.
           -> [Loc FuncDeclE]
determineF mm ls aSids mRSid =
    filter funcMatches ls
    where
      funcMatches :: Loc FuncDeclE -> Bool
      funcMatches l = const False ||| id $ do
          sig <- lookup l mm
          return $ sortArgs sig == aSids &&
                   fromMaybe True ((sortRet sig ==) <$> mRSid)

-- | Select the function definitions that matches the given arguments and return
-- types.
determineF_2 :: [Signature]
             -> [SortId]        -- ^ Arguments SortId
             -> Maybe SortId    -- ^ Return Sort, if known.
             -> [Signature]
determineF_2 sigs aSids mRSid =
    filter funcMatches sigs
    where
      funcMatches :: Signature -> Bool
      funcMatches sig = sortArgs sig == aSids &&
                        fromMaybe True ((sortRet sig ==) <$> mRSid)


-- | Determine the signature and the handler based on the sort arguments and
-- the expected return type (if any).
determineSH :: [(Signature, Handler VarId)]
             -> [SortId]     -- ^ @SortId@s of the arguments
             -> Maybe SortId -- ^ @SortId@ (if known)
             -> Either Error (Signature, Handler VarId)
determineSH shs sargs msret =
    case filter (sigMatches . fst) shs of
        [(sig, h)] -> return (sig, h)
        -- TODO: give the appropriate error using Left
        [] -> error "Could not determine the function based on the given signature "
        _ -> error "Found matching signatures"
    where
      sigMatches sig = (sortArgs sig == sargs) && maybe True (sortRet sig ==) msret

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

findFuncDefs :: forall a b . (Typeable a, Typeable b)
               => Map (Loc VarRefE) (Either a b)
               -> Loc VarRefE
               -> Either Error b
findFuncDefs vdefs l = Left ||| cErr ||| Right $ lookup l vdefs
    where
      cErr :: a -> Either Error b
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

findFuncSortIds :: MapsTo (Loc FuncDeclE) Signature mm
                => mm -> [Loc FuncDeclE] -> Either Error [SortId]
findFuncSortIds mm fdis = fmap sortRet <$> traverse (`lookup` mm) fdis

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

-- TODO: rename this to join!
closure2 :: (Ord k, Ord v) => Map k v -> Map v w -> Map k w
closure2 m0 m1 = foldl maybeAddPair Map.empty (Map.toList m0)
    where
      maybeAddPair acc (k, v) =
          maybe acc (\w -> Map.insert k w acc) (Map.lookup v m1)

joins :: forall a b c . (HasErrorLoc b, Ord b, Show b, Typeable b, Typeable c)
      => Map a [b]
      -> Map b c
      -> Either Error (Map a [c])
joins a2bs b2c = Map.traverseWithKey (const $ traverse (b2c .@@)) a2bs

leftValuesOnly :: Ord a => Map a (Either b c) -> Map a b
leftValuesOnly = Map.foldlWithKey keepLeft Map.empty
    where
      keepLeft acc a (Left b)  = Map.insert a b acc
      keepLeft acc a (Right _) = acc

rightValuesOnly :: Ord a => Map a (Either b c) -> Map a c
rightValuesOnly = Map.foldlWithKey keepRight Map.empty
    where
      keepRight acc _ (Left _)  = acc
      keepRight acc a (Right c) = Map.insert a c acc

-- | Channels referred in the model
usedChIdMap :: ( MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
               , MapsTo (Loc ChanDeclE) ChanId mm )
            => mm -> Map (Loc ChanRefE) ChanId
usedChIdMap mm =  closure2 (innerMap mm :: Map (Loc ChanRefE) (Loc ChanDeclE)) (innerMap mm)
