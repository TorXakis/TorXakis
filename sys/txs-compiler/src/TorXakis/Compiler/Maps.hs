{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.Maps
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compiler specific maps and operations on them.
--------------------------------------------------------------------------------

module TorXakis.Compiler.Maps
    ( -- * Map lookups
      (.@)
    , (.@!!)
    , (.@@)
    , (<!!>)
      -- * Specific lookups
    , findFuncDecl
    , determineF
    , getUniqueElement
    , lookupChId
    , chRefsToIds
    , findSortId
    , findFuncSortIds
    , findSortIdM
    , determineSH
    , findRight
    , usedChIdMap
    , findVarIdM
    , idefsNames
    , usedChIds
      -- * Map manipulation
    , join
    , dropHandler
    )
where

import           Control.Arrow            (left, (|||))
import           Control.Lens             ((.~))
import           Control.Monad.Except     (catchError, liftEither, throwError)
import           Data.List                (nub, sortBy)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import           Data.Maybe               (catMaybes)
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Typeable            (Typeable)
import           Prelude                  hiding (lookup)

import           ChanId                   (ChanId, unid)
import           FuncId                   (FuncId)
import           FuncTable                (Handler, Signature, sortArgs,
                                           sortRet)
import           SortId                   (SortId)
import           VarId                    (VarId)

import           TorXakis.Compiler.Data   (CompilerM)
import           TorXakis.Compiler.Error  (Entity (Entity, Function, Variable),
                                           Error (Error), ErrorLoc (NoErrorLoc),
                                           ErrorType (MultipleDefinitions, Undefined),
                                           HasErrorLoc, errorLoc, errorMsg,
                                           getErrorLoc, _errorLoc, _errorMsg,
                                           _errorType)
import           TorXakis.Compiler.MapsTo (MapsTo, innerMap, lookup, lookupM)
import           TorXakis.Parser.Data     (ChanDeclE, ChanRef, ChanRefE,
                                           FuncDeclE,
                                           Loc (ExtraAut, Loc, PredefLoc),
                                           VarDeclE, VarRefE, getLoc)

-- | Lookup the @SortId@ associated to the given name, using the location for
-- error reporting.
findSortId :: MapsTo Text SortId mm
           => mm -> (Text, Loc t) -> Either Error SortId
findSortId mm (t, l) = left (errorMsg .~ msg) $ lookup t mm <!> l
    where
      msg = "Could not find sort " <> t

-- | Monadic version of @findSortIdM@
findSortIdM :: MapsTo Text SortId mm
           => mm -> (Text, Loc t) -> CompilerM SortId
findSortIdM mm (t, l) = liftEither $ findSortId mm (t, l)

-- | Get the function definition if the given list is a singleton, return an
-- error otherwise.
getUniqueElement :: Show a => [a] -> Either Error a
getUniqueElement [fdi] = Right fdi
getUniqueElement [] = Left Error
    { _errorType = Undefined Entity
    , _errorLoc  = NoErrorLoc
    , _errorMsg  = "Could not find an element."
    }
getUniqueElement xs = Left Error
    { _errorType = MultipleDefinitions Entity
    , _errorLoc  = NoErrorLoc
    , _errorMsg  = "Found multiple elements: " <> T.pack (show xs)
    }

-- | Select the function definitions that matches the given arguments and return
-- types.
determineF :: MapsTo (Loc FuncDeclE) Signature mm
           => mm
           -> [Loc FuncDeclE]
           -> [SortId]        -- ^ Arguments SortId.
           -> Maybe SortId    -- ^ Return Sort, if known.
           -> [Loc FuncDeclE]
determineF mm ls aSids mRSid =
    filter funcMatches ls
    where
      funcMatches :: Loc FuncDeclE -> Bool
      funcMatches l = const False ||| id $ do
          sig <- lookup l mm
          return $ sortArgs sig == aSids &&
                   maybe True (sortRet sig ==) mRSid

-- | Determine the signature and the handler based on the sort arguments and
-- the expected return type (if any).
determineSH :: [(Signature, Handler VarId)]
             -> [SortId]     -- ^ @SortId@s of the arguments.
             -> Maybe SortId -- ^ Expected return @SortId@ (if known).
             -> ErrorLoc     -- ^ Error Loc to be reported in case of error
             -> Either Error (Signature, Handler VarId)
determineSH shs sargs msret el =
    case filter (sigMatches . fst) shs of
        [(sig, h)] -> return (sig, h)
        [] -> Left Error
            { _errorType = Undefined Function
            , _errorLoc = el
            , _errorMsg = "Could not determine the function based on the given signature "
            }
        _ -> Left Error
            { _errorType = MultipleDefinitions Function
            , _errorLoc = el
            , _errorMsg = "Found multiple functions that can be applied."
            }
    where
      sigMatches sig = (sortArgs sig == sargs) && maybe True (sortRet sig ==) msret

-- | Get the name of the implicit function declaration, if any.
fdiName :: Loc FuncDeclE -> Maybe Text
fdiName Loc {}          = Nothing
fdiName ExtraAut {}     = Nothing
fdiName (PredefLoc n _) = Just n

-- | Find the nullary function declaration that corresponds to a variable
-- reference. A nullary function is a funciton without arguments.
findFuncDecl :: MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
             => mm -> Loc VarRefE -> Either Error [Loc FuncDeclE]
findFuncDecl mm l = Left ||| cErr ||| Right $ lookup l mm
    where
      cErr :: Loc VarDeclE -> Either Error a
      cErr _ = Left err
      err = Error
            { _errorType = Undefined Function
            , _errorLoc  = getErrorLoc l
            , _errorMsg  = "Could not function declaration."
            }

-- | Find the right element (if any) that corresponds to the given location.
findRight :: forall a b . (Typeable a, Typeable b)
               => Map (Loc VarRefE) (Either a b)
               -> Loc VarRefE
               -> Either Error b
findRight vdefs l = Left ||| cErr ||| Right $ lookup l vdefs
    where
      cErr :: a -> Either Error b
      cErr _ = Left err
      err = Error
            { _errorType = Undefined Function
            , _errorLoc  = getErrorLoc l
            , _errorMsg  = "Could not find function declaration."
            }

-- | Find the variable declaration that corresponds to a variable reference.
findVarDecl ::  MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
            => mm -> Loc VarRefE -> Either Error (Loc VarDeclE)
findVarDecl mm l = Left ||| Right ||| cErr $ lookup l mm
    where
      cErr :: [Loc FuncDeclE] -> Either Error a
      cErr _ = Left err
      err = Error
            { _errorType = Undefined Variable
            , _errorLoc  = getErrorLoc l
            , _errorMsg  = "Could not find variable declaration."
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

-- | Find all the return @SortId@'s of the given function declarations.
findFuncSortIds :: MapsTo (Loc FuncDeclE) Signature mm
                => mm -> [Loc FuncDeclE] -> Either Error [SortId]
findFuncSortIds mm fdis = fmap sortRet <$> traverse (`lookup` mm) fdis

-- | Extract the names of the implicit function declarations in the map.
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

-- | Infix version of lookup, for those keys that have an error location.
(.@) :: (HasErrorLoc k, MapsTo k v mm, Ord k, Show k
         , Typeable k, Typeable v)
      => mm -> k -> Either Error v
mm .@ k = lookup k mm <!> k

-- | Monadic version of @.\@@.
(.@@) :: ( HasErrorLoc k, MapsTo k v mm, Ord k, Show k
        , Typeable k, Typeable v)
     => mm -> k -> CompilerM v
mm .@@ k = lookupM k mm <!!> k

-- | Lookup the @ChanId@ associated to the given location.
lookupChId :: ( MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
              , MapsTo (Loc ChanDeclE) ChanId mm )
           => mm -> Loc ChanRefE -> CompilerM ChanId
lookupChId mm cr = do
    cd <- mm .@@ cr :: CompilerM (Loc ChanDeclE)
    mm .@@ cd

-- | Lookup the given key, and use the given location for error reporting.
(.@!!) :: ( HasErrorLoc l, MapsTo k v mm, Ord k, Show k
          , Typeable k, Typeable v )
     => mm -> (k, l) -> CompilerM v
mm .@!! (k, l) = lookupM k mm <!!> l

-- | Given a list of channel references and a composite map, get the @ChanId@'s
-- associated to those references.
chRefsToIds :: ( MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
               , MapsTo (Loc ChanDeclE) ChanId mm )
            => mm -> [ChanRef] -> CompilerM [ChanId]
chRefsToIds mm chs = traverse (lookupChId mm) (getLoc <$> chs)

-- | Join two maps 'm0' and 'm1'. A pair '(k, w)' is in the resulting map if
-- and only if there is a key 'v' such that '(k, v)' is in 'm0' and '(v, w)' is
-- in 'm1'.
join :: (Ord k, Ord v) => Map k v -> Map v w -> Map k w
join m0 m1 = foldl maybeAddPair Map.empty (Map.toList m0)
    where
      maybeAddPair acc (k, v) =
          maybe acc (\w -> Map.insert k w acc) (Map.lookup v m1)

-- | Channels referred in the model
usedChIdMap :: ( MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
               , MapsTo (Loc ChanDeclE) ChanId mm )
            => mm -> Map (Loc ChanRefE) ChanId
usedChIdMap mm =  join (innerMap mm :: Map (Loc ChanRefE) (Loc ChanDeclE)) (innerMap mm)

-- | Drop the handler from the map.
dropHandler :: Map (Loc FuncDeclE) (Signature, Handler VarId)
            -> Map (Loc FuncDeclE) Signature
dropHandler = fmap fst

-- | Get the list of channel ids used in an expression (model, purpose, etc).
usedChIds :: ( MapsTo (Loc ChanRefE) (Loc ChanDeclE) mm
             , MapsTo (Loc ChanDeclE) ChanId mm )
          => mm -> [Set ChanId]
usedChIds mm = fmap Set.singleton (sortByUnid . nub . Map.elems $ usedChIdMap mm)
    where
      sortByUnid :: [ChanId] -> [ChanId]
      sortByUnid = sortBy cmpChUnid
          where
            cmpChUnid c0 c1 = unid c0 `compare` unid c1
