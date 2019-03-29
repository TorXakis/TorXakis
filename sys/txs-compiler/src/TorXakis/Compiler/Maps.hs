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
    --, determineF
    , getUniqueElement
    , findSort
    --, findFuncReturnSorts
    , findSortM
    , findRight
    --, idefsNames
      -- * Map manipulation
    , join
    )
where

import           Control.Arrow            (left, (|||))
import           Control.Lens             ((.~))
import           Control.Monad.Except     (catchError, liftEither, throwError)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
--import           Data.Maybe               (catMaybes, maybe)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Typeable            (Typeable)
import           Prelude                  hiding (lookup)

--import           TorXakis.FuncSignature   (FuncSignature(args, returnSort))
import           TorXakis.Sort            (Sort)

import           TorXakis.Compiler.Data   (CompilerM)
import           TorXakis.Compiler.Error  (Entity (Entity, Function),
                                           Error (Error), ErrorLoc (NoErrorLoc),
                                           ErrorType (MultipleDefinitions, Undefined),
                                           HasErrorLoc, errorLoc, errorMsg,
                                           getErrorLoc, _errorLoc, _errorMsg,
                                           _errorType)
import           TorXakis.Compiler.MapsTo ( MapsTo
                                          --, innerMap
                                           , lookup, lookupM)
import           TorXakis.Parser.Data     (FuncDeclE,
                                           Loc --(ExtraAut, Loc, PredefLoc)
                                           , VarDeclE, VarRefE)

-- | Lookup the @Sort@ associated to the given name, using the location for
-- error reporting.
findSort :: MapsTo Text Sort mm
           => mm -> (Text, Loc t) -> Either Error Sort
findSort mm (t, l) = left (errorMsg .~ msg) $ lookup t mm <!> l
    where
      msg = "Could not find sort " <> t

-- | Monadic version of @findSortM@
findSortM :: MapsTo Text Sort mm
           => mm -> (Text, Loc t) -> CompilerM Sort
findSortM mm (t, l) = liftEither $ findSort mm (t, l)

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

    {-
-- | Select the function definitions that matches the given arguments and return
-- types.
determineF :: MapsTo (Loc FuncDeclE) FuncSignature mm
           => mm
           -> [Loc FuncDeclE]
           -> [Sort]        -- ^ Arguments Sort.
           -> Maybe Sort    -- ^ Return Sort, if known.
           -> [Loc FuncDeclE]
determineF mm ls aSids mRSid =
    filter funcMatches ls
    where
      funcMatches :: Loc FuncDeclE -> Bool
      funcMatches l = const False ||| id $ do
          sig <- lookup l mm
          return $ args sig == aSids &&
                   maybe True (returnSort sig ==) mRSid
-}

{-
-- | Get the name of the implicit function declaration, if any.
fdiName :: Loc FuncDeclE -> Maybe Text
fdiName Loc {}          = Nothing
fdiName ExtraAut {}     = Nothing
fdiName (PredefLoc n _) = Just n
-}

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
            , _errorMsg  = "Could not function declaration."
            }

{-
-- | Find all the return @Sort@'s of the given function declarations.
findFuncReturnSorts :: MapsTo (Loc FuncDeclE) FuncSignature mm
                => mm -> [Loc FuncDeclE] -> Either Error [Sort]
findFuncReturnSorts mm fdis = fmap returnSort <$> traverse (`lookup` mm) fdis
-}

{-
-- | Extract the names of the implicit function declarations in the map.
idefsNames :: MapsTo (Loc FuncDeclE) FuncSignature mm => mm -> [Text]
idefsNames mm = catMaybes $ fdiName <$> Map.keys fm
    where
      fm :: Map (Loc FuncDeclE) FuncSignature
      fm = innerMap mm
-}
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


-- | Lookup the given key, and use the given location for error reporting.
(.@!!) :: ( HasErrorLoc l, MapsTo k v mm, Ord k, Show k
          , Typeable k, Typeable v )
     => mm -> (k, l) -> CompilerM v
mm .@!! (k, l) = lookupM k mm <!!> l

-- | Join two maps 'm0' and 'm1'. A pair '(k, w)' is in the resulting map if
-- and only if there is a key 'v' such that '(k, v)' is in 'm0' and '(v, w)' is
-- in 'm1'.
join :: (Ord k, Ord v) => Map k v -> Map v w -> Map k w
join m0 m1 = foldl maybeAddPair Map.empty (Map.toList m0)
    where
      maybeAddPair acc (k, v) =
          maybe acc (\w -> Map.insert k w acc) (Map.lookup v m1)
