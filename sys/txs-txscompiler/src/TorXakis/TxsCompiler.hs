{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.TxsCompiler
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
--  Compiler for the 'TorXakis' language.
--------------------------------------------------------------------------------
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module TorXakis.TxsCompiler
    ( compileFile
    , compileString
    , compileUnsafe
    , TxsCompiler (..)
    )
where

import           Control.Arrow                      ((|||))
import           Control.Lens                       ((^.))
import           Control.Monad.State                (evalStateT)
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Semigroup                     ((<>))
import           Data.Text                          (Text)

import           TorXakis.ContextSort               (ContextSort, empty)
import           TorXakis.Sort                      (Sort(..))

import qualified TorXakis.Compiler

import           TorXakis.TxsCompiler.Data          (CompilerM, newState,
                                                     runCompiler)
import           TorXakis.TxsCompiler.Defs.TxsDefs  (adtsToContext)
import           TorXakis.TxsCompiler.ValExpr.ADTDef(compileToSorts)
import           TorXakis.TxsCompiler.Error         (Error)
import           TorXakis.Parser                    (parseFile,
                                                     parseString)
import           TorXakis.Parser.Data               (ParsedDefs, adts)

data TxsCompiler = TxsCompiler

instance TorXakis.Compiler.Compiler TxsCompiler ContextSort where
    compile _ s = case compileString s of
                    Left err  -> Left $ TorXakis.Compiler.Error err
                    Right ctx -> Right ctx

-- | Compile a file into a TorXakis model.
--
compileFile :: FilePath -> IO (Either Error ContextSort)
compileFile fp = parseFile fp >>= compileParsedDefsIO

-- | Compile a string into a TorXakis model.
--
compileString :: String -> Either Error ContextSort
compileString str = compileEitherParsedDefs $ parseString "" str

compileParsedDefsIO :: Either Error ParsedDefs -> IO (Either Error ContextSort)
compileParsedDefsIO = return . compileEitherParsedDefs

compileEitherParsedDefs :: Either Error ParsedDefs -> Either Error ContextSort
compileEitherParsedDefs (Left err) = Left err
compileEitherParsedDefs (Right pd) = evalStateT (runCompiler . compileParsedDefs $ pd) newState
    
-- | Run the compiler throwing an error if the compiler returns an 'Error'.
compileUnsafe :: CompilerM a -> a
compileUnsafe cmp = throwOnError $
    evalStateT (runCompiler cmp) newState

-- | Call 'error' if the result is 'Left'.
throwOnError :: Either Error a -> a
throwOnError = throwOnLeft ||| id
    where throwOnLeft = error . show

-- | Compile parsed definitions into TorXakis data-types.
compileParsedDefs :: ParsedDefs -> CompilerM ContextSort
compileParsedDefs parsedDefs = do
    -- Generate a map from 'Text' to 'Sort' using the ADT's that are declared
    -- in 'parsedDefs', as well as the predefined Sorts ("Bool", "Int", "Regex",
    -- "String").
    ss <- getSortDictionary parsedDefs

    adtsToContext ss (parsedDefs ^. adts) TorXakis.ContextSort.empty        -- when parsing incrementally empty must be changed to current context


-- | Get a dictionary from sort names to their 'Sort'. The sorts returned
-- include all the sorts defined by a 'TYPEDEF' (in the parsed definitions),
-- and the predefined sorts ('Bool', 'Int', 'Regex', 'String').
getSortDictionary :: ParsedDefs -> CompilerM (Map Text Sort)
getSortDictionary pd = do
    -- Construct the 'Sort's lookup table.
    sMap <- compileToSorts (pd ^. adts)
    let pdsMap = Map.fromList [ ("Bool",   SortBool)
                              , ("Int",    SortInt)
                              , ("Char",   SortChar)
                              , ("String", SortString)
                              , ("Regex",  SortRegex)
                              ]
    return $ pdsMap <> sMap

