{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler.Error
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
-- Compiler error type and associated functions.
--------------------------------------------------------------------------------
module TorXakis.Compiler.Error
    ( -- * Error type and field accessors.
      Error (..)
      -- ** Lenses for error fields.
    , errorLoc
    , errorMsg
    , errorType
      -- * Type of errors.
    , ErrorType (..)
    , Entity (..)
      -- * Error location support.
    , ErrorLoc (..)
    , HasErrorLoc
    , getErrorLoc
    )
where

import           Control.Lens.TH (makeLenses)
import           Data.Data       (Data)
import           Data.Text       (Text)

-- | Entity to which the error is related.
data Entity
    = Function
    | Process
    | InitialState
    | State
    | Variable
    | Sort
    | Channel
    -- | In case an error must be generated from a generic function that cannot
    -- have access to the entity type. The error type can be made more specific
    -- by the caller of such a generic function (see for instance
    -- @getUniqueElement@).
    | Entity
    deriving (Eq, Show, Data)

-- | Type of errors that can occur when compiling a 'TorXakis' model file.
data ErrorType
    = ParseError
    | TypeMismatch
    -- | An entity was not defined.
    | Undefined Entity
    -- | Multiple definitions for the same entity.
    | MultipleDefinitions Entity
    -- | An entity could not be resolved (based on the type information for
    -- instance).
    | Unresolved Entity
    -- | An entity has multiple candidates (based on the type information for
    -- instance)
    | Ambiguous Entity
    -- | An entity is missing from the declaration.
    | Missing Entity
    | NoDefinition -- ^ No definition found for function or process.
    | InvalidExpression
    | CompilerPanic -- ^ An error in the compiler has happened.
    deriving (Eq, Show, Data)

-- | Location of an error.
data ErrorLoc
    -- | The error location could not be determined. By adding location to
    -- TorXakis core types like @SortId@ it is possible to augment the number
    -- of places in which errors can be associated to locations.
    = NoErrorLoc
    -- | The error is related to a pre-defined entity.
    | ErrorPredef Text
    | ErrorLoc
        { errorLine   :: Int
        , errorColumn :: Int
        }
    deriving (Eq, Show, Data)

-- | Entities that have an error location.
class HasErrorLoc l where
    getErrorLoc :: l -> ErrorLoc

-- | Simple error type.
data Error
    -- | Single error.
    = Error
    { _errorType :: ErrorType
    , _errorLoc  :: ErrorLoc
    , _errorMsg  :: Text
    }
    -- | Multiple errors.
    | Errors [Error]
    deriving (Eq, Show, Data)

makeLenses ''Error
