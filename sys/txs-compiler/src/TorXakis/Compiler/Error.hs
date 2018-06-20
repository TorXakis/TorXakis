{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE TemplateHaskell #-}
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

module TorXakis.Compiler.Error where

import           Control.Arrow   (left)
import           Control.Lens    ((.~))
import           Control.Lens.TH (makeLenses)
import           Data.Text       (Text)


-- | Entity to which the error is related.
data Entity
    = Function
    | Process
    | InitialState
    | State
    | Variable
    | Sort
    -- | In case an error must be generated from a generic function that cannot
    -- have access to the entity type. The error type can be made more specific
    -- by the caller of such a generic function (see for instance
    -- @getUniqueElement@).
    | Entity
    deriving (Eq, Show)

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
    | NoDefinition -- ^ No definition found for function or process.
    | InvalidExpression
    | CompilerPanic -- ^ An error in the compiler has happened.
    deriving (Eq, Show)

data ErrorLoc
    = NoErrorLoc
    | ErrorPredef Text
    | ErrorLoc
        { errorLine   :: Int
        , errorColumn :: Int
        }
    deriving (Eq, Show)

class HasErrorLoc l where
    getErrorLoc :: l -> ErrorLoc

-- | TODO: for now we define these ad-hoc instances. Maybe we want to define a
-- more general mechanism. These instances are coupled to the design decisions
-- taken at `TorXakis.Compiler.Data`, which shouldn't. Maybe we need to define
-- a type wrapper, instead of using a tuple.
instance HasErrorLoc l => HasErrorLoc (Either l b) where
    getErrorLoc (Left l) = getErrorLoc l
    getErrorLoc _        = NoErrorLoc

instance HasErrorLoc l => HasErrorLoc (k, l) where
    getErrorLoc (_, l) = getErrorLoc l

instance HasErrorLoc l => HasErrorLoc (l, c, d) where
    getErrorLoc (l, _, _) = getErrorLoc l

-- | For now we use this simple error type.
data Error
    = Error
    { _errorType :: ErrorType
    , _errorLoc  :: ErrorLoc
    , _errorMsg  :: Text
    }
    | Errors [Error]
    deriving (Eq, Show)

makeLenses ''Error
