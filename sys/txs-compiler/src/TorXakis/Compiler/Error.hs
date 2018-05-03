{-# LANGUAGE TemplateHaskell #-}
module TorXakis.Compiler.Error where

import           Control.Arrow   (left)
import           Control.Lens    ((.~))
import           Control.Lens.TH (makeLenses)
import           Data.Text       (Text)


data ErrorType
    = ParseError
    | UndefinedRef -- TODO: we could specify the type of what's being undefined (variable, sort, etc...)
    | TypeMismatch
    | UndefinedType
    | FunctionNotDefined
    | UnresolvedIdentifier
    | MultipleDefinitions
    | NoDefinition -- ^ No definition found for function or process.
    | ProcessNotDefined
    | CompilerPanic -- ^ An error in the compiler has happened.
    deriving (Eq, Show)

data Decl
    = Variable
    | Sort
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
