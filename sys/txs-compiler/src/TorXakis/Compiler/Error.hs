module TorXakis.Compiler.Error where

import           Data.Text (Text)

-- | For now we use this simple error type.
data Error = Error
    { errorType :: ErrorType
    , errorLoc  :: ErrorLoc
    , errorMsg  :: Text
    } deriving (Eq, Show)

data ErrorType
    = ParseError
    | UndefinedRef -- TODO: we could specify the type of what's being undefined (variable, sort, etc...)
    | TypeMismatch
    | UndefinedType
    | FunctionNotDefined
    deriving (Eq, Show)

data Decl
    = Variable
    | Sort
    deriving (Eq, Show)

data ErrorLoc
    = NoErrorLoc
    | ErrorLoc
        { errorLine   :: Int
        , errorColumn :: Int
        }
    deriving (Eq, Show)

class HasErrorLoc l where
    getErrorLoc :: l -> ErrorLoc

instance HasErrorLoc l => HasErrorLoc (Either l b) where
    getErrorLoc (Left l) = getErrorLoc l
    getErrorLoc _        = NoErrorLoc
