{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module TorXakis.Parser.Data where

import           Data.Text (Text)

-- | State of the parser.
-- TODO: make the constructor private.
newtype St = St { nextId :: Int } deriving (Eq, Show)

data ParseTree t c = ParseTree
    { nodeName  :: Name t
    , nodeType  :: t
    , nodeMdata :: Metadata t
    , child     :: c
    } deriving (Show, Eq)

newtype Name t = Name { toText :: Text } deriving (Show, Eq, Ord)

nodeNameT :: ParseTree t c -> Text
nodeNameT = toText . nodeName

-- | Metadata associated to the elements being parsed.
data Metadata t = Metadata
    {  -- | Line in which a declaration occurs.
      line   :: Int
       -- | Start column.
    , start  :: Int
       -- | Unique identifier.
    , loc    :: Loc t
    } deriving (Show, Eq)

newtype Loc t = Loc Int deriving (Show, Eq, Ord)

class HasLoc a t where
    getLoc :: a -> Loc t

instance HasLoc (ParseTree t c) t where
    getLoc = loc . nodeMdata

instance HasLoc ExpDecl Exp where
    getLoc (Var _ m) = loc m

-- * Types of entities encountered when parsing.

-- | ADT.
data ADT   = ADT   deriving (Eq, Show)
-- | Constructor.
data Cstr  = Cstr  deriving (Eq, Show)
-- | Field.
data Field = Field deriving (Eq, Show)
-- | Reference to an existing sort.
data SortRef = SortRef deriving (Eq, Show)
-- | Function.
data Func = Func deriving (Eq, Show)
-- | An expression
data Exp = Exp deriving (Eq, Show)

-- * Types of parse trees.
type ADTDecl   = ParseTree ADT     [CstrDecl]
type CstrDecl  = ParseTree Cstr    [FieldDecl]
type FieldDecl = ParseTree Field   OfSort
type OfSort    = ParseTree SortRef ()

-- | Components of a function
data FuncComps = FuncComps
    { funcParams  :: [FieldDecl]
    , funcRetType :: OfSort
    , funcBody    :: ExpDecl
    } deriving (Eq, Show)

-- | Expressions.
data ExpDecl = Var Text (Metadata Exp)
    deriving (Eq, Show)

type FuncDecl  = ParseTree Func FuncComps
