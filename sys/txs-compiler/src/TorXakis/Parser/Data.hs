-- | 

module TorXakis.Parser.Data where

import           Data.Text (Text)

-- | State of the parser.
newtype St = St { nextId :: Int } deriving (Eq, Show)

data ParseTree t c = ParseTree
    { nodeName  :: Text
    , nodeType  :: t
    , nodeMdata :: Metadata
    , child     :: c
    } deriving (Show, Eq)

-- | Metadata associated to the elements being parsed.
data Metadata = Metadata
    {  -- | Line in which a declaration occurs.
      line   :: Int
       -- | Start column.
    , start  :: Int
       -- | Unique identifier.
    , uid    :: Int
    } deriving (Show, Eq)

-- * Types of entities encountered when parsing.

-- | ADT declaration.
data ADT   = ADT   deriving (Eq, Show)
-- | Constructor declaration.
data Cstr  = Cstr  deriving (Eq, Show)
-- | Field declaration.
data Field = Field deriving (Eq, Show)
-- | Reference to an existing sort.
data SortRef = SortRef deriving (Eq, Show)

-- | Types of parse trees.
type ADTDecl   = ParseTree ADT     [CstrDecl]
type CstrDecl  = ParseTree Cstr    [FieldDecl]
type FieldDecl = ParseTree Field   OfSort
type OfSort    = ParseTree SortRef ()
