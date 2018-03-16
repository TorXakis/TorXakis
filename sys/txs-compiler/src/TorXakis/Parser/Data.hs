-- | 

module TorXakis.Parser.Data where

import           Data.Text (Text)

-- | State of the parser.
-- TODO: make the constructor private.
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

-- | Types of parse trees.
type ADTDecl   = ParseTree ADT     [CstrDecl]
type CstrDecl  = ParseTree Cstr    [FieldDecl]
type FieldDecl = ParseTree Field   OfSort
type OfSort    = ParseTree SortRef ()

-- | Components of a function
data FuncComps = FuncComps
    { funcParams  :: [FieldDecl]
    , funcRetType :: OfSort
    , funcBody    :: Exp
    } deriving (Eq, Show)

-- | Expressions.
data Exp = Var Text Metadata
    deriving (Eq, Show)

type FuncDecl  = ParseTree Func FuncComps
