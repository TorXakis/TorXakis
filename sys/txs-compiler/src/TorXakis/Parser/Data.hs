{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module TorXakis.Parser.Data
    ( St
    , mkState
    , Metadata (Metadata)
    , nodeMdata
    , incId
    , nextId
    , Loc (Loc)
    -- * Name
    , Name
    , toText
    -- * Types of the entities.
    , ADTE
    , CstrE
    , FieldE
    , SortRefE
    , FuncDeclE
    , VarDeclE
    , ExpDeclE
    -- * Declarations.
    -- ** ADT's
    , ADTDecl
    , mkADTDecl
    , adtName
    , constructors
    , CstrDecl
    , mkCstrDecl
    , cstrName
    , cstrFields
    , FieldDecl
    , mkFieldDecl
    , fieldName
    , fieldSort
    -- ** Type declarations
    , OfSort
    , mkOfSort
    -- ** Functions
    , FuncDecl
    , mkFuncDecl
    , funcName
    , funcParams
    , funcBody
    , funcRetSort
    , VarDecl
    , mkVarDecl
    , varDeclSort
    , varName
    -- ** Expressions
    , ExpDecl (VarExp)
    , mkVarExp
    , mkBoolConstExp
    , mkIntConstExp
    , mkStringConstExp    
    -- * Location of the entities.
    , getLoc
    )
where

import           Data.Text (Text)

-- | State of the parser.
newtype St = St { nextId :: Int } deriving (Eq, Show)

mkState :: Int -> St
mkState = St

-- | Increment the id of the state.
incId :: St -> St
incId (St i) = St (i + 1)

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

instance HasLoc ExpDecl ExpDeclE where
    getLoc (VarExp _ m) = loc m
    getLoc (ConstExp _ m) = loc m

-- * Types of entities encountered when parsing.

-- | ADT.
data ADTE = ADTE deriving (Eq, Show)

-- | Constructor.
data CstrE = CstrE  deriving (Eq, Show)

-- | Field of a constructor.
data FieldE = FieldE deriving (Eq, Show)

-- | Reference to an existing (previously defined or primitive) sort.
data SortRefE = SortRefE deriving (Eq, Show)

-- | Function declaration.
data FuncDeclE = FuncDeclE deriving (Eq, Show)

-- | Function call.
data FuncCallE = FuncCallE deriving (Eq, Show)

-- | An expression
data ExpDeclE = ExpDeclE deriving (Eq, Show)

-- | A variable declaration.
data VarDeclE = VarDeclE deriving (Eq, Show)

-- | A variable occurrence in an expression. It is assumed to be a
-- **reference** to an existing variable.
data VarRefE = VarRefR deriving (Eq, Show)

-- | A constant declaration.
data ConstE = ConstE deriving (Eq, Show)

-- * Types of parse trees.
type ADTDecl   = ParseTree ADTE     [CstrDecl]

mkADTDecl :: Text -> Metadata ADTE -> [CstrDecl] -> ADTDecl
mkADTDecl n m cs = ParseTree (Name n) ADTE m cs

adtName :: ADTDecl -> Text
adtName = nodeNameT

constructors :: ADTDecl -> [CstrDecl]
constructors = child

type CstrDecl  = ParseTree CstrE    [FieldDecl]

mkCstrDecl :: Text -> Metadata CstrE -> [FieldDecl] -> CstrDecl
mkCstrDecl n m fs = ParseTree (Name n) CstrE m fs

cstrName :: CstrDecl -> Text
cstrName = nodeNameT

cstrFields :: CstrDecl -> [FieldDecl]
cstrFields = child

type FieldDecl = ParseTree FieldE   OfSort

mkFieldDecl :: Text -> Metadata FieldE -> OfSort -> FieldDecl
mkFieldDecl n m s = ParseTree (Name n) FieldE m s

fieldName :: FieldDecl -> Text
fieldName = nodeNameT

-- | Get the field of a sort, and the metadata associated to it.
fieldSort :: FieldDecl -> (Text, Metadata SortRefE)
fieldSort f = (nodeNameT . child $ f, nodeMdata . child $ f)

-- | Reference to an existing type
type OfSort    = ParseTree SortRefE ()

mkOfSort :: Text -> Metadata SortRefE -> OfSort
mkOfSort n m = ParseTree (Name n) SortRefE m ()

-- | Components of a function.
data FuncComps = FuncComps
    { funcCompsParams  :: [VarDecl]
    , funcCompsRetSort :: OfSort
    , funcCompsBody    :: ExpDecl
    } deriving (Eq, Show)

-- | Variable declarations.
type VarDecl = ParseTree VarDeclE OfSort

mkVarDecl :: Text -> Metadata VarDeclE -> OfSort -> VarDecl
mkVarDecl n m s = ParseTree (Name n) VarDeclE m s

-- | Name of a variable
varName :: VarDecl -> Text
varName = nodeNameT

varDeclSort :: VarDecl -> (Text, Metadata SortRefE)
varDeclSort f = (nodeNameT . child $ f, nodeMdata . child $ f)

-- | Expressions.
data ExpDecl = VarExp (Name VarRefE) (Metadata ExpDeclE)
             | ConstExp Const (Metadata ExpDeclE)
    deriving (Eq, Show)

data Const = BoolConst Bool
           | IntConst Integer
           | StringConst Text
    deriving (Eq, Show)

mkVarExp :: Text -> Metadata ExpDeclE -> ExpDecl
mkVarExp n m = VarExp (Name n) m

mkBoolConstExp :: Bool -> Metadata ExpDeclE -> ExpDecl
mkBoolConstExp b m = ConstExp (BoolConst b) m

mkIntConstExp :: Integer -> Metadata ExpDeclE -> ExpDecl
mkIntConstExp i m = ConstExp (IntConst i) m

mkStringConstExp :: Text -> Metadata ExpDeclE -> ExpDecl
mkStringConstExp t m = ConstExp (StringConst t) m

type FuncDecl  = ParseTree FuncDeclE FuncComps

mkFuncDecl :: Text -> Metadata FuncDeclE -> [VarDecl] -> OfSort -> ExpDecl -> FuncDecl
mkFuncDecl n m ps s b = ParseTree (Name n) FuncDeclE m (FuncComps ps s b)

funcName :: FuncDecl -> Text
funcName = nodeNameT

funcParams :: FuncDecl -> [VarDecl]
funcParams = funcCompsParams . child

funcBody :: FuncDecl -> ExpDecl
funcBody = funcCompsBody . child

funcRetSort :: FuncDecl -> (Text, Metadata SortRefE)
funcRetSort f = ( nodeNameT . funcCompsRetSort . child $ f
                , nodeMdata . funcCompsRetSort . child $ f
                )
