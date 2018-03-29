{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module TorXakis.Parser.Data
    ( St
    , mkState
    , Metadata (Metadata)
    , nodeMdata
    , incId
    , nextId
    , Loc (Loc)
    , HasLoc
    , IsVariable
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
    , VarRefE
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
    , funcNameL
    , funcParams
    , funcBody
    , funcRetSort
    , VarDecl
    , mkVarDecl
    , varDeclSort
    , varName
    -- ** Expressions
    , ExpDecl
    , ExpChild (..)
    , Const (..)
    , expChild
    , mkVarExp
    , mkBoolConstExp
    , mkIntConstExp
    , mkStringConstExp
--    , expVars
    , expLetVarDecls
    , LetVarDecl
    , varDeclExp
    , letVarDeclSortName
    -- * Location of the entities.
    , getLoc
    , loc'
    )
where

import           Data.Text  (Text)
import           Lens.Micro (Lens')

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
    } deriving (Show, Eq, Ord)

newtype Name t = Name { toText :: Text } deriving (Show, Eq, Ord)

nodeNameT :: ParseTree t c -> Text
nodeNameT = toText . nodeName

-- | Metadata associated to the elements being parsed.
data Metadata t = Metadata
    {  -- | Line in which a declaration occurs.
      line  :: Int
       -- | Start column.
    , start :: Int
       -- | Unique identifier.
    , loc   :: Loc t
    } deriving (Show, Eq, Ord)

newtype Loc t = Loc { intVal :: Int} deriving (Show, Eq, Ord)

-- | Change extract the location of the metadata, and change its type from 't'
-- to 'u'. This is useful when defining parsed entities whose locations
-- coincide, like expressions and variable-references or constant-literals.
locFromMetadata :: Metadata t -> Loc u
locFromMetadata m = let Loc i = loc m in Loc i

class HasLoc a t where
    getLoc :: a -> Loc t
    setLoc :: a -> Loc t -> a
    loc' :: Lens' a (Loc t)
    loc' f a = setLoc a <$> f (getLoc a)

instance HasLoc (ParseTree t c) t where
    getLoc = loc . nodeMdata
    setLoc (ParseTree n t m c) l = ParseTree n t (m {loc = l}) c

-- instance HasLoc ExpDecl ExpDeclE where
--     getLoc (LetExp _ _ m) = loc m
--     getLoc (VarExp _ m)   = loc m
--     getLoc (ConstExp _ m) = loc m

-- * Types of entities encountered when parsing.

-- | ADT.
data ADTE = ADTE deriving (Eq, Ord, Show)

-- | Constructor.
data CstrE = CstrE  deriving (Eq, Ord, Show)

-- | Field of a constructor.
data FieldE = FieldE deriving (Eq, Ord, Show)

-- | Reference to an existing (previously defined or primitive) sort.
data SortRefE = SortRefE deriving (Eq, Ord, Show)

-- | Function declaration.
data FuncDeclE = FuncDeclE deriving (Eq, Ord, Show)

-- | Function call.
data FuncCallE = FuncCallE deriving (Eq, Ord, Show)

-- | An expression
data ExpDeclE = ExpDeclE deriving (Eq, Ord, Show)

-- | A variable declaration.
data VarDeclE = VarDeclE deriving (Eq, Ord, Show)

-- | A variable occurrence in an expression. It is assumed to be a
-- **reference** to an existing variable.
data VarRefE = VarRefE deriving (Eq, Ord, Show)

-- | A constant literal
data ConstLitE = ConstLitE deriving (Eq, Ord, Show)

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
    } deriving (Eq, Show, Ord)

-- | Variable declarations.
type VarDecl = ParseTree VarDeclE OfSort

mkVarDecl :: Text -> Metadata VarDeclE -> OfSort -> VarDecl
mkVarDecl n m s = ParseTree (Name n) VarDeclE m s

class IsVariable v where
    -- | Name of a variable
    varName :: v -> Text

instance IsVariable VarDecl where
    varName = nodeNameT

varDeclSort :: VarDecl -> (Text, Metadata SortRefE)
varDeclSort f = (nodeNameT . child $ f, nodeMdata . child $ f)

-- | Expressions.
type ExpDecl = ParseTree ExpDeclE ExpChild

expChild :: ExpDecl -> ExpChild
expChild = child

-- | Find the variables of an expression.
-- expVars :: ExpDecl -> [(Name VarRefE, Loc VarRefE)]
-- expVars (ParseTree _ _ _ (VarRef n l))  = [(n, l)]
-- expVars (ParseTree _ _ _ (ConstLit _ )) = []
-- expVars (ParseTree _ _ _ (LetExp vs e)) =
--     concatMap (expVars . snd . child) vs ++ expVars e

-- | Extract all the expression declarations of an expression.
expLetVarDecls :: ExpDecl -> [LetVarDecl]
expLetVarDecls (ParseTree _ _ _ (VarRef _ _))  = []
expLetVarDecls (ParseTree _ _ _ (ConstLit _ )) = []
expLetVarDecls (ParseTree _ _ _ (LetExp vs e)) =
    vs ++ expLetVarDecls e

-- foldExp :: (b -> ExpChild -> b) -> b -> ExpDecl -> b
-- foldExp f b (ParseTree _ _ _ c) = f b c

data ExpChild = VarRef (Name VarRefE) (Loc VarRefE)
              | ConstLit Const
              | LetExp [LetVarDecl] ExpDecl
    deriving (Eq, Ord, Show)

-- mkLetExp :: [LetVarDecl] -> ExpDecl -> Metadata ExpDecl -> ExpDecl
-- mkLetExp = undefined

type LetVarDecl = ParseTree VarDeclE (Maybe OfSort, ExpDecl)

varDeclExp :: LetVarDecl -> ExpDecl
varDeclExp = snd . child

instance IsVariable LetVarDecl where
    varName = nodeNameT

letVarDeclSortName :: LetVarDecl -> Maybe (Text, Metadata SortRefE)
letVarDeclSortName vd = do
    srt <- fst . child $ vd
    return (nodeNameT srt, nodeMdata srt)

-- mkLetVarDecl :: Name VarDeclE -> Maybe SortRefE -> ExpDecl -> Metadata VarDeclE -> LetVarDecl
-- mkLetVarDecl = undefined

data Const = BoolConst Bool
           | IntConst Integer
           | StringConst Text
    deriving (Eq, Show, Ord)

mkExpDecl :: Metadata ExpDeclE -> ExpChild -> ExpDecl
mkExpDecl m c = ParseTree (Name "") ExpDeclE m c

-- | Make a variable expression. The location of the expression will become the
-- location of the variable.
mkVarExp :: Text -> Metadata ExpDeclE -> ExpDecl
mkVarExp n m = mkExpDecl m (VarRef (Name n) (locFromMetadata m))

mkBoolConstExp :: Bool -> Metadata ExpDeclE -> ExpDecl
mkBoolConstExp b m = mkExpDecl m (ConstLit (BoolConst b))

mkIntConstExp :: Integer -> Metadata ExpDeclE -> ExpDecl
mkIntConstExp i m = mkExpDecl m (ConstLit (IntConst i))

mkStringConstExp :: Text -> Metadata ExpDeclE -> ExpDecl
mkStringConstExp t m = mkExpDecl m (ConstLit (StringConst t))

type FuncDecl  = ParseTree FuncDeclE FuncComps

mkFuncDecl :: Text -> Metadata FuncDeclE -> [VarDecl] -> OfSort -> ExpDecl -> FuncDecl
mkFuncDecl n m ps s b = ParseTree (Name n) FuncDeclE m (FuncComps ps s b)

funcName :: FuncDecl -> Text
funcName = nodeNameT

funcNameL :: Lens' FuncDecl Text
funcNameL = undefined

funcParams :: FuncDecl -> [VarDecl]
funcParams = funcCompsParams . child

funcBody :: FuncDecl -> ExpDecl
funcBody = funcCompsBody . child

funcRetSort :: FuncDecl -> (Text, Metadata SortRefE)
funcRetSort f = ( nodeNameT . funcCompsRetSort . child $ f
                , nodeMdata . funcCompsRetSort . child $ f
                )
