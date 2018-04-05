{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module TorXakis.Parser.Data
    ( St
    , mkState
    , nodeLoc
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
    , childExps
    , mkVarExp
    , mkBoolConstExp
    , mkIntConstExp
    , mkStringConstExp
    , mkLetExpDecl
    , mkITEExpDecl
    , mkFappl
    , mkLetVarDecl
    , expLetVarDecls
    , LetVarDecl
    , varDeclExp
    , letVarDeclSortName
    -- * Location of the entities.
    , getLoc
    , loc'
    )
where

import           Data.Text               (Text)
import           Lens.Micro              (Lens')

import           TorXakis.Compiler.Error

-- | State of the parser.
newtype St = St { nextId :: Int } deriving (Eq, Show)

mkState :: Int -> St
mkState = St

-- | Increment the id of the state.
incId :: St -> St
incId (St i) = St (i + 1)

data ParseTree t c = ParseTree
    { nodeName :: Name t
    , nodeType :: t
    , nodeLoc  :: Loc t
    , child    :: c
    } deriving (Show, Eq, Ord)

newtype Name t = Name { toText :: Text } deriving (Show, Eq, Ord)

nodeNameT :: ParseTree t c -> Text
nodeNameT = toText . nodeName

-- | Location associated to the elements being parsed.
data Loc t = Loc
    { -- | Line in which a declaration occurs.
      line   :: Int
      -- | Start column.
    , start  :: Int
      -- | Unique identifier.
    , locUid :: Int
    }  deriving (Show, Eq, Ord)

-- | Change extract the location of the metadata, and change its type from 't'
-- to 'u'. This is useful when defining parsed entities whose locations
-- coincide, like expressions and variable-references or constant-literals.
locFromLoc :: Loc t -> Loc u
locFromLoc (Loc l c i) = Loc l c i

class HasLoc a t where
    getLoc :: a -> Loc t
    setLoc :: a -> Loc t -> a
    loc' :: Lens' a (Loc t)
    loc' f a = setLoc a <$> f (getLoc a)

instance HasLoc (ParseTree t c) t where
    getLoc = nodeLoc
    setLoc (ParseTree n t _ c) l' = ParseTree n t l' c

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

mkADTDecl :: Text -> Loc ADTE -> [CstrDecl] -> ADTDecl
mkADTDecl n m cs = ParseTree (Name n) ADTE m cs

adtName :: ADTDecl -> Text
adtName = nodeNameT

constructors :: ADTDecl -> [CstrDecl]
constructors = child

type CstrDecl  = ParseTree CstrE    [FieldDecl]

mkCstrDecl :: Text -> Loc CstrE -> [FieldDecl] -> CstrDecl
mkCstrDecl n m fs = ParseTree (Name n) CstrE m fs

cstrName :: CstrDecl -> Text
cstrName = nodeNameT

cstrFields :: CstrDecl -> [FieldDecl]
cstrFields = child

type FieldDecl = ParseTree FieldE   OfSort

mkFieldDecl :: Text -> Loc FieldE -> OfSort -> FieldDecl
mkFieldDecl n m s = ParseTree (Name n) FieldE m s

fieldName :: FieldDecl -> Text
fieldName = nodeNameT

-- | Get the field of a sort, and the metadata associated to it.
fieldSort :: FieldDecl -> (Text, Loc SortRefE)
fieldSort f = (nodeNameT . child $ f, nodeLoc . child $ f)

-- | Reference to an existing type
type OfSort    = ParseTree SortRefE ()

mkOfSort :: Text -> Loc SortRefE -> OfSort
mkOfSort n m = ParseTree (Name n) SortRefE m ()

-- | Components of a function.
data FuncComps = FuncComps
    { funcCompsParams  :: [VarDecl]
    , funcCompsRetSort :: OfSort
    , funcCompsBody    :: ExpDecl
    } deriving (Eq, Show, Ord)

-- | Variable declarations.
type VarDecl = ParseTree VarDeclE OfSort

mkVarDecl :: Text -> Loc VarDeclE -> OfSort -> VarDecl
mkVarDecl n m s = ParseTree (Name n) VarDeclE m s

class IsVariable v where
    -- | Name of a variable
    varName :: v -> Text

instance IsVariable VarDecl where
    varName = nodeNameT

varDeclSort :: VarDecl -> (Text, Loc SortRefE)
varDeclSort f = (nodeNameT . child $ f, nodeLoc . child $ f)

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

-- foldExp :: (b -> ExpChild -> b) -> b -> ExpDecl -> b
-- foldExp f b (ParseTree _ _ _ c) = f b c

data ExpChild = VarRef (Name VarRefE) (Loc VarRefE)
              | ConstLit Const
              | LetExp [LetVarDecl] ExpDecl
              | If ExpDecl ExpDecl ExpDecl
              | Fappl (Name VarRefE) (Loc VarRefE) [ExpDecl] -- ^ Function application. A function is applied
                                                             -- to a list of expressions.
    deriving (Eq, Ord, Show)

data Const = BoolConst Bool
           | IntConst Integer
           | StringConst Text
    deriving (Eq, Show, Ord)

-- | Extract all the let-variable declarations of an expression.
--
expLetVarDecls :: ExpDecl -> [LetVarDecl]
expLetVarDecls ParseTree { child = VarRef _ _  } = []
expLetVarDecls ParseTree { child = ConstLit _  } = []
expLetVarDecls ParseTree { child = LetExp vs e } =
    vs ++ expLetVarDecls e ++ concatMap (expLetVarDecls . varDeclExp) vs
expLetVarDecls ParseTree { child = If e0 e1 e2 } =
    expLetVarDecls e0 ++ expLetVarDecls e1 ++ expLetVarDecls e2
expLetVarDecls ParseTree { child = Fappl _ _ exs } =
    concatMap expLetVarDecls exs

-- | Get the child-expressions of an expression.
--
-- TODO: see if you can use traversals instead.
childExps :: ExpDecl -> [ExpDecl]
childExps ParseTree { child = VarRef _ _  }    = []
childExps ParseTree { child = ConstLit _  }    = []
childExps ParseTree { child = LetExp _ e }     = [e]
childExps ParseTree { child = If ex0 ex1 ex2 } = [ex0, ex1, ex2]
childExps ParseTree { child = Fappl _ _ exs }  = exs

mkLetExpDecl :: [LetVarDecl] -> ExpDecl -> Loc ExpDeclE -> ExpDecl
mkLetExpDecl vs subEx l = mkExpDecl l (LetExp vs subEx)

mkExpDecl :: Loc ExpDeclE -> ExpChild -> ExpDecl
mkExpDecl l c = ParseTree (Name "") ExpDeclE l c

type LetVarDecl = ParseTree VarDeclE (Maybe OfSort, ExpDecl)

varDeclExp :: LetVarDecl -> ExpDecl
varDeclExp = snd . child

instance IsVariable LetVarDecl where
    varName = nodeNameT

letVarDeclSortName :: LetVarDecl -> Maybe (Text, Loc SortRefE)
letVarDeclSortName vd = do
    srt <- fst . child $ vd
    return (nodeNameT srt, nodeLoc srt)

mkLetVarDecl :: Text -> Maybe OfSort -> ExpDecl -> Loc VarDeclE -> LetVarDecl
mkLetVarDecl n ms subEx m = ParseTree (Name n) VarDeclE m (ms, subEx)

mkITEExpDecl :: Loc ExpDeclE -> ExpDecl -> ExpDecl -> ExpDecl -> ExpDecl
mkITEExpDecl l ex0 ex1 ex2 = mkExpDecl l (If ex0 ex1 ex2)

mkFappl :: Loc ExpDeclE -> Loc VarRefE -> Text -> [ExpDecl] -> ExpDecl
mkFappl le lr n exs = mkExpDecl le (Fappl (Name n) lr exs)

-- | Make a variable expression. The location of the expression will become the
-- location of the variable.
mkVarExp :: Loc ExpDeclE -> Text -> ExpDecl
mkVarExp l n = mkExpDecl l (VarRef (Name n) (locFromLoc l))

mkBoolConstExp :: Loc ExpDeclE -> Bool -> ExpDecl
mkBoolConstExp l b = mkExpDecl l (ConstLit (BoolConst b))

mkIntConstExp :: Loc ExpDeclE -> Integer -> ExpDecl
mkIntConstExp l i = mkExpDecl l (ConstLit (IntConst i))

mkStringConstExp :: Loc ExpDeclE -> Text -> ExpDecl
mkStringConstExp l t = mkExpDecl l (ConstLit (StringConst t))

type FuncDecl  = ParseTree FuncDeclE FuncComps

mkFuncDecl :: Text -> Loc FuncDeclE -> [VarDecl] -> OfSort -> ExpDecl -> FuncDecl
mkFuncDecl n m ps s b = ParseTree (Name n) FuncDeclE m (FuncComps ps s b)

funcName :: FuncDecl -> Text
funcName = nodeNameT

funcNameL :: Lens' FuncDecl Text
funcNameL = undefined

funcParams :: FuncDecl -> [VarDecl]
funcParams = funcCompsParams . child

funcBody :: FuncDecl -> ExpDecl
funcBody = funcCompsBody . child

funcRetSort :: FuncDecl -> (Text, Loc SortRefE)
funcRetSort f = ( nodeNameT . funcCompsRetSort . child $ f
                , nodeLoc . funcCompsRetSort . child $ f
                )

instance HasErrorLoc (Loc t) where
    -- TODO: add line and column numbers to location. Remove the metadata.
    getErrorLoc (Loc l c _)  = ErrorLoc {errorLine = l, errorColumn = c}

instance HasErrorLoc (ParseTree t c) where
    getErrorLoc pt = ErrorLoc { errorLine = l, errorColumn = c }
        where Loc l c _ = nodeLoc pt

