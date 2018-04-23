{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
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
    , ChanDeclE
    , ChanRefE
    , ProcDeclE
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
    , sortRefName
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
    , IVarDecl
    , mkIVarDecl
    , mkVarRef
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
    , mkRegexConstExp
    , mkLetExpDecl
    , mkITEExpDecl
    , mkFappl
    , mkLetVarDecl
    , expLetVarDecls
    , LetVarDecl
    , varDeclExp
    , letVarDeclSortName
    -- ** Models
    , ModelDecl
    , mkModelDecl
    , modelName
    , modelIns
    , modelOuts
    , modelSyncs
    , modelBExp
    , BExpDecl (..)
    , ActOfferDecl (..)
    , OfferDecl (..)
    , ChanOfferDecl (..)
    , SyncOn (..)
    , chanOfferIvarDecl
    , actOfferDecls
    , asVarReflLoc
    -- ** Channels
    , ChanDecl
    , mkChanDecl
    , chanDeclName
    , chanDeclSorts
    , ChanRef
    , mkChanRef
    , chanRefName
    -- ** Processes
    , ProcDecl
    , mkProcDecl
    , procDeclName
    , procDeclComps
    , procDeclBody
    , procDeclParams
    , ProcComps
    , procChParams
    , procParams
    , procRetSort
    , procBody
    , procRefName
    , ExitSortDecl (..)
    -- * Location of the entities.
    , getLoc
    , loc'
    )
where

import           Control.Lens            (Lens')
import           Data.Set                (Set)
import           Data.Text               (Text)

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

class HasLoc a t | a -> t where
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

-- | An expression
data ExpDeclE = ExpDeclE deriving (Eq, Ord, Show)

-- | A variable declaration.
data VarDeclE = VarDeclE deriving (Eq, Ord, Show)

-- | A variable occurrence in an expression. It is assumed to be a
-- **reference** to an existing variable.
data VarRefE = VarRefE deriving (Eq, Ord, Show)

-- | A constant literal
data ConstLitE = ConstLitE deriving (Eq, Ord, Show)

-- | Channel declaration.
data ChanDeclE = ChanDeclE deriving (Eq, Ord, Show)

-- | Channel  reference.
data ChanRefE = ChanRefE deriving (Eq, Ord, Show)

-- | Model declaration.
data ModelDeclE = ModelDeclE deriving (Eq, Ord, Show)

-- | Process declaration.
data ProcDeclE = ProcDeclE deriving (Eq, Ord, Show)

-- | Process reference. Used at process instantiations.
data ProcRefE = ProcRefE deriving (Eq, Ord, Show)

-- | Parallel operator occurrence in a behavior expression.
data ParOpE = ParOpE deriving (Eq, Ord, Show)

-- * Types of parse trees.
type ADTDecl   = ParseTree ADTE     [CstrDecl]

mkADTDecl :: Text -> Loc ADTE -> [CstrDecl] -> ADTDecl
mkADTDecl n l = ParseTree (Name n) ADTE l

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

type FieldDecl = ParseTree FieldE OfSort

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

sortRefName :: OfSort -> Text
sortRefName = nodeNameT

-- | Components of a function.
data FuncComps = FuncComps
    { funcCompsParams  :: [VarDecl]
    , funcCompsRetSort :: OfSort
    , funcCompsBody    :: ExpDecl
    } deriving (Eq, Show, Ord)

-- | Variable declarations.
type VarDecl = ParseTree VarDeclE OfSort

mkVarDecl :: Text -> Loc VarDeclE -> OfSort -> VarDecl
mkVarDecl n l s = ParseTree (Name n) VarDeclE l s

-- | Implicit variable declaration (with no sort associated to it)
type IVarDecl = ParseTree VarDeclE ()

mkIVarDecl :: Text -> Loc VarDeclE -> IVarDecl
mkIVarDecl n l = ParseTree (Name n) VarDeclE l ()

class IsVariable v where
    -- | Name of a variable
    varName :: v -> Text

instance IsVariable VarDecl where
    varName = nodeNameT

instance IsVariable IVarDecl where
    varName = nodeNameT

varDeclSort :: VarDecl -> (Text, Loc SortRefE)
varDeclSort f = (nodeNameT . child $ f, nodeLoc . child $ f)

-- | Expressions.
type ExpDecl = ParseTree ExpDeclE ExpChild

expChild :: ExpDecl -> ExpChild
expChild = child

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
           | RegexConst Text
           | AnyConst
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

mkRegexConstExp :: Loc ExpDeclE -> Text -> ExpDecl
mkRegexConstExp l t = mkExpDecl l (ConstLit (RegexConst t))

mkAnyConstExp :: Loc ExpDeclE -> ExpDecl
mkAnyConstExp l = mkExpDecl l (ConstLit AnyConst)

type FuncDecl  = ParseTree FuncDeclE FuncComps

mkFuncDecl :: Text -> Loc FuncDeclE -> [VarDecl] -> OfSort -> ExpDecl -> FuncDecl
mkFuncDecl n l ps s b = ParseTree (Name n) FuncDeclE l (FuncComps ps s b)

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

type ModelDecl = ParseTree ModelDeclE ModelComps

-- | Make a model declaration.
mkModelDecl :: Text                -- ^ Model name.
            -> Loc ModelDeclE      -- ^ Location of the model.
            -> [ChanRef]           -- ^ References to input channels.
            -> [ChanRef]           -- ^ References to output channels.
            -> Maybe [Set ChanRef] -- ^ References to sets of synchronized channels.
            -> BExpDecl            -- ^ Behavior expression that defines the model.
            -> ModelDecl
mkModelDecl n l is os ys be =
    ParseTree (Name n) ModelDeclE l (ModelComps is os ys be)

modelName :: ModelDecl -> Text
modelName = nodeNameT

modelBExp :: ModelDecl -> BExpDecl
modelBExp = bexp . child

modelIns :: ModelDecl -> [ChanRef]
modelIns = inchs . child

modelOuts :: ModelDecl -> [ChanRef]
modelOuts = outchs . child

modelSyncs :: ModelDecl -> Maybe [Set ChanRef]
modelSyncs = synchs . child

type ChanRef = ParseTree ChanRefE ()

-- | Make a channel reference.
mkChanRef :: Text         -- ^ Name of the channel that is being referred.
          -> Loc ChanRefE -- ^ Location where the reference took place.
          -> ChanRef
mkChanRef n l = ParseTree (Name n) ChanRefE l ()

chanRefName :: ChanRef -> Text
chanRefName = nodeNameT

data ModelComps = ModelComps
    { inchs  :: [ChanRef]
    , outchs :: [ChanRef]
    , synchs :: Maybe [Set ChanRef]
    , bexp   :: BExpDecl
    } deriving (Eq, Ord, Show)

data BExpDecl = Stop
              | ActPref  ActOfferDecl BExpDecl
              | LetBExp  [LetVarDecl] BExpDecl
              | Pappl (Name ProcRefE) (Loc ProcRefE) [ChanRef] [ExpDecl]
              | Par (Loc ParOpE) SyncOn BExpDecl BExpDecl
    deriving (Eq, Ord, Show)

-- | Channels to sync on in a parallel operator.
data SyncOn = All              -- ^ Sync on all channels, this is the result of
                               -- parsing '||'
            | OnlyOn [ChanRef] -- ^ Sync only on the given channels. This is
                               -- the result of parsing either '|||' or
                               -- '|[...]|'. Parsing '|||' will result in an
                               -- empty list, meaning that full interleaving is
                               -- allowed.
            deriving (Eq, Ord, Show)

procRefName :: Text -> Name ProcRefE
procRefName = Name

data ActOfferDecl = ActOfferDecl
    { _offers     :: [OfferDecl]
    , _constraint :: Maybe ExpDecl
    } deriving (Eq, Ord, Show)

data OfferDecl = OfferDecl ChanRef [ChanOfferDecl]
    deriving (Eq, Ord, Show)

data ChanOfferDecl = QuestD IVarDecl
                   | ExclD  ExpDecl
    deriving (Eq, Ord, Show)

chanOfferIvarDecl :: ChanOfferDecl -> Maybe IVarDecl
chanOfferIvarDecl (QuestD iv) = Just iv
chanOfferIvarDecl _           = Nothing

-- | Transform a variable declaration into a variable reference. This is used
-- in the case of an implicit variable declaration (which is a reference to
-- itself).
--
-- TODO: does it make sense to have this function instead of just exporting @locFromLoc@.
asVarReflLoc :: Loc VarDeclE -> Loc VarRefE
asVarReflLoc = locFromLoc

-- | Get all the variable declarations introduced by receiving actions of the
-- form 'Ch ? v'.
actOfferDecls :: ActOfferDecl -> [IVarDecl]
actOfferDecls (ActOfferDecl os _) = concatMap f os
    where
      f :: OfferDecl -> [IVarDecl]
      f (OfferDecl _ cs) = concatMap g cs
      g (QuestD ivd) = [ivd]
      g (ExclD _)    = []

type VarRef = ParseTree VarRefE ()

mkVarRef :: Text -> Loc VarRefE -> VarRef
mkVarRef n l = ParseTree (Name n) VarRefE l ()

type ChanDecl = ParseTree ChanDeclE [OfSort]

-- | Make a channel declaration.
mkChanDecl :: Text -> Loc ChanDeclE -> [OfSort] -> ChanDecl
mkChanDecl n = ParseTree (Name n) ChanDeclE

chanDeclName :: ChanDecl -> Text
chanDeclName = nodeNameT

chanDeclSorts :: ChanDecl -> [(Text, Loc SortRefE)]
chanDeclSorts ch = zip (fmap nodeNameT . child $ ch)
                       (fmap nodeLoc   . child $ ch)

-- | Process declaration.
type ProcDecl = ParseTree ProcDeclE ProcComps

-- | Make a process declaration.
mkProcDecl :: Text
           -> Loc ProcDeclE
           -> [ChanDecl]
           -> [VarDecl]
           -> ExitSortDecl
           -> BExpDecl
           -> ProcDecl
mkProcDecl n l cs vs e b = ParseTree (Name n) ProcDeclE l (ProcComps cs vs e b)

procDeclName :: ProcDecl -> Text
procDeclName = nodeNameT

procDeclComps :: ProcDecl -> ProcComps
procDeclComps = child

procDeclBody :: ProcDecl -> BExpDecl
procDeclBody = procBody . procDeclComps

procDeclParams :: ProcDecl -> [VarDecl]
procDeclParams = procParams . procDeclComps

-- | Components of a process.
data ProcComps = ProcComps
    { procChParams :: [ChanDecl]
    , procParams   :: [VarDecl]
    , procRetSort  :: ExitSortDecl
    , procBody     :: BExpDecl
    } deriving (Eq, Show, Ord)

-- | Possible exit sorts of a process.
data ExitSortDecl = NoExitD
                  | ExitD [OfSort]
                  | HitD
    deriving (Eq, Show, Ord)

