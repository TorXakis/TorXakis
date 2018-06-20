{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
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
-- Abstract syntax trees for the 'TorXakis' parser and compiler.
--------------------------------------------------------------------------------
module TorXakis.Parser.Data
    ( St
    , mkState
    , nodeLoc
    , nodeType
    , incId
    , nextId
    , Loc (Loc, PredefLoc, ExtraAut)
    , locFromLoc
    , line
    , start
    , locUid
    , locName
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
    , StautDeclE
    , StateDeclE
    , StateRefE
    , PurpDeclE
    , GoalDeclE
    , CnectDeclE
    , ActPrefE
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
    , varDeclSort
    , IVarDecl
    , mkIVarDecl
    , ivarDeclSort
    , VarRef
    , mkVarRef
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
    , ParLetVarDecl (ParLetVarDecl)
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
    , offerDecls
    , chanRefOfOfferDecl
    , chanOfferDecls
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
    , procDeclRetSort
    , procDeclChParams
    , procDeclParams
    , ProcComps
    , procChParams
    , procParams
    , procRetSort
    , procBody
    , procRefName
    , ExitSortDecl (..)
    -- ** State automata
    , StautDecl
    , StautItem (..)
    , StateDecl
    , StateRef
    , StUpdate (..)
    , Transition (..)
    , mkStautDecl
    , mkStateDecl
    , stateDeclName
    , mkStateRef
    , stateRefName
    , stautName
    , asProcDeclLoc
    , stautDeclChParams
    , stautDeclParams
    , stautDeclRetSort
    , stautDeclComps
    , stautDeclStates
    , stautDeclInnerVars
    , stautInitStates
    , stautTrans
    , InitStateDecl (..)
    , mkInitState
    -- ** Purposes
    , PurpDecl
    , mkPurpDecl
    , PurpComps (..)
    , purpDeclName
    , purpDeclIns
    , purpDeclOuts
    , purpDeclSyncs
    , purpDeclGoals
    , TestGoalDecl
    , mkTestGoalDecl
    , testGoalDeclName
    , testGoalDeclBExp
    -- ** Connections
    , CnectDecl
    , CnectType (..)
    , CnectItem (..)
    , CnectItemType (..)
    , CodecItem (..)
    , CodecType (..)
    , mkCnectDecl
    , cnectDeclName
    , cnectDeclType
    , cnectDeclCnectItems
    , cnectDeclCodecs
    -- ** Mappers
    , MapperDecl
    , MapperDeclE
    , mkMapperDecl
    , mapperName
    , mapperIns
    , mapperOuts
    , mapperSyncs
    , mapperBExp
    -- * Location of the entities
    , getLoc
    , loc'
    -- * Parsed definitions
    , ParsedDefs
    , adts
    , funcs
    , consts
    , models
    , chdecls
    , procs
    , emptyPds
    , stauts
    , purps
    , cnects
    , mappers
    )
where

import           Control.Lens            (Lens', (^..))
import           Control.Lens.Plated     (Plated, plate)
import           Control.Lens.TH         (makeLenses)
import           Data.Data               (Data)
import           Data.Data.Lens          (biplate, uniplate)
import           Data.Set                (Set)
import           Data.Text               (Text)
import           GHC.Exts                (IsList, Item, fromList, toList)

import           TorXakis.Compiler.Error

-- | State of the parser.
newtype St = St { nextId :: Int } deriving (Eq, Show)

-- | Create anew state.
mkState :: Int -> St
mkState = St

-- | Increment the id of the state.
incId :: St -> St
incId (St i) = St (i + 1)

-- | Abstract-syntax tree. The phantom type 't' denotes the type of the entity
-- (see types suffixed with 'E' like @ADTE@, @FuncDeclE@, etc). The type 'c' is
-- the type of the child of the parse tree.
data ParseTree t c = ParseTree
    { nodeName :: Name t
    , nodeType :: t
    , nodeLoc  :: Loc t
    , child    :: c
    } deriving (Show, Eq, Ord, Data)

-- | Instance needed to do generic traversals using functions like 'uniplate',
-- 'biplate', etc.
instance (Data t, Data c) => Plated (ParseTree t c) where
    plate = uniplate

-- | Name of an entity.
newtype Name t = Name { toText :: Text } deriving (Show, Eq, Ord, Data)

-- | Name of the tree node.
nodeNameT :: ParseTree t c -> Text
nodeNameT = toText . nodeName

-- | Location associated to the elements being parsed.
data Loc t
    = Loc
      { -- | Line in which a declaration occurs.
        line   :: Int
        -- | Start column.
      , start  :: Int
        -- | Unique identifier.
      , locUid :: Int
      }
    | PredefLoc
      { -- | Name of the predefined location
        locName :: Text
        -- | Unique identifier.
      , locUid  :: Int
      }
    -- NOTE: this is needed only to support the three kind of automata that are
    -- generated per each automaton declaration.
    | ExtraAut Text (Loc t)
    deriving (Show, Eq, Ord, Data)

-- | Instance needed from generic traversals using lenses.
instance Data t => Plated (Loc t) where
    plate = uniplate

-- | Change extract the location of the metadata, and change its type from 't'
-- to 'u'. This is useful when defining parsed entities whose locations
-- coincide, like expressions and variable-references or constant-literals.
locFromLoc :: Loc t -> Loc u
locFromLoc (Loc l c i)     = Loc l c i
locFromLoc (PredefLoc n i) = PredefLoc n i
locFromLoc (ExtraAut n l)  = ExtraAut n (locFromLoc l)

-- | Class of entities that have a location associated to them.
class HasLoc a t | a -> t where
    -- | Get the location of the entity.
    getLoc :: a -> Loc t
    -- | Set the location of the entity.
    setLoc :: a -> Loc t -> a
    -- | Location lens.
    loc' :: Lens' a (Loc t)
    loc' f a = setLoc a <$> f (getLoc a)

-- | A parse tree has a location.
instance HasLoc (ParseTree t c) t where
    getLoc = nodeLoc
    setLoc (ParseTree n t _ c) l' = ParseTree n t l' c

-- * Types of entities encountered when parsing.

-- | ADT.
data ADTE = ADTE deriving (Eq, Ord, Show, Data)

-- | Constructor.
data CstrE = CstrE  deriving (Eq, Ord, Show, Data)

-- | Field of a constructor.
data FieldE = FieldE deriving (Eq, Ord, Show, Data)

-- | Reference to an existing (previously defined or primitive) sort.
data SortRefE = SortRefE deriving (Eq, Ord, Show, Data)

-- | Function declaration.
data FuncDeclE = FuncDeclE deriving (Eq, Ord, Show, Data)

-- | An expression
data ExpDeclE = ExpDeclE deriving (Eq, Ord, Show, Data)

-- | A variable declaration.
data VarDeclE = VarDeclE deriving (Eq, Ord, Show, Data)

-- | A variable occurrence in an expression. It is assumed to be a
-- **reference** to an existing variable.
data VarRefE = VarRefE deriving (Eq, Ord, Show, Data)

-- | A constant literal
data ConstLitE = ConstLitE deriving (Eq, Ord, Show, Data)

-- | Channel declaration.
data ChanDeclE = ChanDeclE deriving (Eq, Ord, Show, Data)

-- | Channel  reference.
data ChanRefE = ChanRefE deriving (Eq, Ord, Show, Data)

-- | Model declaration.
data ModelDeclE = ModelDeclE deriving (Eq, Ord, Show, Data)

-- | Process declaration.
data ProcDeclE = ProcDeclE deriving (Eq, Ord, Show, Data)

-- | Process reference. Used at process instantiations.
data ProcRefE = ProcRefE deriving (Eq, Ord, Show, Data)

-- | State automaton declaration.
data StautDeclE = StautDeclE deriving (Eq, Ord, Show, Data)

-- | State declaration.
data StateDeclE = StateDeclE deriving (Eq, Ord, Show, Data)

-- | Reference to a state.
data StateRefE = StateRefE deriving (Eq, Ord, Show, Data)

-- | Purpose declaration.
data PurpDeclE = PurpDeclE deriving (Eq, Ord, Show, Data)

-- | Goal declaration.
data GoalDeclE = GoalDeclE deriving (Eq, Ord, Show, Data)

-- | Connect declaration.
data CnectDeclE = CnectDeclE deriving (Eq, Ord, Show, Data)

-- | Mapper declaration.
data MapperDeclE = MapperDeclE deriving (Eq, Ord, Show, Data)

-- | Parallel operator occurrence in a behavior expression.
data ParOpE = ParOpE deriving (Eq, Ord, Show, Data)

-- | Enable operator occurrence.
data EnableE = EnableE deriving (Eq, Ord, Show, Data)

-- | Disable operator occurrence.
data DisableE = DisableE deriving (Eq, Ord, Show, Data)

-- | Interrupt operator occurrence.
data InterruptE = InterruptE deriving (Eq, Ord, Show, Data)

-- | Choice operator occurrence.
data ChoiceE = ChoiceE deriving (Eq, Ord, Show, Data)

-- | Hide operator occurrence.
data HideE = HideE deriving (Eq, Ord, Show, Data)

-- | Accept operator.
data AcceptE = AcceptE deriving (Eq, Ord, Show, Data)

-- | Action prefix.
data ActPrefE = ActPrefE deriving (Eq, Ord, Show, Data)

-- * Types of parse trees.
type ADTDecl = ParseTree ADTE     [CstrDecl]

-- | Make an ADT declaration.
mkADTDecl :: Text -> Loc ADTE -> [CstrDecl] -> ADTDecl
mkADTDecl n = ParseTree (Name n) ADTE

-- | Name of the ADT declaration.
adtName :: ADTDecl -> Text
adtName = nodeNameT

-- | Constructors of the ADT.
constructors :: ADTDecl -> [CstrDecl]
constructors = child

-- | ADT constructor declaration.
type CstrDecl = ParseTree CstrE [FieldDecl]

-- | Make a constructor declaration.
mkCstrDecl :: Text -> Loc CstrE -> [FieldDecl] -> CstrDecl
mkCstrDecl n = ParseTree (Name n) CstrE

-- | Name of the constructor.
cstrName :: CstrDecl -> Text
cstrName = nodeNameT

-- | Fields of the constructor.
cstrFields :: CstrDecl -> [FieldDecl]
cstrFields = child

-- | Field declarations.
type FieldDecl = ParseTree FieldE OfSort

-- | Make a field declaration.
mkFieldDecl :: Text -> Loc FieldE -> OfSort -> FieldDecl
mkFieldDecl n = ParseTree (Name n) FieldE

-- | Name of the field declaration.
fieldName :: FieldDecl -> Text
fieldName = nodeNameT

-- | Get the field of a sort, and the metadata associated to it.
fieldSort :: FieldDecl -> (Text, Loc SortRefE)
fieldSort f = (nodeNameT . child $ f, nodeLoc . child $ f)

-- | Reference to an existing type
type OfSort    = ParseTree SortRefE ()

-- | Make a declaration of an entity belonging to a sort.
mkOfSort :: Text -> Loc SortRefE -> OfSort
mkOfSort n m = ParseTree (Name n) SortRefE m ()

-- | Name of the sort reference.
sortRefName :: OfSort -> Text
sortRefName = nodeNameT

-- | Components of a function.
data FuncComps = FuncComps
    { funcCompsParams  :: [VarDecl]
    , funcCompsRetSort :: OfSort
    , funcCompsBody    :: ExpDecl
    } deriving (Eq, Show, Ord, Data)

-- | Variable declarations (with an explicit sort).
type VarDecl = ParseTree VarDeclE OfSort

-- | Make a variable declaration.
mkVarDecl :: Text -> Loc VarDeclE -> OfSort -> VarDecl
mkVarDecl n = ParseTree (Name n) VarDeclE

-- | Implicit variable declaration (maybe with a sort associated to it).
type IVarDecl = ParseTree VarDeclE (Maybe OfSort)

-- | Make an implicit variable declaration, which might have an optional type
-- associated to it.
mkIVarDecl :: Text -> Loc VarDeclE -> Maybe OfSort -> IVarDecl
mkIVarDecl n = ParseTree (Name n) VarDeclE

ivarDeclSort :: IVarDecl -> Maybe OfSort
ivarDeclSort = child

-- | Entities that are variables.
class IsVariable v where
    -- | Name of a variable.
    varName :: v -> Text

-- | A variable declaration is a variable.
instance IsVariable VarDecl where
    varName = nodeNameT

-- | An implicit variable declaration is a variable.
instance IsVariable IVarDecl where
    varName = nodeNameT

-- | Sort of a variable declaration.
varDeclSort :: VarDecl -> (Text, Loc SortRefE)
varDeclSort f = (nodeNameT . child $ f, nodeLoc . child $ f)

-- | Declaration of an expression.
type ExpDecl = ParseTree ExpDeclE ExpChild

-- | Child of an expression.
expChild :: ExpDecl -> ExpChild
expChild = child

data ExpChild = VarRef (Name VarRefE) (Loc VarRefE)
              | ConstLit Const
              -- | A let expression allows to introduce a series of value
              -- bindings of the form:
              --
              -- > x0 = v0, ..., xn = vn
              --
              -- A let expression contains a list of lists that have the form
              -- above. Values declared within one '[LetVarDecl]' list
              -- introduce variable names in parallel (in the sense that one
              -- variable in the list cannot be used in the expressions within
              -- that list). However, values declared in '[LetVarDecl]' lists
              -- can be used in subsequent '[LetVarDecl]' lists. For instance,
              -- the following let expression:
              --
              -- > LET x = 1, y = 5; z = x + y IN ...
              --
              -- Will be parsed to the following list
              --
              -- > [[(x, 1), (y, 5)], [(z, x + y)]]
              --
              -- Here 'x' and 'y' cannot be used in the expressions of the
              -- first list, but it can be used in the expressions of the
              -- second.
              | LetExp [ParLetVarDecl] ExpDecl
              | If ExpDecl ExpDecl ExpDecl
              -- | Function application. A function is applied to a list of
              -- expressions.
              | Fappl (Name VarRefE) (Loc VarRefE) [ExpDecl]
    deriving (Eq, Ord, Show, Data)

-- | Constant declarations.
data Const = BoolConst Bool
           | IntConst Integer
           | StringConst Text
           | RegexConst Text
           | AnyConst
    deriving (Eq, Show, Ord, Data)

-- | Extract all the let-variable declarations of an expression.
--
expLetVarDecls :: ExpDecl -> [[LetVarDecl]]
expLetVarDecls ParseTree { child = VarRef _ _  } = []
expLetVarDecls ParseTree { child = ConstLit _  } = []
expLetVarDecls ParseTree { child = LetExp vs e }
    =  (toList <$> vs)
    ++ expLetVarDecls e
    ++ concatMap (concatMap (expLetVarDecls . varDeclExp)) (toList <$> vs)
expLetVarDecls ParseTree { child = If e0 e1 e2 } =
    expLetVarDecls e0 ++ expLetVarDecls e1 ++ expLetVarDecls e2
expLetVarDecls ParseTree { child = Fappl _ _ exs } =
    concatMap expLetVarDecls exs

-- | Get the child-expressions of an expression.
--
childExps :: ExpDecl -> [ExpDecl]
childExps ParseTree { child = VarRef _ _  }    = []
childExps ParseTree { child = ConstLit _  }    = []
childExps ParseTree { child = LetExp _ e }     = [e]
childExps ParseTree { child = If ex0 ex1 ex2 } = [ex0, ex1, ex2]
childExps ParseTree { child = Fappl _ _ exs }  = exs

-- | Make a let-expression declaration.
mkLetExpDecl :: [ParLetVarDecl] -> ExpDecl -> Loc ExpDeclE -> ExpDecl
mkLetExpDecl vss subEx l = mkExpDecl l (LetExp vss subEx)

-- | Make an expression declaration.
mkExpDecl :: Loc ExpDeclE -> ExpChild -> ExpDecl
mkExpDecl = ParseTree (Name "") ExpDeclE

-- | Let-variable declarations.
type LetVarDecl = ParseTree VarDeclE (Maybe OfSort, ExpDecl)

-- | Expression declaration in a let-variable declaration.
varDeclExp :: LetVarDecl -> ExpDecl
varDeclExp = snd . child

-- | A let-variable declaration is a variable.
instance IsVariable LetVarDecl where
    varName = nodeNameT

-- | Sort name of a let-variable declaration.
letVarDeclSortName :: LetVarDecl -> Maybe (Text, Loc SortRefE)
letVarDeclSortName vd = do
    srt <- fst . child $ vd
    return (nodeNameT srt, nodeLoc srt)

-- | Make a let variable declaration.
mkLetVarDecl :: Text -> Maybe OfSort -> ExpDecl -> Loc VarDeclE -> LetVarDecl
mkLetVarDecl n ms subEx m = ParseTree (Name n) VarDeclE m (ms, subEx)

-- | Parallel let-variable declarations.
newtype ParLetVarDecl = ParLetVarDecl [LetVarDecl] deriving (Eq, Ord, Show, Data)

-- | A parallel let-variable declaration can be converted to and from lists.
instance IsList ParLetVarDecl where
    type Item ParLetVarDecl = LetVarDecl
    fromList = ParLetVarDecl
    toList (ParLetVarDecl ls) = ls

-- | Make an if-then-else expression declaration.
mkITEExpDecl :: Loc ExpDeclE -> ExpDecl -> ExpDecl -> ExpDecl -> ExpDecl
mkITEExpDecl l ex0 ex1 ex2 = mkExpDecl l (If ex0 ex1 ex2)

-- | Make a function-application expression declaration.
mkFappl :: Loc ExpDeclE -> Loc VarRefE -> Text -> [ExpDecl] -> ExpDecl
mkFappl le lr n exs = mkExpDecl le (Fappl (Name n) lr exs)

-- | Make a variable expression. The location of the expression will become the
-- location of the variable.
mkVarExp :: Loc ExpDeclE -> Text -> ExpDecl
mkVarExp l n = mkExpDecl l (VarRef (Name n) (locFromLoc l))

-- | Make a Boolean constant expression.
mkBoolConstExp :: Loc ExpDeclE -> Bool -> ExpDecl
mkBoolConstExp l b = mkExpDecl l (ConstLit (BoolConst b))

-- | Make an integer constant expression.
mkIntConstExp :: Loc ExpDeclE -> Integer -> ExpDecl
mkIntConstExp l i = mkExpDecl l (ConstLit (IntConst i))

-- | Make a string constant expression.
mkStringConstExp :: Loc ExpDeclE -> Text -> ExpDecl
mkStringConstExp l t = mkExpDecl l (ConstLit (StringConst t))

-- | Make a regex constant expression.
mkRegexConstExp :: Loc ExpDeclE -> Text -> ExpDecl
mkRegexConstExp l t = mkExpDecl l (ConstLit (RegexConst t))

-- | Function declarations.
type FuncDecl  = ParseTree FuncDeclE FuncComps

-- | Make a function declaration.
mkFuncDecl :: Text -> Loc FuncDeclE -> [VarDecl] -> OfSort -> ExpDecl -> FuncDecl
mkFuncDecl n l ps s b = ParseTree (Name n) FuncDeclE l (FuncComps ps s b)

-- | Name of the function declaration.
funcName :: FuncDecl -> Text
funcName = nodeNameT

-- | Lens to the function name of a declaration.
funcNameL :: Lens' FuncDecl Text
funcNameL = undefined

-- | Function parameters.
funcParams :: FuncDecl -> [VarDecl]
funcParams = funcCompsParams . child

-- | Function body,
funcBody :: FuncDecl -> ExpDecl
funcBody = funcCompsBody . child

-- | Return sort of a function.
funcRetSort :: FuncDecl -> (Text, Loc SortRefE)
funcRetSort f = ( nodeNameT . funcCompsRetSort . child $ f
                , nodeLoc . funcCompsRetSort . child $ f
                )

-- | A location can be converted to an error location.
instance HasErrorLoc (Loc t) where
    getErrorLoc (Loc l c _)     = ErrorLoc {errorLine = l, errorColumn = c}
    getErrorLoc (PredefLoc n _) = ErrorPredef n
    getErrorLoc (ExtraAut _ l)  = getErrorLoc l

-- | A parse tree can be converted to an error location.
instance HasErrorLoc (ParseTree t c) where
    getErrorLoc pt = ErrorLoc { errorLine = l, errorColumn = c }
        where Loc l c _ = nodeLoc pt

-- | Model declaration.
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

-- | Name of the model.
modelName :: ModelDecl -> Text
modelName = nodeNameT

-- | Model input channels.
modelIns :: ModelDecl -> [ChanRef]
modelIns = inchs . child

-- | Model output channels.
modelOuts :: ModelDecl -> [ChanRef]
modelOuts = outchs . child

-- | Model synchronization channels.
modelSyncs :: ModelDecl -> Maybe [Set ChanRef]
modelSyncs = synchs . child

-- | Model behavior expressions.
modelBExp :: ModelDecl -> BExpDecl
modelBExp = bexp . child

-- | Channel references.
type ChanRef = ParseTree ChanRefE ()

-- | Make a channel reference.
mkChanRef :: Text         -- ^ Name of the channel that is being referred.
          -> Loc ChanRefE -- ^ Location where the reference took place.
          -> ChanRef
mkChanRef n l = ParseTree (Name n) ChanRefE l ()

-- | Name of the channel reference.
chanRefName :: ChanRef -> Text
chanRefName = nodeNameT

-- | Components of a model.
data ModelComps = ModelComps
    { inchs  :: [ChanRef]
    , outchs :: [ChanRef]
    , synchs :: Maybe [Set ChanRef]
    , bexp   :: BExpDecl
    } deriving (Eq, Ord, Show, Data)

-- | Behavior expression declaration.
data BExpDecl
    -- | 'STOP' operator.
    = Stop
    -- | '>->' (action prefix) operator.
    | ActPref (Loc ActPrefE)  ActOfferDecl BExpDecl
    -- | 'LET' declarations for behavior expressions.
    | LetBExp  [ParLetVarDecl] BExpDecl
    -- | Process instantiation.
    | Pappl (Name ProcRefE) (Loc ProcRefE) [ChanRef] [ExpDecl]
    -- | Parallel operators.
    | Par (Loc ParOpE) SyncOn BExpDecl BExpDecl
    -- | Enable operator.
    | Enable (Loc EnableE) BExpDecl BExpDecl
    -- | 'ACCEPT' operator.
    --
    -- Note that while the parser will allow 'ACCEPT's in arbitrary positions,
    -- the compiler will check that they only occur after an enable operator
    -- ('>>>')
    | Accept (Loc AcceptE) [ChanOfferDecl] BExpDecl
    -- | Disable operator.
    | Disable (Loc DisableE) BExpDecl BExpDecl
    -- | Interrupt operator.
    | Interrupt (Loc InterruptE) BExpDecl BExpDecl
    -- | Choice operator.
    | Choice (Loc ChoiceE) BExpDecl BExpDecl
    -- | Guard operator.
    | Guard ExpDecl BExpDecl
    -- | Hide operator.
    | Hide (Loc HideE) [ChanDecl] BExpDecl
    deriving (Eq, Ord, Show, Data)

-- | Channels to sync on in a parallel operator.
data SyncOn = All              -- ^ Sync on all channels, this is the result of
                               -- parsing '||'
            | OnlyOn [ChanRef] -- ^ Sync only on the given channels. This is
                               -- the result of parsing either '|||' or
                               -- '|[...]|'. Parsing '|||' will result in an
                               -- empty list, meaning that full interleaving is
                               -- allowed.
            deriving (Eq, Ord, Show, Data)

-- | Process reference name.
procRefName :: Text -> Name ProcRefE
procRefName = Name

-- | Action offer declaration.
data ActOfferDecl = ActOfferDecl
    { _offers     :: [OfferDecl]
    , _constraint :: Maybe ExpDecl
    } deriving (Eq, Ord, Show, Data)

-- | Offer declaration.
data OfferDecl = OfferDecl ChanRef [ChanOfferDecl]
    deriving (Eq, Ord, Show, Data)

-- | Reference to a channel in an offer declaration.
chanRefOfOfferDecl :: OfferDecl -> ChanRef
chanRefOfOfferDecl (OfferDecl cr _) = cr

-- | Channel offer declarations.
--
-- Note that a receiving action with an explicit type declaration are only
-- needed to simplify the type inference of exit variables used in expressions
-- of the form 'EXIT ? v :: T'.
data ChanOfferDecl = QuestD IVarDecl
                   | ExclD  ExpDecl
    deriving (Eq, Ord, Show, Data)

-- | Implicit variable (if any) of a channel offer declaration.
chanOfferIvarDecl :: ChanOfferDecl -> Maybe IVarDecl
chanOfferIvarDecl (QuestD iv) = Just iv
chanOfferIvarDecl _           = Nothing

-- | Transform the location of a variable declaration into the location of a
-- variable reference. A variable declaration refers to itself.
--
-- We do not want to export @locFromLoc@ since we will be loosing the
-- type-guarantees offered by the phantom type. By having @locFromLoc@ private
-- to this module we are in control of which conversions are allowed.
asVarReflLoc :: Loc VarDeclE -> Loc VarRefE
asVarReflLoc = locFromLoc

-- | Get all the variable declarations introduced by receiving actions of the
-- form 'Ch ? v'.
actOfferDecls :: ActOfferDecl -> [IVarDecl]
actOfferDecls (ActOfferDecl os _) = concatMap offerDecls os

-- | Get the implicit variable declarations of an offer declaration.
offerDecls :: OfferDecl -> [IVarDecl]
offerDecls (OfferDecl _ cs) = concatMap chanOfferDecls cs

-- | Implicit variable declarations of a channel offer.
chanOfferDecls :: ChanOfferDecl -> [IVarDecl]
chanOfferDecls (QuestD ivd) = [ivd]
chanOfferDecls (ExclD _)    = []

-- | Variable references.
type VarRef = ParseTree VarRefE ()

-- | Make a variable reference.
mkVarRef :: Text -> Loc VarRefE -> VarRef
mkVarRef n l = ParseTree (Name n) VarRefE l ()

-- | A variable reference is a variable.
instance IsVariable VarRef where
    varName = nodeNameT

-- | Channel declaration.
type ChanDecl = ParseTree ChanDeclE [OfSort]

-- | Make a channel declaration.
mkChanDecl :: Text -> Loc ChanDeclE -> [OfSort] -> ChanDecl
mkChanDecl n = ParseTree (Name n) ChanDeclE

-- | Name of the channel declaration.
chanDeclName :: ChanDecl -> Text
chanDeclName = nodeNameT

-- | Sorts of a channel declaration with their locations, which can be used for
-- lookup and error reporting.
chanDeclSorts :: ChanDecl -> [(Text, Loc SortRefE)]
chanDeclSorts ch = zip (fmap nodeNameT . child $ ch)
                       (fmap nodeLoc   . child $ ch)

-- | Process declaration.
type ProcDecl = ParseTree ProcDeclE ProcComps

-- | Components of a process.
data ProcComps = ProcComps
    { procChParams :: [ChanDecl]
    , procParams   :: [VarDecl]
    , procRetSort  :: ExitSortDecl
    , procBody     :: BExpDecl
    } deriving (Eq, Show, Ord, Data)

-- | Make a process declaration.
mkProcDecl :: Text
           -> Loc ProcDeclE
           -> [ChanDecl]
           -> [VarDecl]
           -> ExitSortDecl
           -> BExpDecl
           -> ProcDecl
mkProcDecl n l cs vs e b = ParseTree (Name n) ProcDeclE l (ProcComps cs vs e b)

-- | Name of a process declaration.
procDeclName :: ProcDecl -> Text
procDeclName = nodeNameT

-- | Components of a process declaration.
procDeclComps :: ProcDecl -> ProcComps
procDeclComps = child

-- | Parameters of a process declaration.
procDeclChParams :: ProcDecl -> [ChanDecl]
procDeclChParams = procChParams . procDeclComps

-- | Parameters of a process declaration.
procDeclParams :: ProcDecl -> [VarDecl]
procDeclParams = procParams . procDeclComps

-- | Return sort of a process declaration.
procDeclRetSort :: ProcDecl -> ExitSortDecl
procDeclRetSort = procRetSort . procDeclComps

-- | Body of a process declaration.
procDeclBody :: ProcDecl -> BExpDecl
procDeclBody = procBody . procDeclComps

-- | Possible exit sorts of a process.
data ExitSortDecl = NoExitD
                  | ExitD [OfSort]
                  | HitD
    deriving (Eq, Show, Ord, Data)

-- | State automaton declaration.
type StautDecl = ParseTree StautDeclE StautComps

-- | Make a state automaton declaration.
mkStautDecl :: Text
            -> Loc StautDeclE
            -> [ChanDecl]
            -> [VarDecl]
            -> ExitSortDecl
            -> [StautItem]
            -> StautDecl
mkStautDecl n l cs vs e is = ParseTree (Name n) StautDeclE l (StautComps cs vs e is)

-- | Name of the state automaton.
stautName :: StautDecl -> Text
stautName = nodeNameT

-- | Return the location of a state automaton as the location of a process declaration.
asProcDeclLoc :: StautDecl -> Loc ProcDeclE
asProcDeclLoc = locFromLoc . getLoc

-- | Get the channel declarations of a state automaton.
stautDeclChParams :: StautDecl -> [ChanDecl]
stautDeclChParams = stautChParams . child

-- | Get the formal parameters of a state automaton.
stautDeclParams :: StautDecl -> [VarDecl]
stautDeclParams = stautParams . child

-- | Get the return sort of a state automaton.
stautDeclRetSort :: StautDecl -> ExitSortDecl
stautDeclRetSort = stautRetSort . child

-- | Get the components of a state automaton.
stautDeclComps :: StautDecl -> [StautItem]
stautDeclComps = stautComps . child

-- | Components of a state automaton.
data StautComps = StautComps
    { stautChParams :: [ChanDecl]
    , stautParams   :: [VarDecl]
    , stautRetSort  :: ExitSortDecl
    , stautComps    :: [StautItem]
    } deriving (Eq, Show, Ord, Data)

-- | Item of a state automaton.
data StautItem = States [StateDecl]
               | StVarDecl [VarDecl]
               | InitState InitStateDecl
               | Trans [Transition]
    deriving (Eq, Show, Ord, Data)

-- | Extract the states declared in the automaton.
stautDeclStates :: StautDecl -> [StateDecl]
stautDeclStates staut = staut ^.. biplate

-- | Extract the variables declared in the automaton.
stautDeclInnerVars :: StautDecl -> [VarDecl]
stautDeclInnerVars staut = stautComps (child staut) ^.. biplate

-- | Extract the initial states declared in the automaton.
stautInitStates :: StautDecl -> [InitStateDecl]
stautInitStates staut = staut ^.. biplate

-- | Extract the transitions declared in the automaton.
stautTrans :: StautDecl -> [Transition]
stautTrans staut = concat $ staut ^.. biplate

-- | Declaration of an automaton state.
type StateDecl = ParseTree StateDeclE ()

-- | Make a state declaration.
mkStateDecl :: Text -> Loc StateDeclE -> StateDecl
mkStateDecl n l = ParseTree (Name n) StateDeclE l ()

-- | Name the state declaration.
stateDeclName :: StateDecl -> Text
stateDeclName = nodeNameT

-- | Declaration of an initial state.
data InitStateDecl = InitStateDecl StateRef [StUpdate]
    deriving (Eq, Ord, Show, Data)

-- | Make an initial state declaration.
mkInitState :: StateRef -> [StUpdate] -> StautItem
mkInitState s uds = InitState (InitStateDecl s uds)

-- | Reference to a previously declared automaton state.
type StateRef = ParseTree StateRefE ()

-- | Make a state reference.
mkStateRef :: Text -> Loc StateRefE -> StateRef
mkStateRef n l = ParseTree (Name n) StateRefE l ()

-- | Name of the state reference.
stateRefName :: StateRef -> Text
stateRefName = nodeNameT

-- | State automaton update.
data StUpdate = StUpdate [VarRef] ExpDecl
    deriving (Eq, Show, Ord, Data)

-- | State automaton transition.
data Transition = Transition StateRef ActOfferDecl [StUpdate] StateRef
    deriving (Eq, Show, Ord, Data)

-- | Purpose declaration.
type PurpDecl = ParseTree PurpDeclE PurpComps

-- | Components of a purpose.
data PurpComps = PurpComps
    { purpIns   :: [ChanRef]
    , purpOuts  :: [ChanRef]
    , purpSyncs :: Maybe [Set ChanRef]
    , goals     :: [TestGoalDecl]
    } deriving (Eq, Show, Data)

-- | Declaration of a test goal.
type TestGoalDecl = ParseTree GoalDeclE BExpDecl

-- | Make a purpose declaration.
mkPurpDecl :: Text
           -> Loc PurpDeclE
           -> [ChanRef]
           -> [ChanRef]
           -> Maybe [Set ChanRef]
           -> [TestGoalDecl]
           -> PurpDecl
mkPurpDecl n l is os ys ts =
    ParseTree (Name n) PurpDeclE l (PurpComps is os ys ts)

-- | Name of the purpose declaration.
purpDeclName :: PurpDecl -> Text
purpDeclName = nodeNameT

-- | Input channels of a purpose declaration.
purpDeclIns :: PurpDecl -> [ChanRef]
purpDeclIns = purpIns . child

-- | Output channels of a purpose declaration.
purpDeclOuts :: PurpDecl -> [ChanRef]
purpDeclOuts = purpOuts . child

-- | Synchronization channels of a purpose declaration.
purpDeclSyncs :: PurpDecl -> Maybe [Set ChanRef]
purpDeclSyncs = purpSyncs . child

-- | Goals of a purpose declaration.
purpDeclGoals :: PurpDecl -> [TestGoalDecl]
purpDeclGoals = goals . child

-- | Make a test goal declaration.
mkTestGoalDecl :: Text -> Loc GoalDeclE -> BExpDecl -> TestGoalDecl
mkTestGoalDecl n = ParseTree (Name n) GoalDeclE

-- | Name of a test goal declaration.
testGoalDeclName :: TestGoalDecl -> Text
testGoalDeclName = nodeNameT

-- | Behavior expression of a test goal declaration.
testGoalDeclBExp :: TestGoalDecl -> BExpDecl
testGoalDeclBExp = child

-- | 'CNECTDEF' declaration.
type CnectDecl = ParseTree CnectDeclE (CnectType, [CnectItem], [CodecItem])

-- | Connect type declaration.
data CnectType = CTClient | CTServer
    deriving (Eq, Show, Data)

-- | Connect item declaration.
data CnectItem = CnectItem
    { cnectCh   :: ChanRef
    , cnectType :: CnectItemType
    , host      :: Text
    , port      :: Integer
    } deriving (Eq, Show, Data)

-- | Type of the connect item.
data CnectItemType = ChanIn | ChanOut
    deriving (Eq, Ord, Show, Data)

-- | Codec item declaration.
data CodecItem = CodecItem
      { codecOffer   :: OfferDecl
      , codecChOffer :: ChanOfferDecl
      , codecType    :: CodecType
      }
    deriving (Eq, Show, Data)

-- | Type of the codec item.
data CodecType = Decode | Encode
    deriving (Eq, Show, Data)

-- | Make a connect declaration.
mkCnectDecl :: Text
            -> Loc CnectDeclE
            -> CnectType
            -> [CnectItem]
            -> [CodecItem]
            -> CnectDecl
mkCnectDecl n l ct is cs = ParseTree (Name n) CnectDeclE l (ct, is, cs)

-- | Name of the connect declaration.
cnectDeclName :: CnectDecl -> Text
cnectDeclName = nodeNameT

-- | Type of the connect declaration.
cnectDeclType :: CnectDecl -> CnectType
cnectDeclType = fst3 . child
    where fst3 (f, _, _) = f

-- | Connect items of the connect declaration.
cnectDeclCnectItems :: CnectDecl -> [CnectItem]
cnectDeclCnectItems = snd3 . child
    where snd3 (_, s, _) = s

-- | Codec items of the connect declaration.
cnectDeclCodecs :: CnectDecl -> [CodecItem]
cnectDeclCodecs = thrd . child
    where thrd (_, _, t) = t

-- | Mapper declaration.
type MapperDecl = ParseTree MapperDeclE ModelComps

-- | Make a mapper declaration.
mkMapperDecl :: Text
            -> Loc MapperDeclE
            -> [ChanRef]
            -> [ChanRef]
            -> Maybe [Set ChanRef]
            -> BExpDecl
            -> MapperDecl
mkMapperDecl n l is os ys be =
    ParseTree (Name n) MapperDeclE l (ModelComps is os ys be)

-- | Name of the mapper.
mapperName :: MapperDecl -> Text
mapperName = nodeNameT

-- | Input channels of the mapper declaration.
mapperIns :: MapperDecl -> [ChanRef]
mapperIns = inchs . child

-- | Output channels of the mapper declaration.
mapperOuts :: MapperDecl -> [ChanRef]
mapperOuts = outchs . child

-- | Synchronization channels of the mapper declaration.
mapperSyncs :: MapperDecl -> Maybe [Set ChanRef]
mapperSyncs = synchs . child

-- | Behavior expression of the mapper declaration.
mapperBExp :: MapperDecl -> BExpDecl
mapperBExp = bexp . child

-- | TorXakis definitions generated by the parser.
data ParsedDefs = ParsedDefs
    { _adts    :: [ADTDecl]
    , _funcs   :: [FuncDecl]
    , _consts  :: [FuncDecl]
    , _models  :: [ModelDecl]
    , _chdecls :: [ChanDecl]
    , _procs   :: [ProcDecl]
    , _stauts  :: [StautDecl]
    , _purps   :: [PurpDecl]
    , _cnects  :: [CnectDecl]
    , _mappers :: [MapperDecl]
    } deriving (Eq, Show, Data)
makeLenses ''ParsedDefs

-- | Empty parsed definitions.
emptyPds :: ParsedDefs
emptyPds = ParsedDefs [] [] [] [] [] [] [] [] [] []
