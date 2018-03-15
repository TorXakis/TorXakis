{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Compiler where

import           Data.Text            ( Text )
import qualified Data.Text            as T
import           Control.Arrow        ( (|||), left, right )
import           Control.Monad        ( replicateM )
import           Control.Monad.State  ( State, get, put, evalState )
import           Control.Monad.Reader ( ReaderT, ask )
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import qualified Data.HashMap.Strict  as HMap
import           Control.Monad.Trans.Class (lift)
import           Data.Semigroup ((<>))

import           TxsDefs (TxsDefs, sortDefs, cstrDefs)
import qualified TxsDefs
import           Sigs    (Sigs)
import           VarId   (VarId (VarId))
import           SortId  (SortId (SortId), sortIdBool, sortIdInt, sortIdString)
import           CstrId  (CstrId (CstrId))
import           CstrDef (CstrDef (CstrDef))
import           FuncDef (FuncDef (FuncDef))
import           Id  (Id (Id))
import           FuncId  (FuncId (FuncId))
import           SortDef  (SortDef (SortDef))
import           ValExpr (cstrAccess, cstrVar)
import           TorXakis.Sort.ADTDefs ( ADTDefs, addADTDefs, emptyADTDefs, getDefs
                                       , ADTDef, Sort, adtConstructors, adtDefsToMap
                                       , adtSort, adtDefsToList
                                       , Sort (SortBool, SortInt, SortString)
                                       )
import           TorXakis.Sort.ConstructorDefs ( ConstructorDefs, ConstructorDef
                                               , constructorName, fields, cDefsToMap
                                               , constructors                                               
                                               )
import           TorXakis.Sort.FieldDefs (FieldDef, sort, fDefsToList)
import           TorXakis.Sort.Name (Name, toText, getName)
import           TorXakis.Sort.Ref  (Ref, mkRef)

import           TorXakis.Compiler.Error (Error)
import           TorXakis.Parser (ParsedDefs, adts)

-- | Compile a string into a TorXakis model.
--
-- TODO: for now we use String's to be able to leverage on the 'haskell' token
-- parser, in the future we might want to change this to text, and benchmark
-- what is the performance gain.
compile :: String -> Either Error (Id, TxsDefs, Sigs VarId)
compile = undefined

-- | Legacy compile function, used to comply with the old interface. It should
-- be deprecated in favor of @compile@.
compileLegacy :: String -> (Id, TxsDefs, Sigs VarId)
compileLegacy = (throwOnLeft ||| id) . compile
    where throwOnLeft = error . show

toTxsDefs :: ParsedDefs -> Either Error TxsDefs
toTxsDefs pd = right (\as -> evalState (adtDefsToTxsDefs as) 0) eas
    where
      eas :: Either Error ADTDefs
      eas = left (T.pack . show) $ addADTDefs (adts pd) emptyADTDefs

-- TODO: This function should be placed in a temporary module to allow
-- transforming all the new definitions into the old ones. After we have smart
-- constructors for all the layers of TorXakis we can get rid of this.
adtDefsToTxsDefs :: ADTDefs -> State Int TxsDefs
--
-- data  TxsDefs  =  TxsDefs { sortDefs   :: Map.Map SortId SortDef
--                           , cstrDefs   :: Map.Map CstrId CstrDef
--                           , funcDefs   :: Map.Map FuncId (FuncDef VarId)
--                           , varDefs    :: Map.Map VarId ()             -- MAYBE?
--                           }
--
-- TODO: wtf?
-- data  SortDef        = SortDef
adtDefsToTxsDefs ads = do
    adtSids <- sortIds ads
    let sm = Map.fromList adtSids
             `Map.union`
             -- TODO: check if we need to do this mapping here...
             Map.fromList [ (SortBool  , sortIdBool  )
                          , (SortInt   , sortIdInt   )
                          , (SortString, sortIdString)
                          ]
    cIds  <- cstrIds   sm ads
    cDefs <- adtDefsToCstrDefs sm ads
    let sids = snd <$> adtSids
    return $ TxsDefs.empty
        { sortDefs = Map.fromList $ zip sids (repeat SortDef)
        , cstrDefs = Map.fromList $ zip cIds cDefs
        }

sortIds :: ADTDefs -> State Int [(Sort, SortId)]
-- data SortId = SortId
--     { name :: Name            -- capid
--     , unid :: Id
--     }
sortIds adfs  = do
    let ts = adtSort <$> getDefs adfs
        ns = toText . getName <$> getDefs adfs
    is <- replicateM (length ns) nextId
    return $ zip ts $ uncurry SortId <$> zip ns is

type SortsMap = Map Sort SortId

cstrIds :: SortsMap -> ADTDefs -> State Int [CstrId]
cstrIds sm adfs = concat <$> traverse (cstrId sm) (adtDefsToList adfs)

cstrId :: SortsMap -> ADTDef Sort -> State Int [CstrId]
cstrId sm adf =
    cstrIdFromCstrs sm adfSort (adtConstructors adf)
    where
      Just adfSort = Map.lookup (adtSort adf) sm

cstrIdFromCstrs :: SortsMap
                -> SortId                 -- ^ SortId of the ADT to which the constructors belong to.
                -> ConstructorDefs Sort
                -> State Int [CstrId]
-- data CstrId = CstrId
--     { name     :: Name            -- capid
--     , unid     :: Id
--     , cstrargs :: [SortId]
--     , cstrsort :: SortId
--     }
--
cstrIdFromCstrs sm s cs =
    traverse (cstrIdFromCstr sm s) (constructors cs)

cstrIdFromCstr :: SortsMap
               -> SortId                 -- ^ SortId of the ADT to which the constructor belongs to.
               -> ConstructorDef Sort
               -> State Int CstrId
cstrIdFromCstr sm s c = do
    i <- nextId
    let n = toText (constructorName c)
        ps :: [SortId]
        ps = fieldSortId sm <$> (fDefsToList . fields) c
    return $ CstrId n i ps s

fieldSortId :: SortsMap -> FieldDef Sort -> SortId
fieldSortId ts f =
    -- At this point, all the sorts in the field definition have to exist,
    -- that's why we coerce the result of the 'lookup'.
    --
    -- TODO: maybe we can use a Monad Except/Error as a base monad, or Return
    -- an Either type, to avoid throwing errors in case this function is not
    -- used correctly.
    let Just sid = Map.lookup (sort f) ts in
    sid

-- TODO: we might need to put this function in a separate package (like StateId
-- or UuidsM or something like that).
nextId :: State Int Id
nextId = do
    i <- get
    put (i + 1)
    return (Id i)

-- | A constructor has a `isCstr` functions, and a list of accessors functions.
adtDefsToCstrDefs :: SortsMap -> ADTDefs -> State Int [CstrDef]
adtDefsToCstrDefs sm ads = concat <$>
    traverse (adtDefToCstrDef sm) (adtDefsToList ads)

adtDefToCstrDef :: SortsMap -> ADTDef Sort -> State Int [CstrDef]
adtDefToCstrDef sm adt =
    traverse (cDefsToCstrDef sm s) (constructors adt)
    where s = adtSort adt

cDefsToCstrDef :: SortsMap
               -> Sort -- ^ Sort of the containing ADT.
               -> ConstructorDef Sort
               -> State Int CstrDef
-- data  CstrDef       = CstrDef    FuncId [FuncId]
--
-- data FuncId = FuncId
-- { name     :: Name            -- smallid
-- , unid     :: Id
-- , funcargs :: [SortId]
-- , funcsort :: SortId
-- }
cDefsToCstrDef sm s c = do
    let n = getName c
        Just sid = Map.lookup s sm
    isCstrFunc   <- mkIsCstrFun n sid
    cstrAccFuncs <- traverse (mkCstrAccFunc sm s) (fDefsToList . fields $ c)
    return $ CstrDef isCstrFunc cstrAccFuncs

mkIsCstrFun :: Name -- ^ Name of the constructor
            -> SortId
            -> State Int FuncId
mkIsCstrFun n s = do
    i <- nextId
    return $ FuncId ("is" <> toText n) i [s] sortIdBool

mkCstrAccFunc :: SortsMap
              -> Sort  -- ^ Sort of the containing ADT.
              -> FieldDef Sort
              -> State Int FuncId
mkCstrAccFunc sm s f = do
    i <- nextId
    let Just adtSid = Map.lookup s sm
        Just fSid   = Map.lookup (sort f) sm
    return $ FuncId (toText . getName $ f) i [adtSid] fSid

mkCstrAccFuncDef :: SortsMap
                 -> CstrId        -- ^ Id of the containing constructor.
                 -> Int           -- ^ Position of the field in the constructor
                 -> FieldDef Sort
                 -> State Int (FuncDef VarId)
mkCstrAccFuncDef sm cid p f = do
    i <- nextId
    let v         = VarId (toText . getName $ f) i fSid
        Just fSid = Map.lookup (sort f) sm
    return $ FuncDef [v] $ cstrAccess cid p (cstrVar v)

-- * Compilation into FuncDef
-- data  FuncDef v      = FuncDef    [v] (ValExpr v)
-- FuncDef VarId
-- y = VarId "y" vid $$.inhDefgSort
-- FuncDef [y] (cstrAccess cid pos (cstrVar y))




