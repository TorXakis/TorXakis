{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Compiler
-- Copyright   :  (c) TNO and Radboud University
-- License     :  BSD3 (see the file license.txt)
--
-- Maintainer  :  damian.nadales@gmail.com (Embedded Systems Innovation by TNO)
-- Stability   :  experimental
-- Portability :  portable
--
--  Compiler for the 'TorXakis' language.
--------------------------------------------------------------------------------
module TorXakis.Compiler
    ( compileFile
    , compileString
    , compileUnsafe
    , compileValDefs
    , compileVarDecls
    , compileValExpr
    , compileBExpr
    , compileOffer
    , compileLegacy
    )
where

import           Control.Arrow                      (second, (|||))
import           Control.Lens                       ((^.), (^..))
import           Control.Monad                      (forM)
import           Control.Monad.Except               (liftEither)
import           Control.Monad.State                (evalStateT)
import           Data.Data                          (Data)
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)

import           BehExprDefs                        (Offer, chanIdIstep)
import           ChanId                             (ChanId)
import qualified ChanId
import           CstrId                             (CstrId)
import           FuncDef                            (FuncDef)
import           FuncId                             (FuncId, name)
import           FuncTable                          (FuncTable, Handler,
                                                     Signature, toMap)
import           Id                                 (Id (Id), _id)
import           Sigs                               (Sigs, chan, func, pro,
                                                     sort, uniqueCombine)
import qualified Sigs                               (empty)
import           SortId                             (SortId, sortIdBool,
                                                     sortIdInt, sortIdRegex,
                                                     sortIdString)
import           StdTDefs                           (stdFuncTable, stdTDefs)
import           TxsDefs                            (BExpr, ProcDef,
                                                     ProcId, TxsDefs,
                                                     chanid, cnectDefs,
                                                     fromList, funcDefs,
                                                     mapperDefs, modelDefs,
                                                     procDefs, purpDefs, union)
import qualified TxsDefs                            (empty)
import           ValExpr                            (ValExpr)
import           VarId                              (VarId, varsort)
import qualified VarId

import           TorXakis.Compiler.Data             (CompilerM, getNextId,
                                                     getUnid, newState,
                                                     runCompiler, setUnid)
import           TorXakis.Compiler.Data.ProcDecl    (ProcInfo, getPId)
import           TorXakis.Compiler.Defs.BehExprDefs (toBExpr, toOffer)
import           TorXakis.Compiler.Defs.FuncTable   (adtsToFuncTable,
                                                     fLocToSignatureHandlers)
import           TorXakis.Compiler.Defs.ProcDef     (procDeclsToProcDefMap,
                                                     stautDeclsToProcDefMap)
import           TorXakis.Compiler.Defs.Sigs        (adtDeclsToSigs,
                                                     funDeclsToSigs,
                                                     sortsToSigs)
import           TorXakis.Compiler.Defs.TxsDefs     (adtsToTxsDefs,
                                                     cnectDeclsToTxsDefs,
                                                     mapperDeclsToTxsDefs,
                                                     modelDeclsToTxsDefs,
                                                     purpDeclsToTxsDefs)
import           TorXakis.Compiler.Error            (Entity (Channel, Function, Process),
                                                     Error,
                                                     ErrorLoc (NoErrorLoc))
import           TorXakis.Compiler.Maps             (getUniqueElement,
                                                     idefsNames, (.@@))
import           TorXakis.Compiler.Maps.DefinesAMap (DefinesAMap, getMap)
import           TorXakis.Compiler.Maps.VarRef      (varDefsFromExp)
import           TorXakis.Compiler.MapsTo           ((:&) ((:&)), Contents, In,
                                                     MapsTo, innerMap, values,
                                                     (<.+>))
import           TorXakis.Compiler.Simplifiable     (simplify)
import           TorXakis.Compiler.ValExpr.CstrId   (compileToCstrId)
import           TorXakis.Compiler.ValExpr.ExpDecl  (HasVarReferences,
                                                     mapRefToDecls)
import           TorXakis.Compiler.ValExpr.FuncDef  (FuncDefInfo, funcDeclToSH,
                                                     funcDeclsToFuncDefs,
                                                     funcDef,
                                                     innerSigHandlerMap)
import           TorXakis.Compiler.ValExpr.FuncId   (adtsToFuncIds,
                                                     funcDeclsToFuncIds,
                                                     funcIdAsSignature,
                                                     getStdFuncIds)
import           TorXakis.Compiler.ValExpr.SortId   (HasTypedVars,
                                                     compileToSortId,
                                                     inferExpTypes, inferTypes,
                                                     inferVarTypes)
import           TorXakis.Compiler.ValExpr.ValExpr  (expDeclToValExpr,
                                                     parValDeclToMap)
import           TorXakis.Compiler.ValExpr.VarId    (DeclaresVariables,
                                                     generateVarIds, mkVarIds)
import           TorXakis.Compiler.Validation
import           TorXakis.Parser                    (parse, parseFile,
                                                     parseString)
import           TorXakis.Parser.BExpDecl           (bexpDeclP, offersP)
import           TorXakis.Parser.Common             (TxsParser)
import           TorXakis.Parser.Data               (ChanDeclE, ChanRefE, CstrE,
                                                     FuncDecl, FuncDeclE,
                                                     Loc (PredefLoc),
                                                     ParsedDefs, ProcDeclE,
                                                     VarDeclE, VarRefE, adts,
                                                     chanDeclName, chdecls,
                                                     cnects, consts, funcs,
                                                     getLoc, loc', mappers,
                                                     models, procs, purps,
                                                     stauts)
import           TorXakis.Parser.ValExprDecl        (letVarDeclsP, valExpP)
import           TorXakis.Parser.VarDecl            (varDeclsP)

-- | Compile a file into a TorXakis model.
--
compileFile :: FilePath -> IO (Either Error (Id, TxsDefs, Sigs VarId))
compileFile fp = parseFile fp >>= compileParsedDefsIO

-- | Compile a string into a TorXakis model.
--
compileString :: String -> IO (Either Error (Id, TxsDefs, Sigs VarId))
compileString str = compileParsedDefsIO $ parseString "" str

compileParsedDefsIO :: Either Error ParsedDefs -> IO (Either Error (Id, TxsDefs, Sigs VarId))
compileParsedDefsIO (Left err) = return . Left $ err
compileParsedDefsIO (Right pd) = return $
    evalStateT (runCompiler . compileParsedDefs $ pd) newState

-- | Run the compiler throwing an error if the compiler returns an 'Error'.
compileUnsafe :: CompilerM a -> a
compileUnsafe cmp = throwOnError $
    evalStateT (runCompiler cmp) newState

-- | Legacy compile function, used to comply with the old interface. It should
-- be deprecated in favor of 'compileFile'.
compileLegacy :: String -> (Id, TxsDefs, Sigs VarId)
compileLegacy str =
    case parseString "" str of
        Left err -> error $ show err
        Right pd ->
            compileUnsafe (compileParsedDefs pd)

-- | Call 'error' if the result is 'Left'.
throwOnError :: Either Error a -> a
throwOnError = throwOnLeft ||| id
    where throwOnLeft = error . show

-- | Compile parsed definitions into TorXakis data-types.
compileParsedDefs :: ParsedDefs -> CompilerM (Id, TxsDefs, Sigs VarId)
compileParsedDefs parsedDefs = do
    -- Generate a map from 'Text' to 'SortId' using the ADT's that are declared
    -- in 'parsedDefs', as well as the predefined Sorts ("Bool", "Int", "Regex",
    -- "String").
    sIds <- compileToSortIds parsedDefs

    -- Generate a map from constructor declarations to 'CstrId''s.
    cstrIds <- compileToCstrId sIds (parsedDefs ^. adts)

    -- Generate a map from locations of function declarations to 'FuncId''s.
    -- This map includes the predefined functions (standard functions) such as
    -- '*', '++', 'toString', 'fromString'.
    stdFuncIdsL  <- getStdFuncIds
    cstrFuncIdsL <- adtsToFuncIds sIds (parsedDefs ^. adts)
    fIdsL        <- funcDeclsToFuncIds sIds (allFuncs parsedDefs)
    checkUnique (NoErrorLoc, Function, "Functions ")
                (fmap snd $ stdFuncIdsL <> cstrFuncIdsL <> fIdsL)
    let
        stdFuncIds = Map.fromList stdFuncIdsL
        cstrFuncIds = Map.fromList cstrFuncIdsL
        fIds = Map.fromList fIdsL
        allFids = stdFuncIds <> cstrFuncIds <> fIds
        lfDefs = compileToFuncLocs allFids

    -- Generate a map from locations of variable references to the location in
    -- which these entities (variables or functions) are declared.
    decls <- compileToDecls lfDefs parsedDefs

    -- Infer the types of all variable declarations.
    let emptyVdMap = Map.empty :: Map (Loc VarDeclE) SortId
    -- We pass to 'inferTypes' an empty map from 'Loc VarDeclE' to 'SortId'
    -- since no variables can be declared at the top level.
    let allFSigs = funcIdAsSignature <$> allFids
    vdSortMap <- inferTypes (sIds :& decls :& allFSigs :& emptyVdMap) (allFuncs parsedDefs)
    -- Construct the variable declarations to 'VarId''s lookup table.
    vIds <- generateVarIds vdSortMap (allFuncs parsedDefs)

    -- Create a map from locations of function declarations to their signature
    -- and handlers.
    adtsFt <- adtsToFuncTable (sIds :& cstrIds) (parsedDefs ^. adts)
    stdSHs <- fLocToSignatureHandlers stdFuncIds stdFuncTable
    adtsSHs <- fLocToSignatureHandlers cstrFuncIds adtsFt
    fSHs <- Map.fromList <$> traverse (funcDeclToSH allFids) (allFuncs parsedDefs)

    -- Generate a map from function declarations to their definitions.
    fdefs <- funcDeclsToFuncDefs (vIds :& allFids :& decls)
                                 (stdSHs <> adtsSHs <> fSHs)
                                 (allFuncs parsedDefs)
    let fdefsSHs = innerSigHandlerMap (fIds :& fdefs)
        allFSHs = stdSHs <> adtsSHs <> fdefsSHs

    -- Generate a map from process declarations to their definitions.
    pdefs <- compileToProcDefs (sIds :& cstrIds :& allFids :& allFSHs :& decls) parsedDefs
    checkUnique (NoErrorLoc, Channel, "Channel definition")
                (chanDeclName <$> (parsedDefs ^. chdecls))
    chIds <- getMap sIds (parsedDefs ^. chdecls) :: CompilerM (Map (Loc ChanDeclE) ChanId)
    let mm = sIds :& pdefs :& cstrIds :& allFids :& fdefs

    -- Generate the 'TorXakis' 'Sigs'.
    sigs    <- toSigs (mm :& chIds) parsedDefs

    -- Generate the 'TorXakis' 'TxsDefs'.
    chNames <-  getMap () (parsedDefs ^. chdecls) :: CompilerM (Map Text (Loc ChanDeclE))
    -- We need the map from channel names to the locations in which these
    -- channels are declared, because the model definitions rely on channels
    -- declared outside its scope.
    txsDefs <- toTxsDefs (func sigs) (mm :& decls :& vIds :& vdSortMap :& chNames :& chIds :& allFSHs) parsedDefs

    -- We need to return the next unique id as well.
    i <- getUnid
    return (Id i, txsDefs, sigs)

-- | Compile the parsed definitions to 'TxsDefs'.
toTxsDefs :: ( MapsTo Text        SortId mm
             , MapsTo (Loc CstrE) CstrId mm
             , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
             , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm
             , MapsTo (Loc FuncDeclE) FuncId mm
             , MapsTo FuncId FuncDefInfo mm
             , MapsTo ProcId ProcDef mm
             , MapsTo Text (Loc ChanDeclE) mm
             , MapsTo (Loc ChanDeclE) ChanId mm
             , MapsTo (Loc VarDeclE) VarId mm
             , MapsTo (Loc VarDeclE) SortId mm
             , In (Loc FuncDeclE, Signature) (Contents mm) ~ 'False
             , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False
             , In (ProcId, ()) (Contents mm) ~ 'False )
          => FuncTable VarId -> mm -> ParsedDefs -> CompilerM TxsDefs
toTxsDefs ft mm pd = do
    ads <- adtsToTxsDefs mm (pd ^. adts)
    -- Get the function id's of all the constants.
    cfIds <- traverse (mm .@@) (pd ^.. consts . traverse . loc')
    let
        fdiMap :: Map FuncId FuncDefInfo
        fdiMap = innerMap mm
        fdefMap :: Map FuncId (FuncDef VarId)
        fdefMap = funcDef <$> fdiMap
        -- We have to remove the constants to comply with what TorXakis generates :/
        funcDefsNoConsts = Map.withoutKeys fdefMap (Set.fromList cfIds)
        -- We have to simplify to comply with what TorXakis generates.
        fn = idefsNames mm ++ fmap name cfIds
        fds = TxsDefs.empty {
            funcDefs = simplify ft fn funcDefsNoConsts
            }
        pds = TxsDefs.empty {
            procDefs = simplify ft fn (innerMap mm)
            }

    mDefMap <- modelDeclsToTxsDefs mm (pd ^. models)
    let mds = TxsDefs.empty { modelDefs = simplify ft fn mDefMap }
    uDefMap <- purpDeclsToTxsDefs mm (pd ^. purps)
    let uds = TxsDefs.empty { purpDefs = simplify ft fn uDefMap }
    cDefMap <- cnectDeclsToTxsDefs mm (pd ^. cnects)
    let cds = TxsDefs.empty { cnectDefs = simplify ft fn cDefMap }
    rDefMap <- mapperDeclsToTxsDefs mm (pd ^. mappers)
    let rds = TxsDefs.empty { mapperDefs = simplify ft fn rDefMap }
    return $ ads
        `union` fds
        `union` pds
        `union` fromList stdTDefs
        `union` mds
        `union` uds
        `union` cds
        `union` rds

-- | Compile the parsed definitions to 'Sigs'.
toSigs :: ( MapsTo Text        SortId mm
          , MapsTo (Loc CstrE) CstrId mm
          , MapsTo (Loc FuncDeclE) FuncId mm
          , MapsTo FuncId FuncDefInfo mm
          , MapsTo ProcId ProcDef mm
          , MapsTo (Loc ChanDeclE) ChanId mm)
       => mm -> ParsedDefs -> CompilerM (Sigs VarId)
toSigs mm pd = do
    let ts   = sortsToSigs (innerMap mm)
    as  <- adtDeclsToSigs mm (pd ^. adts)
    fs  <- funDeclsToSigs mm (pd ^. funcs)
    cs  <- funDeclsToSigs mm (pd ^. consts)
    let pidMap :: Map ProcId ProcDef
        pidMap = innerMap mm
        ss = Sigs.empty { func = stdFuncTable
                        , chan = values @(Loc ChanDeclE) mm
                        , pro  = Map.keys pidMap
                        }
    return $ ts `uniqueCombine` as
        `uniqueCombine` fs
        `uniqueCombine` cs
        `uniqueCombine` ss

-- | Get a dictionary from sort names to their 'SortId'. The sorts returned
-- include all the sorts defined by a 'TYPEDEF' (in the parsed definitions),
-- and the predefined sorts ('Bool', 'Int', 'Regex', 'String').
compileToSortIds :: ParsedDefs -> CompilerM (Map Text SortId)
compileToSortIds pd = do
    -- Construct the 'SortId''s lookup table.
    sMap <- compileToSortId (pd ^. adts)
    let pdsMap = Map.fromList [ ("Bool", sortIdBool)
                              , ("Int", sortIdInt)
                              , ("Regex", sortIdRegex)
                              , ("String", sortIdString)
                              ]
    return $ pdsMap <> sMap

-- | Get all the functions in the parsed definitions.
allFuncs :: ParsedDefs -> [FuncDecl]
allFuncs pd = pd ^. funcs ++ pd ^. consts

-- | Get a dictionary from the function names to the locations in which these
-- functions are defined.
--
compileToFuncLocs :: Map (Loc FuncDeclE) FuncId -> Map Text [Loc FuncDeclE]
compileToFuncLocs fIds = Map.fromListWith (++) $
    fmap mkPair (Map.toList fIds)
    where
      mkPair :: (Loc FuncDeclE, FuncId) -> (Text, [Loc FuncDeclE])
      mkPair (fdi, fId) = (name fId, [fdi])

-- | Get a dictionary from variable references to the possible location in
-- which these variables are declared. Due to overloading a syntactic reference
-- to a variable can refer to a variable, or multiple functions.
compileToDecls :: Map Text [Loc FuncDeclE]
               -> ParsedDefs
               -> CompilerM (Map (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]))
compileToDecls lfDefs pd = do
    let eVdMap = Map.empty :: Map Text (Loc VarDeclE)
    fRtoDs <- Map.fromList <$> mapRefToDecls (eVdMap :& lfDefs) (allFuncs pd)
    pRtoDs <- Map.fromList <$> mapRefToDecls (eVdMap :& lfDefs) (pd ^. procs)
    sRtoDs <- Map.fromList <$> mapRefToDecls (eVdMap :& lfDefs) (pd ^. stauts)
    mRtoDs <- Map.fromList <$> mapRefToDecls (eVdMap :& lfDefs) (pd ^. models)
    uRtoDs <- Map.fromList <$> mapRefToDecls (eVdMap :& lfDefs) (pd ^. purps)
    cRtoDs <- Map.fromList <$> mapRefToDecls (eVdMap :& lfDefs) (pd ^. cnects)
    rRtoDs <- Map.fromList <$> mapRefToDecls (eVdMap :& lfDefs) (pd ^. mappers)
    return $ fRtoDs <> pRtoDs <> sRtoDs <> mRtoDs
            <> uRtoDs <> cRtoDs <> rRtoDs

-- | Generate the map from process id's definitions to process definitions.
compileToProcDefs :: ( MapsTo Text SortId mm
                     , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm
                     , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                     , In (Loc FuncDeclE, Signature) (Contents mm) ~ 'False
                     , In (Loc ChanDeclE, ChanId) (Contents mm) ~ 'False
                     , In (Loc VarDeclE, VarId) (Contents mm) ~ 'False
                     , In (Loc ProcDeclE, ProcInfo) (Contents mm) ~ 'False
                     , In (Loc ChanRefE, Loc ChanDeclE) (Contents mm) ~ 'False
                     , In (ProcId, ()) (Contents mm) ~ 'False
                     , In (Loc VarDeclE, SortId) (Contents mm) ~ 'False)
                  => mm -> ParsedDefs -> CompilerM (Map ProcId ProcDef)
compileToProcDefs mm pd = do
    pmsP <- getMap mm (pd ^. procs)  :: CompilerM (Map (Loc ProcDeclE) ProcInfo)
    pmsS <- getMap mm (pd ^. stauts) :: CompilerM (Map (Loc ProcDeclE) ProcInfo)
    let pms = pmsP <> pmsS
    checkUnique (NoErrorLoc, Process, "Process") (getPId <$> Map.elems pms) 
    procPDefMap  <- procDeclsToProcDefMap (pms :& mm) (pd ^. procs)
    stautPDefMap <- stautDeclsToProcDefMap (pms :& mm) (pd ^. stauts)
    return $ procPDefMap <> stautPDefMap

--------------------------------------------------------------------------------
-- External parsing functions
--------------------------------------------------------------------------------

-- | Maps required for the sub-compilation functions.
data SubCompileMaps = SubCompileMaps
    { text2sidM      :: Map Text SortId
    , lvd2sidM       :: Map (Loc VarDeclE) SortId
    , lvd2vidM       :: Map (Loc VarDeclE) VarId
    , text2lfdM      :: Map Text [Loc FuncDeclE]
    , lfd2sgM        :: Map (Loc FuncDeclE) Signature
    , lfd2sghdM      :: Map (Loc FuncDeclE) (Signature, Handler VarId)
    , lvr2lvdOrlfdM  :: Map (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE])
    , lvr2vidOrsghdM :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
    , pidsM          :: Map ProcId ()
    , lchr2lchdM     :: Map (Loc ChanRefE) (Loc ChanDeclE)
    , lchd2chidM     :: Map (Loc ChanDeclE) ChanId
    }

-- | Context used in type inference
type TypeInferenceEnv = Map Text SortId
                      :& Map (Loc VarDeclE) SortId
                      :& Map (Loc FuncDeclE) Signature
                      :& Map (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE])
                      :& Map (Loc ChanRefE) (Loc ChanDeclE)
                      :& Map (Loc ChanDeclE) ChanId
                      :& Map ProcId ()

-- | Compile a subset of TorXakis, using the given external definitions.
subCompile :: ( DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) e (Map Text (Loc ChanDeclE))
              , HasVarReferences e
              , DeclaresVariables e
              , HasTypedVars TypeInferenceEnv e
              , Data e )
           => Sigs VarId
           -> [ChanId]
           -> [VarId]
           -> Int
           -> String
           -> TxsParser e
           -> (SubCompileMaps -> e -> CompilerM a)
           -> CompilerM (Id, a)
subCompile sigs chids vids unid str expP cmpF = do
    edecl <- liftEither $ parse 0 "" str expP
    setUnid unid

    let
        lchd2chid :: Map (Loc ChanDeclE) ChanId
        lchd2chid = Map.fromList $ zip (chid2loc <$> chids) chids

        text2chid :: Map Text (Loc ChanDeclE)
        text2chid = Map.fromList $ zip (ChanId.name <$> chids) (chid2loc <$> chids)

        pids :: Map ProcId ()
        pids = Map.fromList $ zip (pro sigs) (repeat ())

        lvd2vid :: [(Loc VarDeclE, VarId)]
        lvd2vid = zip (vid2loc <$> vids) vids

        text2lvd :: Map Text (Loc VarDeclE)
        text2lvd = Map.fromList $
                     zip (VarId.name . snd <$> lvd2vid) (fst <$> lvd2vid)

        text2sghd :: [(Text, (Signature, Handler VarId))]
        text2sghd = do
            (t, shmap) <- Map.toList . toMap . func $ sigs
            (s, h) <- Map.toList shmap
            return (t, (s, h))

    lfd2sghd <- forM text2sghd $ \(t, (s, h)) -> do
        i <- getNextId
        return (PredefLoc t i, (s, h))

    let
        text2lfd :: Map Text [Loc FuncDeclE]
        text2lfd = Map.fromListWith (++) $
            zip (fst <$> text2sghd) (pure . fst <$> lfd2sghd)
        text2sid :: Map Text SortId
        text2sid = sort sigs
        lvd2sid :: Map (Loc VarDeclE) SortId
        lvd2sid = Map.fromList . fmap (second varsort) $ lvd2vid
        lfd2sg :: Map (Loc FuncDeclE) Signature
        lfd2sg = Map.fromList $ fmap (second fst) lfd2sghd

    lchr2lchd <- getMap text2chid edecl  :: CompilerM (Map (Loc ChanRefE) (Loc ChanDeclE))
    lvr2lvdOrlfd <- Map.fromList <$> mapRefToDecls (text2lvd :& text2lfd) edecl
    let mm =  text2sid :& lvd2sid :& lfd2sg :& lvr2lvdOrlfd
           :& lchr2lchd :& lchd2chid :& pids
    lvd2sid' <- Map.fromList <$> inferVarTypes mm edecl
    lvd2vid'  <- Map.fromList <$> mkVarIds (lvd2sid' <.+> lvd2sid) edecl

    let mm' =  lvr2lvdOrlfd
            :& lvd2vid' <> Map.fromList lvd2vid
            :& Map.fromList lfd2sghd

    lvr2vidOrsghd <- liftEither $ varDefsFromExp mm' edecl

    let cmpMaps = SubCompileMaps
            { text2sidM = text2sid
            , lvd2sidM = lvd2sid' <.+> lvd2sid
            , lvd2vidM = lvd2vid' <> Map.fromList lvd2vid
            , text2lfdM = text2lfd
            , lfd2sgM = lfd2sg
            , lfd2sghdM = Map.fromList lfd2sghd
            , lvr2lvdOrlfdM = lvr2lvdOrlfd
            , lvr2vidOrsghdM = lvr2vidOrsghd
            , pidsM = pids
            , lchr2lchdM = lchr2lchd
            , lchd2chidM = lchd2chid
            }

    e <- cmpF cmpMaps edecl

    unid' <- getUnid
    return (Id unid', e)

-- | Create a location of a variable declaration from a variable id. This is
-- needed in the sub-compilers, which take existing variable id's.
vid2loc :: VarId -> Loc VarDeclE
vid2loc vId = PredefLoc (VarId.name vId) (_id . VarId.unid $ vId)

-- | Create a location of a channel declaration from a channel id. This is
-- needed in the sub-compilers, which take existing channel id's.
chid2loc :: ChanId -> Loc ChanDeclE
chid2loc chId = PredefLoc (ChanId.name chId) (_id . ChanId.unid $ chId)

-- | Sub-compiler for value definitions
compileValDefs :: Sigs VarId
               -> [VarId]
               -> Int        -- ^ Next unique identifier.
               -> String     -- ^ String to parse.
               -> CompilerM (Id, Map VarId (ValExpr VarId))
compileValDefs sigs vids unid str =
    subCompile sigs [] vids unid str letVarDeclsP $ \scm ls ->
        liftEither $ parValDeclToMap (lvr2vidOrsghdM scm) ls

-- | Sub-compiler for variable declarations.
compileVarDecls :: Sigs VarId
                -> Int        -- ^ Next unique identifier.
                -> String     -- ^ String to parse.
                -> CompilerM (Id, [VarId])
compileVarDecls sigs unid str =
    subCompile sigs [] [] unid str varDeclsP $ \scm vdecls ->
        return $ Map.elems $
            Map.restrictKeys (lvd2vidM scm) (Set.fromList $ getLoc <$> vdecls)

-- | Sub-compiler for value expressions.
compileValExpr :: Sigs VarId
               -> [VarId]
               -> Int        -- ^ Next unique identifier.
               -> String     -- ^ String to parse.
               -> CompilerM (Id, ValExpr VarId)
compileValExpr sigs vids unid str =
    subCompile sigs [] vids unid str valExpP $ \scm edecl -> do

        let mm =  text2sidM scm
               :& lvd2sidM scm
               :& lfd2sgM scm
               :& lvr2lvdOrlfdM scm
        eSid  <- liftEither $ inferExpTypes mm edecl >>= getUniqueElement
        liftEither $ expDeclToValExpr (lvr2vidOrsghdM scm) eSid edecl

-- | Sub-compiler for behavior expressions.
compileBExpr :: Sigs VarId
            -> [ChanId]
            -> [VarId]
            -> Int        -- ^ Next unique identifier.
            -> String     -- ^ String to parse.
            -> CompilerM (Id, BExpr)
compileBExpr sigs chids vids unid str =
    subCompile sigs chids vids unid str bexpDeclP $ \scm bdecl -> do
        let mm =  text2sidM scm
               :& lvr2lvdOrlfdM scm
               :& lvd2sidM scm
               :& lfd2sghdM scm
               :& lchr2lchdM scm
               :& lchd2chidM scm
               :& pidsM scm
        toBExpr mm (lvr2vidOrsghdM scm) bdecl

-- | Sub-compiler for action prefix offers.
compileOffer :: Sigs VarId
             -> [ChanId]
             -> [VarId]
             -> Int        -- ^ Next unique identifier.
             -> String     -- ^ String to parse.
             -> CompilerM (Id, Set Offer)
compileOffer sigs chids vids unid str =
    subCompile sigs chids vids unid str offersP $ \scm ofsDecl -> do
        let mm = text2sidM scm
                :& lvr2lvdOrlfdM scm
                :& lvd2sidM scm
                :& lfd2sghdM scm
                :& lchr2lchdM scm
                :& lchd2chidM scm

        os <- traverse (toOffer mm (lvr2vidOrsghdM scm)) ofsDecl
        -- Filter the internal actions (to comply with the current TorXakis compiler).
        return $ Set.fromList $ filter ((chanIdIstep /=) . chanid) os
