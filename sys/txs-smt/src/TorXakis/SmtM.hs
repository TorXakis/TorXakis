{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TorXakis.Assertions
-- Copyright   :  (c) 2015-2017 TNO and Radboud University
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Pierre van de Laar <pierre.vandelaar@tno.nl>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides the SMT ProblemSolver instance.
-- The user should make an initial SmtState with `mkSmtState`.
-- Use the `SmtM` monad to solve the problems incrementally.
-- And when done, destroy the SmtState with `destroySmtState`.
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TorXakis.SmtM
( SmtM (..)
, SmtState
, mkSmtState
, destroySmtState
)
where
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.State
import           Control.Monad.Except
import           Data.Bimap
import           Data.HashMap
import           Data.List
import qualified Data.Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set
import           Data.String.Utils
import           Data.Time
import           Numeric
import           System.Exit
import           System.IO
import           System.Process

import           TorXakis.ContextFunc
import           TorXakis.ContextValExpr
import           TorXakis.Error
import           TorXakis.FuncDef
import           TorXakis.FuncSignature
import           TorXakis.Name
import           TorXakis.NameMap
import           TorXakis.ProblemSolver
import           TorXakis.RefByIndex
import           TorXakis.SMTAlex
import           TorXakis.SMTHappy
import           TorXakis.SmtLanguage
import           TorXakis.Sort
import           TorXakis.ValExpr
import           TorXakis.Value
import           TorXakis.Var

type ADTMap  = Data.HashMap.Map (RefByName ADTDef) SmtString
type FuncMap = Data.HashMap.Map RefByFuncSignature SmtString
type VarMap  = Data.Bimap.Bimap (RefByName VarDef) SmtString
type CstrMap = Data.Bimap.Bimap (RefByName ADTDef, RefByName ConstructorDef) SmtString

-- | Smt State
data SmtState = SmtState { inHandle       :: Handle
                         , outHandle      :: Handle
                         , processHandle  :: ProcessHandle
                         , logFileMHandle :: Maybe Handle
                         , depth          :: Integer
                         -- defined definitions
                         , funcContext  :: ContextFunc
                         , varDefsStack :: [NameMap VarDef]
                         -- translation to SMT (and from SMT when necessary)
                         , adtId     :: Integer
                         , adtToSmt  :: ADTMap -- `a + show adtId`
                         , funcId    :: Integer
                         , funcToSmt :: FuncMap -- `f + show funcId`
                         , varId     :: Integer
                         , varToSmt  :: VarMap -- `v + show varId`
                         , cstrToSmt :: CstrMap  -- `c + show adtId + $ + show (local cstrId)`
                         }

-- | mkSmtState - make Smt State
--
-- * open Smt Solver
--
-- * handle input, output and errors from Smt Solver
--
-- * when wanted, log Smt commands
--
-- * Bring Smt Solver to desired initial state
mkSmtState :: FilePath -> [String] -> Bool -> IO (Either Error SmtState)
mkSmtState fp as lgFlag = do
        -- start smt solver process
        (Just hin, Just hout, Just herr, ph) <-  createProcess (proc fp as) { std_in  = CreatePipe
                                                                            , std_out = CreatePipe
                                                                            , std_err = CreatePipe
                                                                            }
        hSetBuffering hin  NoBuffering
        hSetBuffering hout NoBuffering
        hSetBuffering herr NoBuffering

        hSetEncoding hin  latin1
        hSetEncoding hout latin1
        hSetEncoding herr latin1

        -- open SMT logging file
        mlg <- if lgFlag
                then do timeZone <- getCurrentTimeZone
                        startTime <- getCurrentTime
                        let timeString = replace ":" "-" $
                                         replace " " "-" $
                                         show (utcToLocalTime timeZone startTime) in do
                            h <-  openFile ("logSMT." ++ timeString ++ ".smt2") WriteMode
                            hSetBuffering h NoBuffering
                            hSetEncoding h latin1
                            return $ Just h
                else return Nothing

        -- handle warning messages of Smt Solver (over herr)
        -- note: errors in SMT are reported over hout with the response "(error <string>)"
        _ <-  forkIO (showErrors herr "SMT WARN >> ")

        -- initialize the smt solver
        -- To reuse the smtPut function (to ensure e.g. consistent logging) we have to run it in the `SmtM` monad.
        -- Note: putting the `mkSmtState` completely in the `SmtM` monad,
        --       would require a more complicate SmtState (that reflects whether the Smt Solver is started)
        --       and checking on the state in all `SmtM` Monadic functions.
        es <- runExceptT $ execStateT ( toStateT (do
                                                    smtPut $ fromString "(set-info :smt-lib-version 2.5)"
                                                    smtPut $ fromString "(set-option :produce-models true)"
                                                    smtPut $ fromString "(set-logic ALL)"
                                                )
                                      )
                                      ( SmtState hin
                                                 hout
                                                 ph
                                                 mlg
                                                 0
                                                 TorXakis.ContextFunc.empty
                                                 []
                                                 0
                                                 Data.HashMap.empty
                                                 0
                                                 Data.HashMap.empty
                                                 0
                                                 Data.Bimap.empty
                                                 Data.Bimap.empty
                                      )
        case es of
            Left err -> return $ Left err
            Right s  -> return $ Right s

-- | destroySmtState
--
-- * exit Smt Solver, and check its exit code
--
-- * close log file
destroySmtState :: SmtState -> IO (Maybe Error)
destroySmtState s = do
    -- destroy / exit the smt solver
    -- To reuse the smtPut function (to ensure e.g. consistent logging) we have to run it in the `SmtM` monad.
    -- Note: putting the `destroySmtState` completely in the `SmtM` monad,
    --       would require a more complicate SmtState (that reflects whether the Smt Solver is started)
    --       and checking on the state in all `SmtM` Monadic functions.
    response <- runExceptT $ runStateT ( toStateT (smtPut smtExit) ) s
    mapM_ hClose (logFileMHandle s)
    case response of
            Left err -> return $ Just err
            Right _  -> do
                            ec <- waitForProcess (processHandle s)
                            case ec of
                                ExitSuccess   -> return Nothing
                                ExitFailure c -> return $ Just (Error ("Smt Solver exited with error code " ++ show c))


-- | Smt Monad
newtype SmtM a = SmtM { -- | to `StateT`
                        toStateT :: StateT SmtState (ExceptT Error IO) a
                      }
                      deriving (Functor, Applicative, Monad, MonadState SmtState, MonadIO, MonadError Error)

-- TODO: is it worthwhile to implement a specific solve that uses get-model? See -- https://github.com/TorXakis/TorXakis/issues/932
instance ProblemSolver SmtM where
    info = do
                n <- getInfo "name"
                v <- getInfo "version"
                return $ "SMT solver using " ++ n ++ " [" ++ v ++ "]"

    addADTs as =
            do
                st <- get
                if TorXakis.SmtM.depth st > 0
                then throwError $ Error "AddADTs only at depth 0. Depth is controlled by push and pop."
                else case as of
                        [] -> return ()
                        _ -> case TorXakis.ContextFunc.addADTs as (TorXakis.SmtM.funcContext st) of
                                Left e -> throwError $ Error ("Smt ProblemSolver - addADTs failed due to " ++ show e)
                                Right funcContext' ->
                                    let aId = adtId st
                                        aId' = aId + toInteger (Data.List.length as)
                                        cTs = cstrToSmt st
                                        aTs = adtToSmt st
                                        (aTs', cTs') = foldl insertADT (aTs, cTs) $ zip as [aId ..]
                                        in do
                                            put $ st{ TorXakis.SmtM.funcContext = funcContext'
                                                    , TorXakis.SmtM.adtId       = aId'
                                                    , TorXakis.SmtM.adtToSmt    = aTs'
                                                    , TorXakis.SmtM.cstrToSmt   = cTs'
                                                    }
                                            ddts <- mapM (\a -> let ar :: RefByName ADTDef
                                                                    ar = RefByName (adtName a) in do
                                                                    sa <- adtRefToSmt ar
                                                                    cs <- mapM (\c -> let cr :: RefByName ConstructorDef
                                                                                          cr = RefByName (constructorName c) in do
                                                                                        sc <- cstrRefToSmt ar cr
                                                                                        fs <- mapM (\(s,p) -> let fp :: RefByIndex FieldDef
                                                                                                                  fp = RefByIndex p in do
                                                                                                                sf <- fieldRefToSmt ar cr fp
                                                                                                                ss <- sortToSmt s
                                                                                                                return $ smtDeclareField sf ss
                                                                                                   )
                                                                                                   (zip (Data.List.map TorXakis.Sort.sort (elemsField c)) [0..])
                                                                                        return $ smtDeclareConstructor sc fs
                                                                               ) (elemsConstructor a)
                                                                    return $ smtDeclareDatatype sa cs
                                                         ) as
                                            smtPut $ smtDeclareDatatypes ddts
        where
            insertADT :: (ADTMap, CstrMap) -> (ADTDef, Integer) -> (ADTMap, CstrMap)
            insertADT (am, cm) (a, i) =
                    ( Data.HashMap.insert aRef toSmtAdtText am
                    , foldl (flip (uncurry Data.Bimap.insert)) cm $ zip (fmap toRefTuple (elemsConstructor a)) (fmap toSmtCstrText [0..])
                    )
                where
                    aRef :: RefByName ADTDef
                    aRef = RefByName (adtName a)

                    iText :: SmtString
                    iText = fromString (showHex i "")

                    toSmtAdtText :: SmtString
                    toSmtAdtText = append (TorXakis.SmtLanguage.singleton 'a') iText

                    toRefTuple :: ConstructorDef -> (RefByName ADTDef, RefByName ConstructorDef)
                    toRefTuple cd = (aRef, RefByName (constructorName cd))

                    toSmtCstrText :: Integer -> SmtString
                    toSmtCstrText c = TorXakis.SmtLanguage.concat
                                            [ TorXakis.SmtLanguage.singleton 'c'
                                            , iText
                                            , TorXakis.SmtLanguage.singleton '$'
                                            , fromString (showHex c "")
                                            ]


    addFunctions fs = do
            st <- get
            if TorXakis.SmtM.depth st > 0
                then throwError $ Error "AddFunctions only at depth 0. Depth is controlled by push and pop."
                else case fs of
                        [] -> return ()
                        _  -> case TorXakis.ContextFunc.addFuncs fs (TorXakis.SmtM.funcContext st) of
                                Left e -> throwError $ Error ("Smt ProblemSolver - addFunctions failed due to " ++ show e)
                                Right funcContext' ->
                                    let fI = funcId st
                                        fI' = fI + toInteger (Data.List.length fs)
                                        fTs = funcToSmt st
                                        fTs' = foldl (insertFunction funcContext') fTs $ zip fs [fI ..]
                                      in do
                                            put $ st{ TorXakis.SmtM.funcContext = funcContext'
                                                    , TorXakis.SmtM.funcId      = fI'
                                                    , TorXakis.SmtM.funcToSmt   = fTs'
                                                    }
                                            smtFuncs <- foldM addFunctionHeaderBodyTuple [] fs
                                            smtPut $ smtDeclareFunctions smtFuncs
        where
            insertFunction :: SortContext c => c -> FuncMap -> (FuncDef, Integer) -> FuncMap
            insertFunction ctx m (f,i) =
                Data.HashMap.insert (RefByFuncSignature (getFuncSignature ctx f)) (toSmtFuncText i) m

            toSmtFuncText :: Integer -> SmtString
            toSmtFuncText i = TorXakis.SmtLanguage.append (TorXakis.SmtLanguage.singleton 'f')
                                                          (fromString (showHex i ""))

            addFunctionHeaderBodyTuple :: [(SmtString, SmtString)] -> FuncDef -> SmtM [(SmtString, SmtString)]
            addFunctionHeaderBodyTuple ls f = do
                st <- get
                assert (TorXakis.NameMap.null (TorXakis.NameMap.unions (TorXakis.SmtM.varDefsStack st))) $
                    let ps = TorXakis.Var.toList (paramDefs f) in do
                        _ <- push
                        declareVariables ps
                        ctx <- toValExprContext
                        let fr = getFuncSignature ctx f in do
                            sfr <- funcRefToSmt (RefByFuncSignature fr)
                            sps <- mapM toParameterSmt ps
                            sr <- sortToSmt (returnSort fr)
                            b <- valExprToSmt (body f)
                            _ <- pop
                            let h = TorXakis.SmtLanguage.concat [ TorXakis.SmtLanguage.singleton '('
                                                                , sfr
                                                                , TorXakis.SmtLanguage.singleton '('
                                                                , TorXakis.SmtLanguage.concat sps
                                                                , TorXakis.SmtLanguage.singleton ')'
                                                                , sr
                                                                , TorXakis.SmtLanguage.singleton ')'
                                                                ] in
                                return ( (h,b) : ls )

    push = do
            smtPut smtPush
            -- update depth (not returned by smt solver) and add new variables on stack
            st <- get
            let newStack = (TorXakis.NameMap.empty : TorXakis.SmtM.varDefsStack st)
                newDepth = succ (TorXakis.SmtM.depth st) in do
                    put $ st { TorXakis.SmtM.depth = newDepth
                             , TorXakis.SmtM.varDefsStack = newStack
                             }
                    return newDepth

    pop = do
            st <- get
            let d = TorXakis.SmtM.depth st in
                if d > 0
                then case TorXakis.SmtM.varDefsStack st of
                        []     -> error "Pop - Stack unexpectedly is empty"
                        (_:tl) -> let newDepth = pred d in do
                                    smtPut smtPop
                                    put $ st{ TorXakis.SmtM.depth = newDepth
                                            , TorXakis.SmtM.varDefsStack = tl
                                            }
                                    return newDepth
                else throwError $ Error ("Pop - Depth is not larger than 0, but " ++ show d ++ ". Did you push before?")

    depth = gets TorXakis.SmtM.depth

    declareVariables vs = do
            st <- get
            let d = TorXakis.SmtM.depth st
                stack = TorXakis.SmtM.varDefsStack st
                varDefs = TorXakis.NameMap.unions stack in
                if d > 0
                then case repeatedByNameIncremental (TorXakis.NameMap.elems varDefs) vs of
                            [] -> let vI = TorXakis.SmtM.varId st
                                      vTs = TorXakis.SmtM.varToSmt st
                                      vI' = vI + toInteger (Data.List.length vs)
                                      vTs' = foldl insertVar vTs $ zip vs [vI ..] in
                                      case stack of
                                          []     -> error "declareVariables - Stack unexpectedly is empty"
                                          (hd:tl) -> do
                                                        put st { TorXakis.SmtM.varDefsStack = TorXakis.NameMap.union (toNameMap vs) hd : tl
                                                               , TorXakis.SmtM.varId = vI'
                                                               , TorXakis.SmtM.varToSmt = vTs'
                                                               }
                                                        mapM_ toSmtVarDecl vs
                            ds -> throwError $ Error ("declareVariables - declaration impossible due to non unique variables: "++ show ds)
                else throwError $ Error ("declareVariables - Precondition depth is larger than 0 is not satisfied since depth is " ++ show d ++ ". Did you push before?")
        where
            insertVar :: VarMap -> (VarDef, Integer) -> VarMap
            insertVar m (v,i) = Data.Bimap.insert (RefByName (name v)) (append (TorXakis.SmtLanguage.singleton 'v') iText) m
                where
                    iText :: SmtString
                    iText = fromString (showHex i "")

            toSmtVarDecl :: VarDef -> SmtM ()
            toSmtVarDecl v = do
                ref <- varRefToSmt (RefByName (name v))
                srt <- sortToSmt (TorXakis.Var.sort v)
                smtPut $ smtDeclareVariable ref srt

    addAssertions = mapM_ ( valExprToSmt >=> (smtPut . smtAssert) )

    solvable = do
                smtPut smtCheckSat
                s <- smtGet
                case s of
                    "sat"        -> return $ SolvableProblem (Just True)
                    "unsat"      -> return $ SolvableProblem (Just False)
                    "unknown"    -> return $ SolvableProblem Nothing
                    _            -> error ("solvable - Unexpected result by smt check-sat '"++ s ++ "'")

    solvePartSolution vs = do
                SolvableProblem r <- solvable
                case r of
                    Nothing     -> return UnableToSolve
                    Just False  -> return Unsolvable
                    Just True   -> Solved <$> getValues
        where
            getValues :: SmtM Solution
            getValues = case vs of
                            [] -> return $ Solution Data.HashMap.empty
                            _  -> do
                                        varRefsSmt <- mapM varRefToSmt vs
                                        smtPut $ smtGetValues varRefsSmt
                                        sv <- smtGet
                                        let mp = Data.Map.mapKeys TorXakis.SmtLanguage.fromString . smtParser . smtLexer $ sv in
                                            assert (Data.Set.fromList varRefsSmt == Data.Set.fromList (Data.Map.keys mp)) $
                                                do
                                                    res <- mapM decode (Data.Map.toList mp)
                                                    return $ Solution (Data.HashMap.fromList res)

            decode :: (SmtString, SMTValue) -> SmtM (RefByName VarDef, Value)
            decode (sVar, sVal) = do
                    m <- gets varToSmt
                    ctx <- toValExprContext
                    let tVarRef = fromMaybe (error ("variable (" ++ show sVar ++ ") unexpectedly not in varToSmt"))
                                            (Data.Bimap.lookupR sVar m)
                        tVarDef = fromMaybe (error ("variable (" ++ show tVarRef ++ ") unexpectedly not in declared variables"))
                                            (lookupVar (toName tVarRef) ctx) in do
                            tVal <- smtValueToTxsValue sVal (TorXakis.Var.sort tVarDef)
                            return (tVarRef, tVal)

    toValExprContext = do
            st <- get
            let fc = funcContext st
                stack = TorXakis.SmtM.varDefsStack st
                varDefs = TorXakis.NameMap.unions stack in
                case addVars (TorXakis.NameMap.elems varDefs) (fromFuncContext fc) of
                     Left e    -> error ("toValExprContext - Addition of variables to function context unexpectedly failed with the following error:\n" ++ show e)
                     Right fc' -> return fc'

-- | get Info of SMT Solver
getInfo :: String -> SmtM String
getInfo topic = do
    smtPut $ smtGetInfo topic
    s <- smtGet
    let list = strip s in
        if startswith "(" list && endswith ")" list
        then let tuple = strip (init . tail $ list) in
                case stripPrefix (":" ++ topic) tuple of
                   Just res -> let str = strip res in
                               if startswith "\"" str && endswith "\"" str
                               then return $ init . tail $ str
                               else error ("SMT response violates quotes in pattern.\nExpected (:" ++ topic ++ " \"<name>\")\nActual "++ list)
                   Nothing -> error ("SMT response violates topic in pattern.\nExpected (:" ++ topic ++ " \"<name>\")\nActual "++ list)
        else error ("SMT response violates brackets in pattern.\nExpected (:" ++ topic ++ " \"<name>\")\nActual "++ list)

-- | execute the SMT commands given as a string
smtPut :: SmtString -> SmtM ()
smtPut cmds  = do
        st <- get
        liftIO $ maybeLog (logFileMHandle st)
        liftIO $ hPutStrLn (inHandle st) stringRep
    where
        stringRep :: String
        stringRep = TorXakis.SmtLanguage.toString cmds

        -- | write string to Log-file when available
        maybeLog :: Maybe Handle -> IO ()
        maybeLog Nothing  = return ()
        maybeLog (Just h) = liftIO $ hPutStrLn h stringRep

-- | show errors
--   The given prefix is prepended to every error line.
showErrors :: Handle -- ^ handle
            -> String -- ^ prefix
            -> IO ()
showErrors h prefix = do
    s <- hIsEOF h
    unless s $ do
                    msg <- hGetLine h
                    hPutStrLn stderr (prefix ++ msg)
                    showErrors h prefix

smtGet :: SmtM String
smtGet = do
            st <- get
            let hout = outHandle st
                ph = processHandle st
             in
                liftIO $ getResponse hout `onException` exitWithError ph
    where
        exitWithError :: ProcessHandle -> IO ()
        exitWithError procHandle = do
            ec <- getProcessExitCode procHandle
            -- The output and error handles of the SMT process are closed when this
            -- error occurs (maybe because they're pipes?) so no information can be
            -- given to the user, other than this.
            error ("smtGet: Could not get a response from the SMT solver\n" ++
                   "smtGet: SMT solver exited with status: " ++ show ec)

-- | read the response (as lines) from the handle
-- this operation is blocking until some data can be read and checks that all brackets are closed.
getResponse :: Handle -> IO String
getResponse h = getResponseCount 0
    where
        getResponseCount :: Integer -> IO String
        getResponseCount count = do
            s <- hGetLine h
            let newCount = count + countBracket s in
                if 0 == newCount
                    then return s
                    else do
                            tl <- getResponseCount newCount
                            return $ s ++ tl

        -- | count the open bracket pairs in the string
        -- i.e. '(' opens a pair and ')' closes a pair.
        -- Note that brackets inside strings (") should be ignored
        countBracket :: String -> Integer
        countBracket ('"':xs) = skipCountInsideString xs          -- ignore brackets inside strings
            where
                skipCountInsideString :: String -> Integer
                skipCountInsideString ('"':'"':ccs) = skipCountInsideString ccs       -- escape quote, stay in string
                skipCountInsideString ('"':cs)      = countBracket cs                 -- outside string
                skipCountInsideString (_:cs)        = skipCountInsideString cs
                skipCountInsideString []            = 0
        countBracket ('(':xs) = 1 + countBracket xs
        countBracket (')':xs) = -1 + countBracket xs
        countBracket (_:xs)   = countBracket xs
        countBracket []       = 0

-- ------------
-- TXS 2 SMT
-- ------------
toParameterSmt :: VarDef -> SmtM SmtString
toParameterSmt p = do
    ref <- varRefToSmt (RefByName (name p))
    srt <- sortToSmt (TorXakis.Var.sort p)
    return $ TorXakis.SmtLanguage.concat [ TorXakis.SmtLanguage.singleton '('
                                         , ref
                                         , TorXakis.SmtLanguage.singleton ' '
                                         , srt
                                         , TorXakis.SmtLanguage.singleton ')'
                                         ]

-- | sort to Smt
sortToSmt :: Sort -> SmtM SmtString
sortToSmt SortBool     = return smtBoolean
sortToSmt SortInt      = return smtInteger
sortToSmt SortChar     = error "Not yet implemented"
sortToSmt SortString   = return smtString
sortToSmt SortRegex    = error "Regex is not defined in SMT"
sortToSmt (SortADT ar) = adtRefToSmt ar

-- | adt Ref To Smt
adtRefToSmt :: RefByName ADTDef-> SmtM SmtString
adtRefToSmt ar = do
                        m <- gets adtToSmt
                        return $ fromMaybe (error ("adtref (" ++ show ar ++ ") unexpectly not present in mapping."))
                                           (Data.HashMap.lookup ar m)

-- | cstr Ref To Smt
cstrRefToSmt :: RefByName ADTDef-> RefByName ConstructorDef -> SmtM SmtString
cstrRefToSmt ar cr = do
                        m <- gets cstrToSmt
                        return $ fromMaybe (error ("tuple (" ++ show ar ++ ", " ++ show cr ++ ") unexpectly not present in mapping."))
                                           (Data.Bimap.lookup (ar,cr) m)

-- | field Ref To Smt
fieldRefToSmt :: RefByName ADTDef-> RefByName ConstructorDef -> RefByIndex FieldDef -> SmtM SmtString
fieldRefToSmt ar cr fr = do
                            m <- gets cstrToSmt
                            case Data.Bimap.lookup (ar,cr) m of
                                Nothing -> error ("tuple (" ++ show ar ++ ", " ++ show cr ++ ") unexpectly not present in mapping.")
                                Just s  -> return $ TorXakis.SmtLanguage.concat
                                                            [ s
                                                            , TorXakis.SmtLanguage.singleton '$'
                                                            , fromString (showHex (toIndex fr) "")
                                                            ]

-- var Ref To Smt
varRefToSmt :: RefByName VarDef -> SmtM SmtString
varRefToSmt vr = do
                    m <- gets varToSmt
                    return $ fromMaybe (error ("varref (" ++ show vr ++ ") unexpectly not present in mapping."))
                                       (Data.Bimap.lookup vr m)

-- funcRef To Smt
funcRefToSmt :: RefByFuncSignature -> SmtM SmtString
funcRefToSmt fr = do
                    m <- gets funcToSmt
                    return $ fromMaybe (error ("funcref (" ++ show fr ++ ") unexpectly not present in mapping."))
                                       (Data.HashMap.lookup fr m)


-- | value to smt
valueToSmt :: Value -> SmtM SmtString
valueToSmt (Cbool True)     = return smtTrue
valueToSmt (Cbool False)    = return smtFalse
valueToSmt (Cint n)         = return $ smtIntegerLiteral n
valueToSmt (Cstring s)      = return $ smtTextLiteral s
valueToSmt (Cregex r)       = return $ smtRegexLiteral r
valueToSmt (Ccstr ar cr []) = cstrRefToSmt ar cr
valueToSmt (Ccstr ar cr as) = do
                                 sc <- cstrRefToSmt ar cr
                                 sas <- mapM valueToSmt as
                                 return $ TorXakis.SmtLanguage.concat [ TorXakis.SmtLanguage.singleton '('
                                                                      , sc
                                                                      , TorXakis.SmtLanguage.singleton ' '
                                                                      , TorXakis.SmtLanguage.intercalate (TorXakis.SmtLanguage.singleton ' ') sas
                                                                      , TorXakis.SmtLanguage.singleton ')'
                                                                      ]
valueToSmt x                = error ("Illegal input valueToSmt - " ++ show x)

-- | valExpr to Smt
valExprToSmt :: ValExpression -> SmtM SmtString
valExprToSmt = valExprViewToSmt . view

-- | valExprView to Smt
valExprViewToSmt :: ValExpressionView -> SmtM SmtString
valExprViewToSmt (Vconst c)        = valueToSmt c
valExprViewToSmt (Vvar v)          = varRefToSmt v
valExprViewToSmt (Vequal v1 v2)    = operatorToSmt (TorXakis.SmtLanguage.singleton '=') <$> mapM valExprToSmt [v1,v2]
valExprViewToSmt (Vite c t f)      = operatorToSmt (fromString "ite") <$> mapM valExprToSmt [c,t,f]
valExprViewToSmt (Vfunc fr [])     = funcRefToSmt fr
valExprViewToSmt (Vfunc fr ps)     = do
                                        sf <- funcRefToSmt fr
                                        sps <- mapM valExprToSmt ps
                                        return $ operatorToSmt sf sps
valExprViewToSmt (Vpredef fr _)    = error ("predefined function should not be passed to SMT solver: " ++ show fr)
valExprViewToSmt (Vnot e)          = unaryOperatorToSmt (fromString "not") <$> valExprToSmt e
valExprViewToSmt (Vand es)         = operatorToSmt (fromString "and") <$> mapM valExprToSmt (Data.Set.toList es)
valExprViewToSmt (Vdivide v1 v2)   = operatorToSmt (fromString "div") <$> mapM valExprToSmt [v1,v2]
valExprViewToSmt (Vmodulo v1 v2)   = operatorToSmt (fromString "mod") <$> mapM valExprToSmt [v1,v2]
valExprViewToSmt (Vsum s)          = let ol = Data.Map.toList s in
                                        case ol of
                                            [o] -> arg2smt o
                                            _   -> operatorToSmt (TorXakis.SmtLanguage.singleton '+') <$> mapM arg2smt ol
                                     where
                                        arg2smt :: (ValExpression, Integer) -> SmtM SmtString
                                        arg2smt (vexpr, 1)                            = valExprToSmt vexpr
                                        arg2smt (vexpr, -1)                           = unaryOperatorToSmt (TorXakis.SmtLanguage.singleton '-') <$> valExprToSmt vexpr
                                        arg2smt (vexpr, multiplier) | multiplier /= 0 = binaryOperatorToSmt (TorXakis.SmtLanguage.singleton '*') (smtIntegerLiteral multiplier) <$> valExprToSmt vexpr
                                        arg2smt (_    , multiplier)                   = error ("valExprToSmt - illegal multiplier " ++ show multiplier)
valExprViewToSmt (Vproduct p)          = let ol = Data.Map.toList p in
                                        case ol of
                                            [o] -> arg2smt o
                                            _   -> operatorToSmt (TorXakis.SmtLanguage.singleton '*') <$> mapM arg2smt ol
                                     where
                                        arg2smt :: (ValExpression, Integer) -> SmtM SmtString
                                        arg2smt (vexpr, 1)                 = valExprToSmt vexpr
                                        arg2smt (vexpr, power) | power > 0 = flip (binaryOperatorToSmt (TorXakis.SmtLanguage.singleton '^')) (smtIntegerLiteral power) <$> valExprToSmt vexpr
                                        arg2smt (_    , power)             = error ("valExprToSmt - illegal power " ++ show power)
valExprViewToSmt (Vgez e)          = unaryOperatorToSmt (fromString "<= 0") <$> valExprToSmt e
valExprViewToSmt (Vlength e)       = unaryOperatorToSmt (fromString "str.len") <$> valExprToSmt e
valExprViewToSmt (Vat v1 v2)       = operatorToSmt (fromString "str.at") <$> mapM valExprToSmt [v1,v2]
valExprViewToSmt (Vconcat es)      = operatorToSmt (fromString "str.++") <$> mapM valExprToSmt es
valExprViewToSmt (Vstrinre v1 v2)  = operatorToSmt (fromString "str.in.re") <$> mapM valExprToSmt [v1,v2]
valExprViewToSmt (Vcstr ar cr [])  = assert False $ cstrRefToSmt ar cr      -- A valExpr of a constructor with no parameters is expected to be rewritten to a constant value
valExprViewToSmt (Vcstr ar cr es)  = do
                                        cs <- cstrRefToSmt ar cr
                                        as <- mapM valExprToSmt es
                                        return $ operatorToSmt cs as
valExprViewToSmt (Viscstr ar cr e) = do
                                        cs <- cstrRefToSmt ar cr
                                        arg <- valExprToSmt e
                                        return $ unaryOperatorToSmt (toSmtIsCstr cs) arg
                                    where
                                        toSmtIsCstr :: SmtString -> SmtString
                                        toSmtIsCstr = TorXakis.SmtLanguage.append (fromString "is-")
valExprViewToSmt (Vaccess ar cr p e) = do
                                        f <- fieldRefToSmt ar cr p
                                        arg <- valExprToSmt e
                                        return $ unaryOperatorToSmt f arg
valExprViewToSmt (Vforall vs e) = do
                                        ss <- mapM toParameterSmt (TorXakis.Var.toList vs)
                                        arg <- valExprToSmt e
                                        return $ TorXakis.SmtLanguage.concat
                                                        [ fromString "(forall ("
                                                        , TorXakis.SmtLanguage.concat ss
                                                        , fromString ") "
                                                        , arg
                                                        , fromString ")"
                                                        ]


-- | unary smt operator
unaryOperatorToSmt :: SmtString -> SmtString -> SmtString
unaryOperatorToSmt operator arg = operatorToSmt operator [arg]

-- | binary smt operator
binaryOperatorToSmt :: SmtString -> SmtString -> SmtString -> SmtString
binaryOperatorToSmt operator arg1 arg2 = operatorToSmt operator [arg1, arg2]

-- list smt operator
operatorToSmt :: SmtString -> [SmtString] -> SmtString
operatorToSmt operator as = TorXakis.SmtLanguage.concat [ TorXakis.SmtLanguage.singleton '('
                                                        , TorXakis.SmtLanguage.intercalate (TorXakis.SmtLanguage.singleton ' ') (operator:as)
                                                        , TorXakis.SmtLanguage.singleton ')'
                                                        ]

-- ------------
-- SMT 2 TXS
-- ------------
-- | convert an SMT value to a torxakis Value.
-- TODO: check that compiler indeed removes all calculations related to checking sort (2nd parameter is only used within asserts)
-- TODO: discuss Jan is this wanted/desired?
smtValueToTxsValue :: SMTValue -> Sort -> SmtM Value
smtValueToTxsValue (SMTBool b)                      srt          = assert (srt == SortBool)   $ return (Cbool b)
smtValueToTxsValue (SMTInt i)                       srt          = assert (srt == SortInt)    $ return (Cint i)
smtValueToTxsValue (SMTString s)                    srt          = assert (srt == SortString) $ return (Cstring s)
smtValueToTxsValue (SMTConstructor cname argValues) srt          = do
    st <- get
    let m = cstrToSmt st
        (ar, cr) = fromMaybe (error ("TXS SMT2TXS smtValueToTxsValue: constructor (" ++ show cname ++ ") unexpectedly not present in map"))
                             (Data.Bimap.lookupR (fromText cname) m) in
        assert (srt == SortADT ar) $
             let fc = funcContext st
                 ad = fromMaybe (error ("TXS SMT2TXS smtValueToTxsValue: adt (" ++ show ar ++ ") unexpectedly not present in map"))
                                (lookupADT (toName ar) fc)
                 cd = fromMaybe (error ("TXS SMT2TXS smtValueToTxsValue: constructor (" ++ show cr ++ ") unexpectedly not present in adt (" ++ show ar ++ ")"))
                                (lookupConstructor (toName cr) ad)
                 fields = elemsField cd in
                    assert (Data.List.length fields == Data.List.length argValues) $
                        Ccstr ar cr <$> zipWithM smtValueToTxsValue argValues (Data.List.map TorXakis.Sort.sort fields)
-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
