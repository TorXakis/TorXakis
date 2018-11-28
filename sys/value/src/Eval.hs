{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}
module Eval

-- ----------------------------------------------------------------------------------------- --
--
--
-- Evaluation of Value Expressions
--
-- ----------------------------------------------------------------------------------------- --
-- export

(        -- * evaluation of value expression; eval shall only work on closed vexpr
eval
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.State
import           Data.Either
import           Data.Maybe
import qualified Data.String.Utils   as Utils
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.Regex.TDFA

import qualified Data.Map            as Map
import qualified Data.Set            as Set

-- import from behavedefs
import qualified EnvBTree            as IOB
import qualified EnvData

-- import from defs
import           FreeMonoidX
import           RegexXSD2Posix
import           TxsDefs
import           TxsShow
import           XmlFormat

-- import from valexpr
import           Constant
import           FuncDef
import           FuncId
import           Id
import           SortId
import           ValExpr             hiding (eval)
import           Variable

import           TorXakis.Compiler   (compileUnsafe, compileValExpr)

evalTuple :: Variable v => (ValExpr v, Integer) -> IOB.IOB (Either String (ValExpr v, Integer))
evalTuple (v,i) = do
    mc <- eval v
    case mc of
        Left t  -> return $ Left $ "evalTuple - " ++ t
        Right c -> return $ Right (cstrConst c,i)

-- ----------------------------------------------------------------------------------------- --
-- eval :  evaluation of value expression
--         eval shall only work on closed vexpr

eval :: Variable v => ValExpr v -> IOB.IOB (Either String Constant)
eval = eval' . ValExpr.view

eval' :: Variable v => ValExprView v -> IOB.IOB (Either String Constant)
eval' (Vfunc fid vexps) = do
     envb <- get
     let tdefs = IOB.tdefs envb
     case Map.lookup fid (funcDefs tdefs) of
       Nothing -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                   $ "Undefined function: " ++ fshow fid ]
                     return $ Left $ "eval - undefined function: " ++ fshow fid
       Just (FuncDef args' vexp)
               -> do mvals <- mapM eval vexps
                     case partitionEithers mvals of
                        ([],vals) -> let we = Map.fromList (zip args' vals) in do
                                        fdefs <- IOB.getFuncDefs
                                        eval (subst (Map.map cstrConst we) fdefs vexp)
                        (es, _)  -> return $ Left $ "eval - function args failed:\n" ++ Utils.join "\n" es

eval' (Vcstr cid vexps) = do
    mvals <- mapM eval vexps
    case partitionEithers mvals of
        ([], vals) -> return $ Right $ Ccstr cid vals
        (es, _)    -> return $ Left $ Utils.join "\n" es

eval' (Viscstr cid1 arg) = do
    mval <- eval arg
    case mval of
        Right (Ccstr cid2 _)-> return $ Right $ Cbool ( cid1 == cid2 )
        Right t             -> return $ Left $ "iscstr : wrong value " ++ show t
        Left t              -> return $ Left $ "iscstr : " ++ t

eval' (Vaccess _cid1 _n p arg) = do
    mval <- eval arg
    case mval of
        Right (Ccstr _cid2 args') -> return $ Right $ args'!!p
        Right t                   -> return $ Left $ "access : wrong value " ++ show t
        Left t                    -> return $ Left $ "access : " ++ t

eval' (Vconst const') = return $ Right const'

eval' (Vvar _vid) =
    return $ Left "var - Evaluation of value expression with free variable(s)"

eval' (Vite cond vexp1 vexp2) = do
    mval <- eval cond
    case mval of
        Right (Cbool val)   -> if val
                                  then eval vexp1
                                  else eval vexp2
        Right t             -> return $ Left $ "ite : wrong value " ++ show t
        Left t              -> return $ Left $ "ite : " ++ t


eval' (Vsum s) = do
    mconsts <- mapM evalTuple (toOccurListT s)
    case partitionEithers mconsts of
        ([], consts) -> eval (cstrSum $ fromOccurListT consts)       -- simplifies to integer
        (es, _)      -> return $ Left $ "Sum:\n" ++ Utils.join "\n  " es

eval' (Vproduct p) = do
    mconsts <- mapM evalTuple (toOccurListT p)
    case partitionEithers mconsts of
        ([], consts) -> eval (cstrProduct $ fromOccurListT consts)       -- simplifies to integer
        (es, _)      -> return $ Left $ "Product:\n" ++ Utils.join "\n  " es


eval' (Vdivide t n) = do
    mvalT <- txs2int t
    case mvalT of
        Right valT -> do
                        mvalN <- txs2int n
                        case mvalN of
                            Right valN -> return $ Right $ Cint $ valT `div` valN
                            Left e -> return $ Left $ "divide n " ++ show e
        Left e -> return $ Left $ "divide t " ++ show e

eval' (Vmodulo t n) = do
    mvalT <- txs2int t
    case mvalT of
        Right valT -> do
                        mvalN <- txs2int n
                        case mvalN of
                            Right valN -> return $ Right $ Cint $ valT `mod` valN
                            Left e -> return $ Left $ "modulo n " ++ show e
        Left e -> return $ Left $ "modulo t " ++ show e

eval' (Vgez vexp) = do
    mval <- eval vexp
    case mval of
        Right (Cint val) -> return $ Right $ Cbool ( 0 <= val)
        Right x          -> return $ Left $ "GEZ: not a int " ++ show x
        Left t           -> return $ Left $ "GEZ: not a int value" ++ show t

eval' (Vequal vexp1 vexp2) = do
    mval1 <- eval vexp1
    case mval1 of
        Right val1 -> do
                        mval2 <- eval vexp2
                        case mval2 of
                            Right val2 -> return $ Right $ Cbool ( val1 == val2 )
                            Left t     -> return $ Left $ "equal: not a value 2" ++ show t
        Left t     -> return $ Left $ "equal: not a value 1" ++ show t



eval' (Vnot vexp) = do
    mexp <- eval vexp
    case mexp of
        Right (Cbool val) -> return $ Right $ Cbool (not val)
        Right x           -> return $ Left $ "not: not a boolean " ++ show x
        Left t            -> return $ Left $ "not: not a boolean value " ++ show t


eval' (Vand vexps) = do
    mconsts <- mapM eval (Set.toList vexps)
    case partitionEithers mconsts of
        ([], consts) -> return $ Right $ Cbool $ all unBool consts
        (es, _)      -> return $ Left $ "And: " ++ Utils.join "\n  " es
  where unBool :: Constant -> Bool
        unBool (Cbool b) = b
        unBool _         = error "unBool applied on non-Bool"

eval' (Vlength vexp) = do
    mexp <- eval vexp
    case mexp of
        Right (Cstring val) -> return $ Right $ Cint $ Prelude.toInteger (T.length val)
        Right x           -> return $ Left $ "length: not a string " ++ show x
        Left t            -> return $ Left $ "length: not a string value " ++ show t

eval' (Vat s p) = do
    ms <- eval s
    case ms of
        Right (Cstring vs) -> do
                                mp <- eval p
                                case mp of
                                    Right (Cint vp) -> return $ Right $ Cstring (T.take 1 (T.drop (fromInteger vp) vs))
                                    Right x           -> return $ Left $ "at: not a int " ++ show x
                                    Left t            -> return $ Left $ "at: not a int value " ++ show t
        Right x           -> return $ Left $ "at: not a string " ++ show x
        Left t            -> return $ Left $ "at: not a string value " ++ show t

eval' (Vconcat vexprs') = do
    ms <- mapM eval vexprs'
    case partitionEithers ms of
        ([], vs) ->  let vs' = map (\(Cstring s) -> s) vs in
                        return $ Right $ Cstring (T.concat vs')
        (es, _)      -> return $ Left $ "Concat:\n" ++ Utils.join "\n  " es

eval' (Vstrinre s r) = do
    ms <- eval s
    case ms of
        Right (Cstring vs) -> do
                                mr <- eval r
                                case mr of
                                    Right (Cregex vr) -> return $ Right $ Cbool (T.unpack vs =~ T.unpack (xsd2posix vr))
                                    Right x           -> return $ Left $ "strinre: not a regex " ++ show x
                                    Left t            -> return $ Left $ "strinre: not a regex value " ++ show t
        Right x           -> return $ Left $ "strinre: not a string " ++ show x
        Left t            -> return $ Left $ "strinre: not a string value " ++ show t

eval' (Vpredef kd fid vexps) =
     case kd of
       AST -> case vexps of
                [vexp] -> do
                            mval <- eval vexp
                            case mval of
                                Right wal   -> return $ Right $ Cstring $ T.pack (pshow wal)
                                Left t      -> return $ Left t
                _      -> return $ Left "eval: AST"
       ASF -> case vexps of
                [vexp] -> do
                            ms <- txs2str vexp
                            case ms of
                                Right s -> do
                                            uid     <- gets IOB.unid
                                            sigs    <- gets IOB.sigs

                                            ((_,vexp'),e) <- lift $ catch
                                                ( let (i,p) = compileUnsafe $
                                                              compileValExpr sigs [] (_id uid + 1) (T.unpack s)
                                                   in return $! show p `deepseq` ((i, Just p),"")
                                                )
                                                ( \ec -> return ((uid, Nothing), show (ec::ErrorCall)))

                                            case vexp' of
                                                Just exp' -> eval exp'
                                                Nothing   -> return $ Left $ "eval: ASF\nvexpr: " ++ show s ++ "\nsignatures: " ++ show sigs ++ "\nerror: " ++ e
                                Left t  -> return $ Left t
                _      -> return $ Left "eval: ASF"
       AXT -> case vexps of
                [vexp] -> do
                            mwal <- eval vexp
                            case mwal of
                                Right wal   -> do
                                                tdefs <- gets IOB.tdefs
                                                return $ Right $ Cstring $ constToXml tdefs wal
                                Left t      -> return $ Left t
                _      -> return $ Left "eval: AXT"
       AXF -> case vexps of
                  [vexp] -> do
                                es <- txs2str vexp
                                case es of
                                    Right s -> do
                                                tdefs <- gets IOB.tdefs
                                                return $ Right $ constFromXml tdefs (funcsort fid) s
                                    Left t  -> return $ Left t
                  _      -> return $ Left "eval: AXF"
       SSB -> evalSSB fid vexps
       SSI -> evalSSI fid vexps
       SSS -> evalSSS fid vexps


-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for Bool - SSB

readBool :: Text -> Bool
readBool "True"  = True
readBool "true"  = True
readBool "False" = False
readBool "false" = False
readBool x       = error $ "Unable to parse bool " ++ show x

evalSSB :: Variable v => FuncId -> [ValExpr v] -> IOB.IOB (Either String Constant)
evalSSB (FuncId nm _ _ _) vexps =
     case ( nm, vexps ) of
       ( "toString",    [v]    ) -> do
                                        es <- txs2bool v
                                        case es of
                                            Right b -> return $ Right $ Cstring $ (T.pack . show) b
                                            Left t  -> return $ Left t
       ( "fromString",  [v]    ) -> do
                                        es <- txs2str v
                                        case es of
                                            Right s -> return $ Right $ Cbool $ readBool s
                                            Left t  -> return $ Left t

       ( "toXml",       [v]    ) -> do
                                        mwal <- eval v
                                        case mwal of
                                            Right wal   -> do
                                                            tdefs <- gets IOB.tdefs
                                                            return $ Right $ Cstring $ constToXml tdefs wal
                                            Left t  -> return $ Left t
       ( "fromXml",     [v]    ) -> do
                                        es <- txs2str v
                                        case es of
                                            Right s -> do
                                                        tdefs <- gets IOB.tdefs
                                                        return $ Right $ constFromXml tdefs sortIdBool s
                                            Left t  -> return $ Left t

       (s,_)                     -> return $ Left $ "evalSSB: standard Bool opn - unknown operator " ++ show s

-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for Int - SSI

evalSSI :: Variable v => FuncId -> [ValExpr v] -> IOB.IOB (Either String Constant)
evalSSI (FuncId nm _ _ _) vexps =
     case ( nm, vexps ) of
       ( "toString",    [v]    ) -> do
                                        es <- txs2int v
                                        case es of
                                            Right i -> return $ Right $ Cstring $ (T.pack . show) i
                                            Left t  -> return $ Left t
       ( "fromString",  [v]    ) -> do
                                        es <- txs2str v
                                        case es of
                                            Right s -> return $ Right $ Cint $ read (T.unpack s)
                                            Left t  -> return $ Left t
       ( "toXml",       [v]    ) -> do
                                        mwal <- eval v
                                        case mwal of
                                            Right wal   -> do
                                                            tdefs <- gets IOB.tdefs
                                                            return $ Right $ Cstring $ constToXml tdefs wal
                                            Left t  -> return $ Left t
       ( "fromXml",     [v]    ) -> do
                                        es <- txs2str v
                                        case es of
                                            Right s -> do
                                                        tdefs <- gets IOB.tdefs
                                                        return $ Right $ constFromXml tdefs sortIdInt s
                                            Left t  -> return $ Left t

       (s,_)                     -> return $ Left $ "evalSSI: standard Int opn - unknown operator " ++ show s

-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for String - SSS


evalSSS :: Variable v => FuncId -> [ValExpr v] -> IOB.IOB (Either String Constant)
evalSSS (FuncId nm _ _ _) vexps =
     case ( nm, vexps ) of
       ( "toString",   [v] ) -> do es <- txs2str v
                                   case es of
                                    Right s -> return $ Right $ Cstring s
                                    Left t  -> return $ Left t

       ( "fromString", [v] ) -> do es <- txs2str v
                                   case es of
                                    Right s -> return $ Right $ Cstring s
                                    Left t  -> return $ Left t

       ( "toXml",      [v] ) -> do mwal <- eval v
                                   case mwal of
                                    Right wal   -> do
                                                    tdefs <- gets IOB.tdefs
                                                    return $ Right $ Cstring $ constToXml tdefs wal
                                    Left t  -> return $ Left t

       ( "fromXml",    [v] ) -> do  es <- txs2str v
                                    case es of
                                        Right s -> do
                                                    tdefs <- gets IOB.tdefs
                                                    return $ Right $ constFromXml tdefs sortIdString s
                                        Left t  -> return $ Left t

       ( "takeWhile",    [v1,v2] ) -> do m1 <- txs2str v1
                                         m2 <- txs2str v2
                                         case partitionEithers [m1,m2] of
                                            ([], [s1,s2])   -> return $ Right $ Cstring $ T.takeWhile (`elemT` s1) s2
                                            (t, _)          -> return $ Left $ "takeWhile - " ++ Utils.join " - " t
       ( "takeWhileNot", [v1,v2] ) -> do m1 <- txs2str v1
                                         m2 <- txs2str v2
                                         case partitionEithers [m1,m2] of
                                            ([], [s1,s2])   -> return $ Right $ Cstring $ T.takeWhile (`notElemT` s1) s2
                                            (t, _)          -> return $ Left $ "takeWhileNot - " ++ Utils.join " - " t
       ( "dropWhile",    [v1,v2] ) -> do m1 <- txs2str v1
                                         m2 <- txs2str v2
                                         case partitionEithers [m1,m2] of
                                            ([], [s1,s2])   -> return $ Right $ Cstring $ T.dropWhile (`elemT` s1) s2
                                            (t, _)          -> return $ Left $ "dropWhile - " ++ Utils.join " - " t
       ( "dropWhileNot", [v1,v2] ) -> do m1 <- txs2str v1
                                         m2 <- txs2str v2
                                         case partitionEithers [m1,m2] of
                                            ([], [s1,s2])   -> return $ Right $ Cstring $ T.dropWhile (`notElemT` s1) s2
                                            (t, _)          -> return $ Left $ "dropWhileNot - " ++ Utils.join " - " t
       (s,_)                       -> return $ Left $ "evalSSS: standard String opn - unknown operator " ++ show s

elemT :: Char -> Text -> Bool
elemT c = isJust . T.find (== c)

notElemT :: Char -> Text -> Bool
notElemT c = not . elemT c

-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: values from torxakis to haskell and v.v.

txs2bool :: Variable v => ValExpr v -> IOB.IOB (Either String Bool)
txs2bool vexp = do
     mwal <- eval vexp
     case mwal of
       Right (Cbool b) -> return $ Right b
       Right v         -> return $ Left $ "txs2bool - not on Bool: " ++ show v
       Left t          -> return $ Left $ "txs2bool - " ++ t

txs2int :: Variable v => ValExpr v -> IOB.IOB (Either String Integer)
txs2int vexp = do
     mwal <- eval vexp
     case mwal of
       Right (Cint i) -> return $ Right i
       Right v        -> return $ Left $ "txs2int - not on Int: " ++ show v
       Left t         -> return $ Left $ "txs2int - " ++ t

txs2str :: Variable v => ValExpr v -> IOB.IOB (Either String Text)
txs2str vexp = do
     mwal <- eval vexp
     case mwal of
       Right (Cstring s) -> return $ Right s
       Right v           -> return $ Left $ "txs2str - not on String: " ++ show v
       Left t            -> return $ Left $ "txs2str - " ++ t

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
