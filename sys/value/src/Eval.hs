{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Eval

-- ----------------------------------------------------------------------------------------- --
--
--
-- Evaluation of Value Expressions
--
-- ----------------------------------------------------------------------------------------- --
-- export

( eval       -- eval :: (TxsDefs.Variable v) => (TxsDefs.ValExpr v) -> IOB.IOB Const
             -- evaluation of value expression; eval shall only work on closed vexpr
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.State
import           Data.Maybe
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
import           StdTDefs
import           TxsDefs
import           TxsShow
import           XmlFormat

-- import from front
import qualified TxsAlex
import qualified TxsHappy


-- ----------------------------------------------------------------------------------------- --
-- eval :  evaluation of value expression
--         eval shall only work on closed vexpr

eval :: TxsDefs.Variable v => TxsDefs.ValExpr v -> IOB.IOB Const

eval (view -> Vfunc fid vexps) = do
     envb <- get
     let tdefs = IOB.tdefs envb
     case Map.lookup fid (funcDefs tdefs) of
       Nothing -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                   $ "Undefined function: " ++ fshow fid ]
                     return $ Cerror "eval: undefined function"
       Just (FuncDef args' vexp)
               -> do vals <- mapM eval vexps
                     let we = Map.fromList (zip args' vals)
                     eval (subst (Map.map cstrConst we) (Map.empty :: Map.Map FuncId (FuncDef VarId)) vexp)

eval (view -> Vcstr cid vexps) = do
    vals <- mapM eval vexps
    return $ Cstr cid vals

eval (view -> Viscstr cid1 arg) = do
    Cstr cid2 _ <- eval arg
    bool2txs ( cid1 == cid2 )

eval (view -> Vaccess _cid1 p arg) = do
    Cstr _cid2 args' <- eval arg
    return $ args'!!p                   -- TODO: check cids are equal?

eval (view -> Vconst const') = return const'

eval (view -> Vvar _vid) = do
    IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "Evaluation of value expression with free variable(s)" ]
    return $ Cerror ""

eval (view -> Vite cond vexp1 vexp2) = do
    Cbool val <- eval cond
    if val
      then eval vexp1
      else eval vexp2

eval (view -> Vsum s) = do
    consts <- mapM evalTuple (toOccurListT s)
    eval (cstrSum $ fromOccurListT consts)       -- simplifies to integer
  where
    evalTuple :: Variable v => (TxsDefs.ValExpr v, Integer) -> IOB.IOB (TxsDefs.ValExpr v, Integer)
    evalTuple (v,i) = do
        c <- eval v
        return (cstrConst c,i)

eval (view -> Vproduct p) = do
    consts <- mapM evalTuple (toOccurListT p)
    eval (cstrProduct $ fromOccurListT consts)       -- simplifies to integer
  where
    evalTuple :: Variable v => (TxsDefs.ValExpr v, Integer) -> IOB.IOB (TxsDefs.ValExpr v, Integer)
    evalTuple (v,i) = do
        c <- eval v
        return (cstrConst c,i)

eval (view -> Vdivide t n) = do
    valT <- txs2int t
    valN <- txs2int n
    int2txs $ valT `div` valN

eval (view -> Vmodulo t n) = do
    valT <- txs2int t
    valN <- txs2int n
    int2txs $ valT `mod` valN

eval (view -> Vgez v) = do
    val <- txs2int v
    bool2txs ( 0 <= val )

eval (view -> Vequal vexp1 vexp2) = do
    val1 <- eval vexp1
    val2 <- eval vexp2
    bool2txs ( val1 == val2 )

eval (view -> Vnot vexp) = do
    Cbool val <- eval vexp
    bool2txs (not val)

eval (view -> Vand vexps) = do
    consts <- mapM eval (Set.toList vexps)
    bool2txs $ all unBool consts
  where unBool :: Const -> Bool
        unBool (Cbool b) = b
        unBool _         = error "unBool applied on non-Bool"

eval (view -> Vlength vexp) = do
    Cint val <- eval vexp
    int2txs val
        
eval (view -> Vat s p) = do
    Cstring vs <- eval s
    Cint vp <- eval p
    str2txs (T.take 1 (T.drop (fromInteger vp) vs))

eval (view -> Vconcat vexprs) = do
    vs <- mapM eval vexprs
    let vs' = map (\(Cstring s) -> s) vs
    str2txs (T.concat vs')

eval (view -> Vstrinre s r) = do
    Cstring vs <- eval s
    Cregex vr <- eval r
    bool2txs (T.unpack vs =~ T.unpack (xsd2posix vr))

eval (view -> Vpredef kd fid vexps) =
     case kd of
       AST -> case vexps of
                [vexp] -> do wal <- eval vexp
                             str2txs (T.pack (pshow wal))
                _      -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "eval: AST" ]
                             return $ Cerror ""
       ASF -> case vexps of
                [vexp] -> do s <- txs2str vexp
                             uid     <- gets IOB.unid
                             sigs    <- gets IOB.sigs
                             ((_,vexp'),e) <- lift $ catch
                                ( let p = TxsHappy.vexprParser ( TxsAlex.Csigs   sigs
                                                               : TxsAlex.Cvarenv []
                                                               : TxsAlex.Cunid (uid + 1)
                                                               : TxsAlex.txsLexer (T.unpack s)
                                                               )
                                   in return $! show p `deepseq` (p,"")
                                )
                                ( \e -> return ((uid, cstrError ""), show (e::ErrorCall)))
                             if  e /= ""
                               then do IOB.putMsgs $ map EnvData.TXS_CORE_SYSTEM_ERROR
                                         [ "eval: ASF"
                                         , "vexpr: " ++ show s
                                         , "signatures" ++ show sigs
                                         ]
                                       return $ Cerror ""
                               else eval vexp'
                _      -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "eval: ASF" ]
                             return $ Cerror ""
       AXT -> case vexps of
                [vexp] -> do wal <- eval vexp
                             tdefs <- gets IOB.tdefs
                             str2txs $ constToXml tdefs wal
                _      -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "eval: AXT" ]
                             return $ Cerror ""
       AXF -> case vexps of
                  [vexp] -> do Cstring s <- eval vexp
                               tdefs <- gets IOB.tdefs
                               return $ constFromXml tdefs (funcsort fid) s
                  _      -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "eval: AXF" ]
                               return $ Cerror ""
       SSB -> evalSSB fid vexps
       SSI -> evalSSI fid vexps
       SSS -> evalSSS fid vexps

eval (view -> Verror str) = do
     IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_WARNING $ "eval: Verror " ++ show str ]
     return $ Cerror ""

eval _ = return $ Cerror "undefined"

-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for Bool - SSB

-- TODO: see how to make this exception safe.
readBool :: Text -> Bool
readBool "True"  = True
readBool "true"  = True
readBool "False" = False
readBool "false" = False
readBool x       = error $ "Unable to parse bool " ++ show x

evalSSB :: Variable v => FuncId -> [ValExpr v] -> IOB.IOB Const
evalSSB (FuncId nm _ _ _) vexps =
     case ( nm, vexps ) of
       ( "toString",    [v1]    ) -> do b1 <- txs2bool v1
                                        str2txs $ (T.pack . show) b1
       ( "fromString",  [v1]    ) -> do s1 <- txs2str v1
                                        bool2txs $ readBool s1
       ( "toXml",       [v1]    ) -> do wal <- eval v1
                                        tdefs <- gets IOB.tdefs
                                        str2txs $ constToXml tdefs wal
       ( "fromXml",     [v1]    ) -> do Cstring s <- eval v1
                                        tdefs <- gets IOB.tdefs
                                        return $ constFromXml tdefs sortId_Bool s
       ( s, _ )                   -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                      $ "evalSSB: unknown standard Bool opn - " ++ show s ]
                                        return $ Cerror ("unknown " ++ show s)

-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for Int - SSI

evalSSI :: Variable v => FuncId -> [ValExpr v] -> IOB.IOB Const
evalSSI (FuncId nm _ _ _) vexps =
     case ( nm, vexps ) of
       ( "toString",    [v1]    ) -> do i1 <- txs2int v1
                                        str2txs $ (T.pack . show) i1
       ( "fromString",  [v1]    ) -> do s1 <- txs2str v1
                                        int2txs $ read (T.unpack s1)
       ( "toXml",       [v1]    ) -> do wal <- eval v1
                                        tdefs <- gets IOB.tdefs
                                        str2txs $ constToXml tdefs wal
       ( "fromXml",     [v1]    ) -> do Cstring s <- eval v1
                                        tdefs <- gets IOB.tdefs
                                        return $ constFromXml tdefs sortId_Int s
       _                          -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "evalSSI: standard Int opn" ]
                                        return $ Cerror ""

-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for String - SSS


evalSSS :: Variable v => FuncId -> [ValExpr v] -> IOB.IOB Const
evalSSS (FuncId nm _ _ _) vexps =
     case ( nm, vexps ) of
       ( "toString",   [v] ) -> do s <- txs2str v
                                   str2txs s
       ( "fromString", [v] ) -> do s <- txs2str v
                                   str2txs s
       ( "toXml",      [v] ) -> do wal <- eval v
                                   tdefs <- gets IOB.tdefs
                                   str2txs $ constToXml tdefs wal
       ( "fromXml",    [v] ) -> do Cstring s <- eval v
                                   tdefs <- gets IOB.tdefs
                                   return $ constFromXml tdefs sortId_String s
       ( "takeWhile",    [v1,v2] ) -> do s1 <- txs2str v1
                                         s2 <- txs2str v2
                                         str2txs $ T.takeWhile (`elemT` s1) s2
       ( "takeWhileNot", [v1,v2] ) -> do s1 <- txs2str v1
                                         s2 <- txs2str v2
                                         str2txs $ T.takeWhile (`notElemT` s1) s2
       ( "dropWhile",    [v1,v2] ) -> do s1 <- txs2str v1
                                         s2 <- txs2str v2
                                         str2txs $ T.dropWhile (`elemT` s1) s2
       ( "dropWhileNot", [v1,v2] ) -> do s1 <- txs2str v1
                                         s2 <- txs2str v2
                                         str2txs $ T.dropWhile (`notElemT` s1) s2
       _                           -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "evalSSS: standard String opn" ]
                                         return $ Cerror ""

elemT :: Char -> Text -> Bool
elemT c = isJust . T.find (== c)

notElemT :: Char -> Text -> Bool
notElemT c = not . elemT c

-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: values from torxakis to haskell and v.v.

txs2bool :: Variable v => ValExpr v -> IOB.IOB Bool
txs2bool vexp = do
     wal <- eval vexp
     case wal of
        Cbool b -> return b
        _       -> do
                        IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                              $ "txs2bool: not on Bool: " ++ show wal ]
                        return False  -- PvdL: why False?

bool2txs :: Bool -> IOB.IOB Const
bool2txs b = return $ Cbool b

txs2int :: Variable v => ValExpr v -> IOB.IOB Integer
txs2int vexp = do
     wal <- eval vexp
     case wal of
       Cint i -> return i
       v      -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                  $ "txs2int: not on Int: " ++ show v ]
                    return 0

int2txs :: Integer -> IOB.IOB Const
int2txs i = return $ Cint i

txs2str :: Variable v => ValExpr v -> IOB.IOB Text
txs2str vexp = do
     wal <- eval vexp
     case wal of
       Cstring s -> return s
       v         -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                     $ "txs2str: not on String: " ++ show v ]
                       return ""

str2txs :: Text -> IOB.IOB Const
str2txs = return . Cstring

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
