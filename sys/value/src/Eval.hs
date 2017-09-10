{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

-- ----------------------------------------------------------------------------------------- --
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
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
, evalCnrs   -- evalCnrs :: Variable v => [ValExpr v] -> IOB.IOB Bool
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.State
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Read
import           Text.Regex.TDFA
import           TextShow

import qualified Data.List           as List
import qualified Data.Map            as Map
import qualified Data.Set            as Set

-- import from behavedefs
import qualified EnvBTree            as IOB
import qualified EnvData

-- import from defs
import           StdTDefs
import           TxsDefs
import           TxsShow
import           TxsUtils
import           XmlFormat

-- import from front
import qualified TxsAlex
import qualified TxsHappy

-- import from solve
import           RegexAlex
import           RegexPosixHappy

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
                     eval (cstrEnv (Map.map cstrConst we) vexp)

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

eval (view -> Venv ve vexp) = eval (TxsUtils.partSubst ve vexp)

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

eval (view -> Vpredef kd fid vexps) =
     case kd of
       AFS -> case vexps of
                [vexp]        -> evalAFS fid vexp
                _             -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                  $ "eval: AFS: " ++ show fid ]
                                    return $ Cerror ""
       ACC -> case vexps of
                [vexp]        -> evalACC fid vexp
                _             -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                                  $ "eval: ACC: " ++ show fid ]
                                    return $ Cerror ""
       AST -> case vexps of
                [vexp] -> do wal <- eval vexp
                             str2txs (T.pack (pshow wal))
                _      -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "eval: AST" ]
                             return $ Cerror ""
       ASF -> case vexps of
                [vexp] -> do s <- txs2str vexp
                             uid     <- gets IOB.unid
                             tdefs   <- gets IOB.tdefs
                             sigs    <- gets IOB.sigs
                             ((_,vexp'),e) <- lift $ catch
                                ( let p = TxsHappy.vexprParser ( TxsAlex.Ctdefs  tdefs
                                                               : TxsAlex.Csigs   sigs
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
                _      -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "eval AXT" ]
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
       SSR -> evalSSR fid vexps

eval (view -> Verror str) = do
     IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_WARNING $ "eval: Verror " ++ show str ]
     return $ Cerror ""

eval _ = return $ Cerror "undefined"
-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: algebraic field selection

evalAFS :: Variable v => FuncId -> ValExpr v -> IOB.IOB Const
evalAFS fid vexp = do
     tdefs <- gets IOB.tdefs
     val   <- eval vexp
     case val of
       Cstr cid vals
         -> case Map.lookup cid (cstrDefs tdefs) of
              Just (CstrDef _ fs)
                -> case List.elemIndices fid fs of
                     [index] -> return $ vals!!index
                     _       -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR $
                                                     "evalAFS: no field: " ++ fshow fid ]
                                   return $ Cerror ""
              _ -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                    $ "evalAFS: used constructor not found: " ++ fshow val ]
                      return $ Cerror ""
       _ -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                             $ "evalAFS: fiels selection not on constructor:" ++ fshow val ]
               return $ Cerror ""

-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: algebraic constructor check

evalACC :: Variable v => FuncId -> ValExpr v -> IOB.IOB Const
evalACC fid vexp = do
     tdefs <- gets IOB.tdefs
     val   <- eval vexp
     case val of
       Cstr cid _
         -> case Map.lookup cid (cstrDefs tdefs) of
              Just (CstrDef cc _) -> bool2txs (cc == fid)
              _                   -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                           $ "evalACC: constructor not found: " ++ show val ]
                                        return $ Cerror "evalACC: constructor not found"
       _ -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                             $ "evalACC: constr check not on constructor: " ++ show val ]
               return $ Cerror ""

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
                                        str2txs $ showt b1
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
                                        str2txs $ showt i1
       ( "fromString",  [v1]    ) -> do s1 <- txs2str v1
                                        int2txs $ read (T.unpack s1)
       ( "toXml",       [v1]    ) -> do wal <- eval v1
                                        tdefs <- gets IOB.tdefs
                                        str2txs $ constToXml tdefs wal
       ( "fromXml",     [v1]    ) -> do Cstring s <- eval v1
                                        tdefs <- gets IOB.tdefs
                                        return $ constFromXml tdefs sortId_Int s
       ( "-",           [v1]    ) -> do i1 <- txs2int v1
                                        int2txs $ (-1) * i1
       ( "+",           [v1,v2] ) -> do i1 <- txs2int v1
                                        i2 <- txs2int v2
                                        int2txs $ i1 + i2
       ( "-",           [v1,v2] ) -> do i1 <- txs2int v1
                                        i2 <- txs2int v2
                                        int2txs $ i1 - i2
       ( "*",           [v1,v2] ) -> do i1 <- txs2int v1
                                        i2 <- txs2int v2
                                        int2txs $ i1 * i2
       ( "/",           [v1,v2] ) -> do i1 <- txs2int v1
                                        i2 <- txs2int v2
                                        int2txs $ i1 `div` i2
       ( "%",           [v1,v2] ) -> do i1 <- txs2int v1
                                        i2 <- txs2int v2
                                        int2txs  $ i1 `mod` i2
       ( "<",           [v1,v2] ) -> do i1 <- txs2int v1
                                        i2 <- txs2int v2
                                        bool2txs $ i1 < i2
       ( "<=",          [v1,v2] ) -> do i1 <- txs2int v1
                                        i2 <- txs2int v2
                                        bool2txs $ i1 <= i2
       ( ">",           [v1,v2] ) -> do i1 <- txs2int v1
                                        i2 <- txs2int v2
                                        bool2txs $ i1 > i2
       ( ">=",          [v1,v2] ) -> do i1 <- txs2int v1
                                        i2 <- txs2int v2
                                        bool2txs $ i1 >= i2
       ( "abs",         [v1]    ) -> do i1 <- txs2int v1
                                        int2txs $ if i1<0 then -i1 else i1
       _                          -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "evalSSI: standard Int opn" ]
                                        return $ Cerror ""

-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for String - SSS


evalSSS :: Variable v => FuncId -> [ValExpr v] -> IOB.IOB Const
evalSSS (FuncId nm _ _ _) vexps =
     case ( nm, vexps ) of
       ( "toString",   [v] ) -> do s <- txs2str v
                                   -- TODO: consider changing the type of `str2txs` to use text to avoid packing and unpacking.
                                   str2txs $ T.pack $ show s
       ( "fromString", [v] ) -> do s <- txs2str v
                                   str2txs $ read (T.unpack s)
       ( "toXml",      [v] ) -> do wal <- eval v
                                   tdefs <- gets IOB.tdefs
                                   str2txs $ constToXml tdefs wal
       ( "fromXml",    [v] ) -> do Cstring s <- eval v
                                   tdefs <- gets IOB.tdefs
                                   return $ constFromXml tdefs sortId_String s
       ( "++",     [v1,v2] ) -> do s1 <- txs2str v1
                                   s2 <- txs2str v2
                                   str2txs $ s1 <> s2
       ( "len",    [v1]    ) -> do s1 <- txs2str v1
                                   int2txs  $ toInteger (T.length s1)
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
       ( "at", [v1,v2] )           -> do s1 <- txs2str v1
                                         i2 <- txs2int v2
                                         let i2' = fromInteger i2
                                         str2txs $ if 0<=i2' && i2'< T.length s1
                                                   then T.singleton (T.index s1 i2') else ""
       _                           -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "evalSSS: standard String opn" ]
                                         return $ Cerror ""

elemT :: Char -> Text -> Bool
elemT c = isJust . T.find (== c)

notElemT :: Char -> Text -> Bool
notElemT c = not . elemT c

-- ----------------------------------------------------------------------------------------- --
-- evaluation of value expression: evaluation of standard functions for Regex - SSR

evalSSR :: Variable v => FuncId -> [ValExpr v] -> IOB.IOB Const
evalSSR (FuncId nm _ _ _) vexps =
     case ( nm, vexps ) of
       ( "strinre",[v1,v2] ) -> do rawRegex     <- T.unpack <$> txs2regex v2
                                   let haskellRegex = T.unpack $ regexPosixParser (regexLexer rawRegex)
                                   value <- T.unpack <$> txs2str v1
                                   bool2txs $ value =~ haskellRegex
       _ -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR "evalSSR: error in standard Regex opn" ]
               return $ Cerror ""

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

txs2regex :: Variable v => ValExpr v -> IOB.IOB Text
txs2regex vexp = do
     wal <- eval vexp
     case wal of
       Cregex r -> return r
       v        -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                                    $ "txs2regex: not on Regex: " ++ show v ]
                      return ""

-- ----------------------------------------------------------------------------------------- --
-- evaluation of constraints

evalCnr :: Variable v => ValExpr v -> IOB.IOB Bool
evalCnr vexp = do
     val <- eval vexp
     return $ val == Cbool True

evalCnrs :: Variable v => [ValExpr v] -> IOB.IOB Bool
evalCnrs []  = return True
evalCnrs (vexp:vexps) = do
    val <- evalCnr vexp
    if val then
        evalCnrs vexps
    else
        return False

-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --
