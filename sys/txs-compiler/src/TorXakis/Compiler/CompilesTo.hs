--------------------------------------------------------------------------------
-- TODO: attempt to reduce duplication
--------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module TorXakis.Compiler.CompilesTo where

import           Control.Lens                       ((%~), (.~), (^.))
import           Control.Lens.TH                    (makeLenses)
import           Control.Monad.Error.Class          (liftEither)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Map.Strict                    (Map)
import           Data.Text                          (Text)

import           ChanId                             (ChanId)
import           FuncTable                          (Handler, Signature)
import           ProcId                             (ProcId)
import           SortId                             (SortId)
import           VarId                              (VarId)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Maps.DefinesAMap
import           TorXakis.Compiler.Maps.VarRef
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.ExpDecl
import           TorXakis.Compiler.ValExpr.SortId
import           TorXakis.Compiler.ValExpr.VarId
import           TorXakis.Parser.Data


-- | Information needed for compiling AST's to 'TorXakis' expressions.
data CInfo = CInfo
    { _text2sidM      :: Map Text SortId
    , _lvd2sidM       :: Map (Loc VarDeclE) SortId
    , _lvd2vidM       :: Map (Loc VarDeclE) VarId
    , _text2vdM       :: Map Text (Loc VarDeclE)
    , _text2lfdM      :: Map Text [Loc FuncDeclE]
    , _lfd2sgM        :: Map (Loc FuncDeclE) Signature
    , _lfd2sghdM      :: Map (Loc FuncDeclE) (Signature, Handler VarId)
    , _lvr2lvdOrlfdM  :: Map (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE])
    , _lvr2vidOrsghdM :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
    , _text2lchdM     :: Map Text (Loc ChanDeclE)
    , _lchr2lchdM     :: Map (Loc ChanRefE) (Loc ChanDeclE)
    , _lchd2chidM     :: Map (Loc ChanDeclE) ChanId
    , _pidsM          :: Map ProcId ()
    }
makeLenses ''CInfo

-- | Maps required to infer variable types for value expressions
type TypeInfMaps =  Map Text SortId
                   :& Map (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE])
                   :& Map (Loc VarDeclE) SortId
                   :& Map (Loc FuncDeclE) Signature
                   :& Map (Loc ChanRefE) (Loc ChanDeclE)
                   :& Map (Loc ChanDeclE) ChanId
                   :& Map ProcId ()

-- | Extract the 'VEInfo' from the given expression, which contains a value
-- expression.
extractInfo :: ( HasTypedVars TypeInfMaps ed
               , HasVarReferences ed
               , DeclaresVariables ed
               , Data ed
               , DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) ed (Map Text (Loc ChanDeclE))
               , DefinesAMap (Loc ChanDeclE) ChanId ed (Map Text SortId) )
            => CInfo -> ed -> CompilerM CInfo
extractInfo ci ed = do
    -- Map the channel references to the places in which they are declared.
    lchr2lchd' <- getMap (ci ^. text2lchdM) ed :: CompilerM (Map (Loc ChanRefE) (Loc ChanDeclE))
    -- Add channel declarations introduced by channel parameters or the hide operator.
    lchd2chid' <- getMap (ci ^. text2sidM) ed :: CompilerM (Map (Loc ChanDeclE) ChanId)
    lvr2lvdOrlfd' <- Map.fromList <$> mapRefToDecls (ci ^. text2vdM  :& ci ^. text2lfdM) ed
    -- Construct the map necessary for inferring variable types.
    let mm =  ci ^. text2sidM
           :& ci ^. lvr2lvdOrlfdM
           :& ci ^. lvd2sidM
           :& ci ^. lfd2sgM
           :& ci ^. lchr2lchdM
           :& ci ^. lchd2chidM
           :& ci ^. pidsM
    lvd2sid' <- Map.fromList <$> inferVarTypes mm ed
    lvd2vid' <- Map.fromList <$> mkVarIds (lvd2sid' <.+> ci ^. lvd2sidM) ed
    let mm' =  lvr2lvdOrlfd'
            :& lvd2vid' `Map.union` (ci ^. lvd2vidM)
            :& ci ^. lfd2sghdM
    lvr2vidOrsghd <- liftEither $ varDefsFromExp mm' ed
    return $ ci & lvr2lvdOrlfdM  %~ Map.union lvr2lvdOrlfd'
                & lvd2sidM       %~ Map.union lvd2sid'
                & lvr2vidOrsghdM %~ Map.union lvr2vidOrsghd
                & lchr2lchdM %~ Map.union lchr2lchd'
                & lchd2chidM %~ Map.union lchd2chid'


class CompilesTo ed t where
    compileTo :: ( HasTypedVars TypeInfMaps ed
               , HasVarReferences ed
               , DeclaresVariables ed
               , Data ed
               , DefinesAMap (Loc ChanRefE) (Loc ChanDeclE) ed (Map Text (Loc ChanDeclE))
               , DefinesAMap (Loc ChanDeclE) ChanId ed (Map Text SortId) )
              => CInfo -> ed -> CompilerM t
    compileTo i ed = do
        i' <- extractInfo i ed
        compileToWithInfo i' ed

    compileToWithInfo :: CInfo -> ed -> CompilerM t
