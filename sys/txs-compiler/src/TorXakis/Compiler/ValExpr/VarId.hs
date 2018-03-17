-- | 

module TorXakis.Compiler.ValExpr.VarId where

import           Data.Map (Map)
import qualified Data.Map as Map

import           VarId (VarId)

import TorXakis.Parser.Data
import TorXakis.Compiler.Data


generateVarIds :: (HasSortIds e)
               => e -> [FuncDecl] -> CompilerM (Map (Loc Field) VarId)
generateVarIds = undefined

-- fieldDeclToVarId :: Env -> FieldDecl -> CompilerM VarId
-- fieldDeclToVarId e f =
    
-- findVarDefM :: Hase -> FieldDecl -> CompilerM VarId
-- findVarDefM e f = 
--     case Map.lookup (uid . nodeMdata $ f) (varDefMap e) of
--         Nothing  -> throwError $ "Could not find variable " <> nodeName f
--         Just vId -> return vId
