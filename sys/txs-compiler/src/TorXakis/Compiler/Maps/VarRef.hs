{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Functions to manipulate maps of variable references.
module TorXakis.Compiler.Maps.VarRef where

import           Control.Arrow            ((|||))
import           Control.Lens             ((^..))
import           Control.Lens.Plated      (cosmosOn)
import           Data.Data                (Data)
import           Data.Data.Lens           (biplate, template)
import           Data.Map                 (Map)
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set

import           FuncTable                (Handler, Signature)
import           VarId                    (VarId)

import           TorXakis.Compiler.Error
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.MapsTo
import           TorXakis.Parser.Data

-- | Given an expression, construct a map from variable references to the
-- concrete TorXakis entities they refer to.
--
varDefsFromExp :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE]) mm
                  , MapsTo (Loc VarDeclE) VarId mm
                  , MapsTo (Loc FuncDeclE) (Signature, Handler VarId) mm
                  , Data e )
               => mm
               -> e
               -> Either Error (Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)]))
varDefsFromExp mm e = varRefsToVarDefs eVrvds vids fshs
    where
      -- Variable references to declarations from the mm.
      vrvds :: Map (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE])
      vrvds = innerMap mm
      -- Locations of all variable references (note that a variable declaration
      -- refers to itself).
      lvrs :: [Loc VarRefE]
      lvrs = (e ^.. cosmosOn biplate) ++ (asVarReflLoc <$> lvds)
      -- Locations of all the variable declarations.
      lvds :: [Loc VarDeclE]
      lvds = e ^.. cosmosOn biplate
      -- Restrict the variable references in 'vrvds' to those references
      -- reachable from the current expression ('e'). Otherwise
      -- 'varRefsToVarDefs' will return an error.
      eVrvds = Map.restrictKeys vrvds (Set.fromList lvrs)
      vids :: Map (Loc VarDeclE) VarId
      vids = innerMap mm
      fshs :: Map (Loc FuncDeclE) (Signature, Handler VarId)
      fshs = innerMap mm

-- | Construct a map from variable references to either variable id's or signature-handler pairs.
--
-- The three given maps are joined to calculate the end-result. Every
-- declaration location must have an image in the second o third map, otherwise
-- an error is returned.
varRefsToVarDefs :: Map (Loc VarRefE) (Either (Loc VarDeclE) [Loc FuncDeclE])
                 -> Map (Loc VarDeclE) VarId
                 -> Map (Loc FuncDeclE) (Signature, Handler VarId)
                 -> Either Error (Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)]))
varRefsToVarDefs vdecls vids fshs = do
    defs <- traverse declToDef (snd <$> vdecls')
    return $ Map.fromAscList $ zip (fst <$> vdecls') defs
    where
      vdecls' :: [(Loc VarRefE, Either (Loc VarDeclE) [Loc FuncDeclE])]
      vdecls' = Map.toList vdecls
      declToDef :: Either (Loc VarDeclE) [Loc FuncDeclE]
                -> Either Error (Either VarId [(Signature, Handler VarId)])
      declToDef (Left vid)  = Left <$> varIdforDecl vid
      declToDef (Right fls) = Right <$> traverse shForDecl fls
      varIdforDecl :: Loc VarDeclE -> Either Error VarId
      varIdforDecl l = maybe vidNotFound Right (Map.lookup l vids)
          where
            vidNotFound = Left Error
                { _errorType = UndefinedRef
                , _errorLoc = getErrorLoc l
                , _errorMsg = "varRefsToVarDefs: VarId not found."
                }
      shForDecl :: Loc FuncDeclE -> Either Error (Signature, Handler VarId)
      shForDecl l = maybe shNotFound Right (Map.lookup l fshs)
          where
            shNotFound = Left Error
                { _errorType = UndefinedRef
                , _errorLoc = getErrorLoc l
                , _errorMsg =  "varRefsToVarDefs: Signature-Handler not found."
                }


varIdForRef :: Map (Loc VarRefE) (Either VarId [(Signature, Handler VarId)])
            -> Loc VarRefE
            -> Either Error VarId
varIdForRef vrvds vr  = do
    eVid <- vrvds .@@ vr
    Right ||| err $ eVid
    where
      err :: [(Signature, Handler VarId)] -> Either Error VarId
      err _ = Left Error
              { _errorType = UndefinedRef
              , _errorLoc = getErrorLoc vr
              , _errorMsg = "VarId not found."
              }

