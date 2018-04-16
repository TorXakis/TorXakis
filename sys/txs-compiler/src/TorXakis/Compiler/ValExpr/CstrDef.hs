{-# LANGUAGE FlexibleContexts  #-}
module TorXakis.Compiler.ValExpr.CstrDef where

import Prelude hiding (lookup)
import           Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import           SortId (SortId)
import           CstrId (CstrId)
import           CstrDef (CstrDef (CstrDef))

import           TorXakis.Parser.Data
import           TorXakis.Compiler.Data hiding (lookup, lookupM)
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.ValExpr.FuncId
    
compileToCstrDefs :: ( MapsTo Text        SortId mm
                     , MapsTo (Loc CstrE) CstrId mm )
                  => mm -> [ADTDecl] -> CompilerM (Map CstrId CstrDef)
compileToCstrDefs mm ds = 
    Map.fromList . concat <$> traverse (adtToCstrDefs mm) ds

adtToCstrDefs :: ( MapsTo Text        SortId mm
                 , MapsTo (Loc CstrE) CstrId mm )
               => mm -> ADTDecl -> CompilerM [(CstrId, CstrDef)]
adtToCstrDefs mm a =
    traverse (cstrToCstrDefs mm) (constructors a)

cstrToCstrDefs :: ( MapsTo Text        SortId mm
                  , MapsTo (Loc CstrE) CstrId mm )
               => mm -> CstrDecl -> CompilerM (CstrId, CstrDef)
cstrToCstrDefs mm c = do
    cId <- lookupM (getLoc c) mm
    isCstrFid <- cstrToIsCstrFuncId cId
    cstrAccFids <- traverse (cstrToAccFuncId mm cId) (cstrFields c)
    return (cId, CstrDef isCstrFid cstrAccFids)
