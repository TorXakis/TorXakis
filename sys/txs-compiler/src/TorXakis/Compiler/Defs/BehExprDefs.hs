{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module TorXakis.Compiler.Defs.BehExprDefs where

import           Control.Monad.Error.Class         (liftEither)
import           Data.Maybe                        (fromMaybe)
import qualified Data.Set                          as Set
import           Data.Text (Text)

import           ConstDefs                         (Const (Cbool))
import           SortId                            (sortIdBool, SortId)
import           TxsDefs                           (ActOffer (ActOffer), BExpr, ChanOffer (Quest, Exclam),
                                                    Offer (Offer), actionPref, stop)
import           ValExpr                           (cstrConst)
import           ChanId (ChanId (ChanId), chansorts, name, unid)
import           VarId (VarId)
import           FuncId (FuncId)
import           FuncDef (FuncDef)

import           TorXakis.Compiler.Data
import           TorXakis.Compiler.Maps
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.ValExpr
import           TorXakis.Parser.Data

toBExpr :: ( MapsTo Text ChanId mm
           , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
           , MapsTo (Loc VarDeclE) VarId mm
           , MapsTo FuncDefInfo FuncId mm
           , MapsTo FuncId (FuncDef VarId) mm )
        => mm -> BExpDecl -> CompilerM BExpr
toBExpr _ Stop            = return stop
toBExpr mm (ActPref ao be) =
    actionPref <$> toActOffer mm ao <*> toBExpr mm be

toActOffer :: ( MapsTo Text ChanId mm
              , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
              , MapsTo (Loc VarDeclE) VarId mm
              , MapsTo FuncDefInfo FuncId mm
              , MapsTo FuncId (FuncDef VarId) mm )
           => mm -> ActOfferDecl -> CompilerM ActOffer
toActOffer mm (ActOfferDecl osd mc) = do
    os <- traverse (toOffer mm) osd
    c  <- fromMaybe (return . cstrConst . Cbool $ True)
                    (liftEither . expDeclToValExpr mm sortIdBool <$> mc)
    return $ ActOffer (Set.fromList os) c

toOffer :: ( MapsTo Text ChanId mm
           , MapsTo (Loc VarDeclE) VarId mm
           , MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
           , MapsTo (Loc VarDeclE) VarId mm
           , MapsTo FuncDefInfo FuncId mm
           , MapsTo FuncId (FuncDef VarId) mm )
        => mm -> OfferDecl -> CompilerM Offer
toOffer mm (OfferDecl cr cods) = do
    cId  <- lookupM (chanRefName cr) mm
            <!!> cr
    ofrs <- traverse (uncurry (toChanOffer mm))
                     (zip (chansorts cId) cods)
    -- | TODO: QUESTION: Here TorXakis assigns the empty list of SortId's to
    -- the EXIT channel. Why is TorXakis not using the expected SortId's? To
    -- comply with the curent compiler I have to erase the sort Ids.
    let cId' =
            case name cId of
                "EXIT" -> ChanId (name cId) (unid cId) []
                _      -> cId
    return $ Offer cId' ofrs

toChanOffer :: ( MapsTo (Loc VarRefE) (Either (Loc VarDeclE) [FuncDefInfo]) mm
               , MapsTo (Loc VarDeclE) VarId mm
               , MapsTo FuncDefInfo FuncId mm
               , MapsTo FuncId (FuncDef VarId) mm )
            => mm
            -> SortId -- ^ Expected sort id's the offer
            -> ChanOfferDecl
            -> CompilerM ChanOffer
toChanOffer mm _ (QuestD vd) =
    Quest  <$> mm .@ getLoc vd
toChanOffer mm eSid (ExclD ex) = liftEither $
    Exclam <$> expDeclToValExpr mm eSid ex


