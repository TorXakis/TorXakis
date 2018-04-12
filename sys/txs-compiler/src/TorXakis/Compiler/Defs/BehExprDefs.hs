{-# LANGUAGE FlexibleContexts #-}
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
import           ChanId (ChanId, chansorts)
import           VarId (VarId)

import           TorXakis.Compiler.Data hiding (lookupM)
import           TorXakis.Compiler.MapsTo
import           TorXakis.Compiler.ValExpr.ValExpr
import           TorXakis.Parser.Data

toBExpr :: ( MapsTo Text ChanId mm
           , MapsTo (Loc VarDeclE) VarId mm )
        => mm -> BExpDecl -> CompilerM BExpr
toBExpr _ Stop            = return stop
toBExpr mm (ActPref ao be) =
    actionPref <$> toActOffer mm ao <*> toBExpr mm be

toActOffer :: ( MapsTo Text ChanId mm
              , MapsTo (Loc VarDeclE) VarId mm )
           => mm -> ActOfferDecl -> CompilerM ActOffer
toActOffer mm (ActOfferDecl osd mc) = do
    os <- traverse (toOffer mm) osd
    c  <- fromMaybe (return . cstrConst . Cbool $ True)
-- TODO: uncomment this once we migrate the 'HasX' to 'MapsTo'
--                    (liftEither . expDeclToValExpr e e' sortIdBool <$> mc)
                    (undefined mm mc)
    return $ ActOffer (Set.fromList os) c

toOffer :: ( MapsTo Text ChanId mm
           , MapsTo (Loc VarDeclE) VarId mm )
        => mm -> OfferDecl -> CompilerM Offer
toOffer mm (OfferDecl cr cods) = do
    cId  <- lookupM (chanRefName cr) mm
    ofrs <- traverse (uncurry (toChanOffer mm))
                     (zip (chansorts cId) cods)
    return $ Offer cId ofrs

toChanOffer :: MapsTo (Loc VarDeclE) VarId mm
            => mm
            -> SortId -- ^ Expected sort id's the offer
            -> ChanOfferDecl
            -> CompilerM ChanOffer
toChanOffer mm _ (QuestD vd) =
    Quest  <$> lookupM (getLoc vd :: Loc VarDeclE) mm
toChanOffer mm eSid (ExclD ex) = liftEither $
-- TODO: uncomment this once we migrate the 'HasX' to 'MapsTo'    
    Exclam <$> undefined eSid mm ex -- expDeclToValExpr e e' eSid ex


