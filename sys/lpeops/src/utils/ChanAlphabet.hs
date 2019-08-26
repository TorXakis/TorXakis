{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and University of Twente
See LICENSE at root directory of this repository.
-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ChanAlphabet
-- Copyright   :  TNO and University of Twente
-- License     :  BSD3
-- Maintainer  :  djurrevanderwal@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module ChanAlphabet (
getChanAlphabet,
isInvisibleChan,
isInvisibleOffer,
isInvisibleActOffer,
removeInvisibleChans,
isInChanAlphabet,
isActOfferInChanAlphabet,
getActOfferChans
) where

import qualified Data.Set as Set
import qualified TxsDefs
import qualified ChanId
import qualified BehExprDefs

-- Checks if a ChanId is invisible (==ISTEP).
isInvisibleChan :: ChanId.ChanId -> Bool
isInvisibleChan cid = cid == TxsDefs.chanIdIstep

-- Checks if an Offer is invisible (==ISTEP).
isInvisibleOffer :: BehExprDefs.Offer -> Bool
isInvisibleOffer = isInvisibleChan . BehExprDefs.chanid

-- Removes all invisible channels from a set.
removeInvisibleChans :: Set.Set ChanId.ChanId -> Set.Set ChanId.ChanId
removeInvisibleChans = Set.filter (not . isInvisibleChan)

-- Constructs a channel alphabet from input and output (multi-)channels.
getChanAlphabet :: [Set.Set ChanId.ChanId] -> [Set.Set ChanId.ChanId] -> Set.Set (Set.Set ChanId.ChanId)
getChanAlphabet inChans outChans = Set.fromList (map removeInvisibleChans ([Set.empty] ++ inChans ++ outChans))

-- Extracts channels from an ActOffer (visible only).
getActOfferChans :: BehExprDefs.ActOffer -> Set.Set ChanId.ChanId
getActOfferChans actOffer = removeInvisibleChans (Set.map BehExprDefs.chanid (BehExprDefs.offers actOffer))

-- Checks if an ActOffer is invisible (==all of its Offers are invisible).
isInvisibleActOffer :: BehExprDefs.ActOffer -> Bool
isInvisibleActOffer actOffer = Set.null (getActOfferChans actOffer)

-- Checks if a given (multi-)channel is in the specified alphabet.
isInChanAlphabet :: Set.Set (Set.Set ChanId.ChanId) -> Set.Set ChanId.ChanId -> Bool
isInChanAlphabet chanAlphabet candidateChans = Set.member (removeInvisibleChans candidateChans) chanAlphabet

-- Checks if the (multi-)channel of a given ActOffer is in the specified alphabet.
isActOfferInChanAlphabet :: Set.Set (Set.Set ChanId.ChanId) -> BehExprDefs.ActOffer -> Bool
isActOfferInChanAlphabet chanAlphabet candidate = isInChanAlphabet chanAlphabet (getActOfferChans candidate)

