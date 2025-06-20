{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Expand

-- ----------------------------------------------------------------------------------------- --
--
-- Expansion of Behaviour States for Unfold, and then Primer
--
-- ----------------------------------------------------------------------------------------- --
-- export

( expand         -- :: [ Set.Set TxsDefs.ChanId ] -> CNode -> IOB.IOB CTree
                 -- 'expand chsets cnode' expands 'cnode'
                 -- (concrete, closed, no interaction variables)
                 -- into a communication tree with interaction variables (closed)
                 -- plain expansion, no input/ouput, no solving
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Arrow
import           Control.Monad.Extra
import           Control.Monad.State
import           Data.Either

import qualified Data.List           as List
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T

import           BTree
import           ChanId
import           Constant
import qualified EnvBTree            as IOB
import qualified EnvData
import           Id
import           Relabel             (relabel)
import           StdTDefs
import           Subst
import           TxsDefs
import           TxsUtils
import           Utils
import           ValExpr
import qualified Eval
import           Variable
import           VarId

-- | transfer tuple containing an either to an either containing a tuple
toEitherTuple :: (a, Either String b) -> Either String (a,b)
toEitherTuple (_, Left s)  = Left s
toEitherTuple (a, Right b) = Right (a,b)


-- | Expansion of CNode into communication tree, recursively over CNode
-- structure CNode must be closed and have no free, symbolic, or interaction
-- variables.
expand :: [ Set.Set TxsDefs.ChanId ] -- ^ Set of expected synchronization channels.
       -> CNode
       -> IOB.IOB CTree
-- expand  :  for  BNbexpr WEnv BExpr

-- ----------------------------------------------------------------------------------------- --
expand chsets (BNbexpr we (TxsDefs.view -> ActionPref (ActOffer offs hidvars cnd) bexp))  =  do
     (ctoffs, quests, exclams) <- expandOffers chsets offs
     hvarlist <- sequence [ liftP2 (hvid, uniIVar hvid) | hvid <- Set.toList hidvars ]
     let ivenv = if null $ Set.intersection hidvars (Set.fromList (map fst quests))
                   then Map.fromList $   [ (vid,  cstrVar ivar) | (vid,  ivar) <- quests   ]
                                      ++ [ (hvid, cstrVar hvar) | (hvid, hvar) <- hvarlist ]
                   else error "ERROR: interaction and hidden variables shall be disjoint\n"
         we'   = Map.fromList [ (vid, wal)
                              | (vid, wal) <- Map.toList we
                              , vid `Map.notMember` ivenv
                              ]
     tds <- gets IOB.tdefs
     let exclams' = [ toEitherTuple (ivar, ValExpr.eval (ValExpr.subst (Map.map cstrConst we) (funcDefs tds) vexp) )
                    | (ivar, vexp) <- exclams
                    ]
     case Data.Either.partitionEithers exclams' of
       ([],r) ->    return
                      [ CTpref { ctoffers  = ctoffs
                               , cthidvars = map snd hvarlist
                               , ctpred    = cstrAnd $ Set.fromList
                                               ( compSubst ivenv (funcDefs tds)
                                                   (ValExpr.subst (Map.map cstrConst we') (funcDefs tds) cnd)
                                               : [ cstrEqual (cstrVar ivar) (cstrConst wal)
                                                 | (ivar, wal) <- r
                                                 ]
                                               )
                               , ctnext    = BNbexpr (we',ivenv) bexp
                               }
                      ]
       (s,_)  -> do IOB.putMsgs [ EnvData.TXS_CORE_MODEL_ERROR
                                  $ "Expand: Eval failed in expand - ActionPref" ++ show s ]
                    return []

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (TxsDefs.view -> Guard c bexp))  = do
    tds <- gets IOB.tdefs
    case ValExpr.eval $ ValExpr.subst (Map.map cstrConst we) (funcDefs tds) c of
        Right (Cbool True)  -> expand chsets (BNbexpr we bexp)
        Right (Cbool False) -> return []
        _                   -> do IOB.putMsgs [ EnvData.TXS_CORE_MODEL_ERROR
                                                "Expand: guard does not evaluate to a boolean" ]
                                  return []

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (TxsDefs.view -> Choice bexps))  =
     concat <$> sequence [ expand chsets (BNbexpr we bexp) | bexp <- Set.toList bexps ]

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (TxsDefs.view -> Parallel chans bexps))  =
     expand chsets $ BNparallel chans [ BNbexpr we bexp | bexp <- bexps ]

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (TxsDefs.view -> Enable bexp1 chanoffs bexp2))  =  do
     chanoffs' <- mapM (evalChanOffer we) chanoffs
     case Data.Either.partitionEithers chanoffs' of
        ([], r) -> expand chsets $ BNenable (BNbexpr we bexp1) r (BNbexpr we bexp2)
        (s, _)  -> do IOB.putMsgs [ EnvData.TXS_CORE_MODEL_ERROR
                                    ("Expand: Eval failed in expand - Enable" ++ show s) ]
                      return []

  where

     evalChanOffer :: WEnv VarId -> ChanOffer -> IOB.IOB (Either String ChanOffer)

     evalChanOffer _ (Quest vid) =
          return $ Right (Quest vid)

     evalChanOffer we' (Exclam vexp)  =  do
          tds <- gets IOB.tdefs
          let res = ValExpr.eval $ ValExpr.subst (Map.map cstrConst we') (funcDefs tds) vexp
          return $ right (Exclam . cstrConst) res

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (TxsDefs.view -> Disable bexp1 bexp2))  =
     expand chsets $ BNdisable (BNbexpr we bexp1) (BNbexpr we bexp2)

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (TxsDefs.view -> Interrupt bexp1 bexp2))  =
     expand chsets $ BNinterrupt (BNbexpr we bexp1) (BNbexpr we bexp2)

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (TxsDefs.view -> ProcInst procid@(ProcId nm _ _ _ _) chans vexps))  =  do
     tdefs <- gets IOB.tdefs
     case Map.lookup procid (procDefs tdefs) of
       Just (ProcDef chids vids bexp)
         -> do let chanmap = Map.fromList (zip chids chans)
               wals <- mapM ( Eval.eval . ValExpr.subst (Map.map cstrConst we) (funcDefs tdefs) ) vexps
               case Data.Either.partitionEithers wals of
                    ([], r) -> do let we' = Map.fromList (zip vids r)
                                  expand chsets $ BNbexpr Map.empty (relabel chanmap (Subst.subst (Map.map cstrConst we') (funcDefs tdefs) bexp) )
                    (s, _)  -> do IOB.putMsgs [ EnvData.TXS_CORE_MODEL_ERROR
                                                ("Expand: Eval failed in expand - ProcInst " ++ show s) ]
                                  return []
       _ -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                             ("Expand: Undefined process name: " ++ T.unpack nm)
                           , EnvData.TXS_CORE_SYSTEM_ERROR
                             ("\twhen looking for: " ++ show procid)
                           , EnvData.TXS_CORE_SYSTEM_ERROR
                             ("\tprocess definition map:" ++ show (procDefs tdefs))
                           ]
               return []

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (TxsDefs.view -> Hide chans bexp))  =
     expand chsets $ BNhide chans (BNbexpr we bexp)

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (TxsDefs.view -> ValueEnv venv bexp))  =  do
    tds   <- gets IOB.tdefs
    let we' = [ toEitherTuple (vid, ValExpr.eval (ValExpr.subst (Map.map cstrConst we) (funcDefs tds) vexp))
              | (vid, vexp) <- Map.toList venv
              ]
    case Data.Either.partitionEithers we' of
        ([],r) -> expand chsets $ BNbexpr (combineWEnv we (Map.fromList r)) bexp
        (s,_)  -> do IOB.putMsgs [ EnvData.TXS_CORE_MODEL_ERROR
                                   ("Expand:  Eval failed in expand - ValueEnv " ++ show s) ]
                     return []

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (TxsDefs.view -> StAut ini ve trns))  =  do
    let envwals = Map.fromList [ (vid, wal)
                               | (vid, wal) <- Map.toList we
                               , vid `Map.notMember` ve
                               ]
    tds   <- gets IOB.tdefs
    let vewals = [ toEitherTuple (vid, ValExpr.eval (ValExpr.subst (Map.map cstrConst we) (funcDefs tds) vexp) )
                 | (vid, vexp) <- Map.toList ve
                 ]
    case Data.Either.partitionEithers vewals of
        ([], r) -> concatMapM (expandTrans chsets envwals (Map.fromList r)) [ tr | tr <- trns, from tr == ini ]
        (s,_)   -> do IOB.putMsgs [ EnvData.TXS_CORE_MODEL_ERROR
                                  $ "Expand:  Eval failed in expand - StAut - vewals " ++ show s ++
                                    "\nCheck all your STAUTDEF VARs: Has a value been assigned to each " ++
                                    "variable (either in initialization or earlier steps)?"
                                  ]
                      return []

  where
    expandTrans :: [ Set.Set TxsDefs.ChanId ] -> WEnv VarId -> WEnv VarId -> Trans
                -> IOB.IOB CTree
    expandTrans chsets' envwals stswals (Trans _ (ActOffer offs hidvars cnd) update' to')  =  do
         (ctoffs, quests, exclams) <- expandOffers chsets' offs
         hvarlist <- sequence [ liftP2 (hvid, uniIVar hvid) | hvid <- Set.toList hidvars ]
         let ivenv = if null $ Set.intersection hidvars (Set.fromList (map fst quests))
                       then Map.fromList $   [ (vid,  cstrVar ivar) | (vid,  ivar) <- quests   ]
                                          ++ [ (hvid, cstrVar hvar) | (hvid, hvar) <- hvarlist ]
                       else error "ERROR: interaction and hidden variables shall be disjoint\n"
             we'   = envwals `combineWEnv` stswals
         tds <- gets IOB.tdefs
         let exclams' = [ toEitherTuple (ivar, ValExpr.eval (ValExpr.subst (Map.map cstrConst we') (funcDefs tds) vexp) )
                        | (ivar, vexp) <- exclams
                        ]
         case Data.Either.partitionEithers exclams' of
            ([], r) -> do let we'' = Map.fromList [ (vid, wal)
                                                  | (vid, wal) <- Map.toList we'
                                                  , vid `Map.notMember` ivenv
                                                  ]
                              envwals' = Map.fromList [ (vid, wal)
                                                      | (vid, wal) <- Map.toList envwals
                                                      , vid `Map.notMember` ivenv
                                                      ]
                              ve' = Map.fromList [ (vid, ValExpr.subst (Map.map cstrConst we'') (funcDefs tds) vexp)
                                                 | (vid, vexp) <- Map.toList update'
                                                 ]
                          return
                            [ CTpref { ctoffers  = ctoffs
                                     , cthidvars = map snd hvarlist
                                     , ctpred    = cstrAnd $ Set.fromList
                                                     ( compSubst ivenv (funcDefs tds)
                                                         (ValExpr.subst (Map.map cstrConst we'') (funcDefs tds) cnd)
                                                     : [ cstrEqual (cstrVar ivar) (cstrConst wal)
                                                       | (ivar, wal) <- r
                                                       ]
                                                     )
                                     , ctnext    = BNbexpr (envwals',ivenv) (stAut to' (Map.union ve' ve) trns)
                                     }
                            ]
            (s,_)   -> do IOB.putMsgs [ EnvData.TXS_CORE_MODEL_ERROR
                                      $ "Expand: Eval failed in expand - StAut - exclams " ++ show s ++
                                        "\nCheck all your STAUTDEF VARs: Has a value been assigned to each "++
                                        "variable (either in initialization or earlier steps)?" ]
                          return []

-- ----------------------------------------------------------------------------------------- --
-- expand  :  for genuine BNode
--
-- ----------------------------------------------------------------------------------------- --

expand chsets (BNparallel chans cnodes)  = do
    ctpairs <- sequence [ liftP2 ( cnode, expand chsets cnode ) | cnode <- cnodes ]
    return $ asyncs ctpairs ++ syncs ctpairs
  where
    asyncs ::  [(CNode,CTree)] -> CTree
    asyncs ctpairs
        = [ let ctprefs = map fst trees in
              CTpref ( Set.unions $ map ctoffers ctprefs )
                     ( concatMap cthidvars ctprefs )
                     ( cstrAnd (Set.fromList (map ctpred ctprefs) ) )
                     ( BNparallel chans $ map ctnext ctprefs ++ map (fmap (\we->(we,Map.empty))) nodes )
          | (nodes, trees) <- snd ( foldl allAsyncs ([],[]) (map calcSets ctpairs) )
          -- not (null nodes)            -- handle not synchronizing, but all synchronous events as well
          ]
      where
        calcSets :: (CNode,CTree) -> (CNode, [(CTBranch, (Set.Set ChanId, Bool) )] )
        calcSets (node, ctree) = (node, [ let set = Set.map ctchan ctoffs in
                                               (ctpref, (set, Set.null (set `Set.intersection` chans) ) )
                                        | ctpref@(CTpref ctoffs _ _ _) <- ctree
                                        ])

        allAsyncs ::    ([CNode], [([CNode],[(CTBranch, (Set.Set ChanId, Bool) )] )] )
                     -> (CNode, [(CTBranch, (Set.Set ChanId, Bool) )] )
                     -> ([CNode], [([CNode],[(CTBranch, (Set.Set ChanId, Bool) )] )] )
        allAsyncs (nodes,transitions) (node,branches) =
                        let newTransitions =  map (Control.Arrow.first ((:) node)) transitions
                                             ++
                                                [ (nodes, [branch])
                                                | branch@(_,(_,intersection)) <- branches
                                                , intersection
                                                ]
                                             ++
                                                [ (ns, branch:bs)
                                                | branch@(_,(_,intersection)) <- branches
                                                , intersection
                                                , (ns, bs) <- transitions
                                                , pairwiseNull branch bs
                                                ]
                            in
                                (node:nodes, newTransitions )
            where
                pairwiseNull :: (CTBranch, (Set.Set ChanId, Bool) ) -> [(CTBranch, (Set.Set ChanId, Bool) )] -> Bool
                pairwiseNull (_,(set,_)) list = and [Set.null (set `Set.intersection` elm) | (_,(elm,_)) <- list ]

    syncs ::  [(CNode,CTree)] -> CTree
    syncs ctpairs
        = [ let ctprefs = map fst zips in
              CTpref ( Set.unions $ map ctoffers ctprefs )
                     ( concatMap cthidvars ctprefs )
                     ( cstrAnd (Set.fromList (map ctpred ctprefs)) )
                     ( BNparallel chans $ map ctnext ctprefs )
          | zips <- foldl allPairsMatch [[]] (map (calcSets . snd ) ctpairs)
          ]
      where
        calcSets :: CTree -> [(CTBranch, (Set.Set ChanId, Set.Set ChanId) )]
        calcSets ctree = [ let set = Set.map ctchan ctoffs in
                                   (ctpref, (set, set `Set.intersection` chans))
                         | ctpref@(CTpref ctoffs _ _ _) <- ctree
                         ]

        allPairsMatch :: [[(CTBranch, (Set.Set ChanId, Set.Set ChanId) )]] -> [(CTBranch, (Set.Set ChanId, Set.Set ChanId) )] -> [[(CTBranch, (Set.Set ChanId, Set.Set ChanId) )]]
        allPairsMatch acc branches = [ branch:a | branch <- branches, a <- acc, pairwiseMatch branch a]
            where
                pairwiseMatch :: (CTBranch, (Set.Set ChanId, Set.Set ChanId) ) -> [(CTBranch, (Set.Set ChanId, Set.Set ChanId) )] -> Bool
                pairwiseMatch (_,(si,icsi)) acc' = and [ let mm = si `Set.intersection` sj in
                                                          not (Set.null mm) &&       -- only handle synchronizing events : non-synchronzing events on all channels already handled
                                                          (icsi == mm) && (icsj == mm)
                                                      | (_,(sj, icsj)) <- acc'
                                                      ]

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNenable cnode1 chanoffs cnode2)  =  do
     ctree1      <- expand chsets cnode1
     (_, quests, exclams) <- expandOffer chsets (Offer chanIdExit chanoffs)
     let ivenv = Map.fromList [ (vid, cstrVar ivar) | (vid, ivar) <- quests ]
     let exclams' = [ toEitherTuple (ivar, ValExpr.eval vexp) | (ivar, vexp) <- exclams ]
     case Data.Either.partitionEithers exclams' of
        ([], r) -> do let accpreds = [ cstrEqual (cstrVar ivar) (cstrConst wal) | (ivar, wal) <- r ]
                          (exits, noExits) = List.partition (\(CTpref ctoffs1 _ _ _) -> chanIdExit `Set.member` Set.map ctchan ctoffs1) ctree1
                      leftExits   <- sequence [ hideCTBranch chsets
                                                      (Set.singleton chanIdExit)
                                                      ( CTpref ctoffs1
                                                               cthidvars1
                                                               ( cstrAnd (Set.fromList (ctpreds1:accpreds) ) )
                                                               ( fmap (\we->(we,ivenv)) cnode2 )
                                                      )
                                              | CTpref ctoffs1 cthidvars1 ctpreds1 _ <- exits
                                              ]
                      let leftNoExits = [ CTpref ctoffs1
                                               cthidvars1
                                               ctpreds1
                                               ( BNenable ctnext1
                                                          chanoffs
                                                          ( fmap (\we->(we,Map.empty)) cnode2 )
                                               )
                                        | CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1 <- noExits
                                        ]
                      return $ leftExits ++ leftNoExits
        (s,_)   -> do IOB.putMsgs [ EnvData.TXS_CORE_MODEL_ERROR
                                    ("Expand:  Eval failed in expand - BNenable - exclams " ++ show s) ]
                      return []
-- ----------------------------------------------------------------------------------------- --

expand chsets (BNdisable cnode1 cnode2)  =  do
     ctree1  <- expand chsets cnode1
     ctree2  <- expand chsets cnode2
     let ctree1' = [ CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1
                   | CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1 <- ctree1
                   , chanIdExit `Set.member` Set.map ctchan ctoffs1
                   ]
     let ctree2' = [ CTpref ctoffs1
                            cthidvars1
                            ctpreds1
                            ( BNdisable ctnext1
                                        ( fmap (\we->(we,Map.empty)) cnode2 )
                            )
                   | CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1 <- ctree1
                   , chanIdExit `Set.notMember` Set.map ctchan ctoffs1
                   ]
     return $ ctree1' ++ ctree2' ++ ctree2

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNinterrupt cnode1 cnode2)  =  do
     ctree1  <- expand chsets cnode1
     ctree2  <- expand chsets cnode2
     let ctree1' = [ CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1
                   | CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1 <- ctree1
                   , chanIdExit `Set.member` Set.map ctchan ctoffs1
                   ]
     let ctree2' = [ CTpref ctoffs1
                            cthidvars1
                            ctpreds1
                            ( BNinterrupt ctnext1
                                          ( fmap (\we->(we,Map.empty)) cnode2 )
                            )
                   | CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1 <- ctree1
                   , chanIdExit `Set.notMember` Set.map ctchan ctoffs1
                   ]
     ctree3' <- sequence [ hideCTBranch chsets
                                 (Set.singleton chanIdExit)
                                 ( CTpref ctoffs2
                                          cthidvars2
                                          ctpreds2
                                          ( BNinterrupt ( fmap (\we->(we,Map.empty)) cnode1 )
                                                        ( fmap (\we->(we,Map.empty)) cnode2 )
                                          )
                                 )
                         | CTpref ctoffs2 cthidvars2 ctpreds2 _ <- ctree2
                         , chanIdExit `Set.member` Set.map ctchan ctoffs2
                         ]
     let ctree4' = [ CTpref ctoffs2
                            cthidvars2
                            ctpreds2
                            ( BNenable ctnext2
                                       []
                                       ( BNinterrupt ( fmap (\we->(we,Map.empty)) cnode1 )
                                                     ( fmap (\we->(we,Map.empty)) cnode2 )
                                       )
                            )
                   | CTpref ctoffs2 cthidvars2 ctpreds2 ctnext2 <- ctree2
                   , chanIdExit `Set.notMember` Set.map ctchan ctoffs2
                   ]
     return $ ctree1' ++ ctree2' ++ ctree3' ++ ctree4'

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNhide chans cnode)  =  do
     ctree   <- expand chsets cnode
     mapM (hideCTBranch chsets chans) ctree

expand _ _  = error "not in view"

-- ----------------------------------------------------------------------------------------- --
-- helper functions
--
-- ----------------------------------------------------------------------------------------- --
-- expand Offers


expandOffers :: [ Set.Set TxsDefs.ChanId ]
             -> Set.Set Offer
             -> IOB.IOB ( Set.Set CTOffer, [(VarId,IVar)], [(IVar,VExpr)] )
expandOffers chsets offs  =  do
     ctofftuples <- mapM (expandOffer chsets) (Set.toList offs)
     let ( ctoffs, quests, exclams ) = unzip3 ctofftuples
     return ( Set.fromList ctoffs, concat quests, concat exclams )


expandOffer :: [ Set.Set TxsDefs.ChanId ]
            -> Offer
            -> IOB.IOB ( CTOffer, [(VarId,IVar)], [(IVar,VExpr)] )
expandOffer _chsets (Offer chid choffs)  =  do
     ctchoffs <- mapM (expandChanOffer chid) ( zip choffs [1..(length choffs)] )
     let ( ivars, quests, exclams ) = unzip3 ctchoffs
     return ( CToffer chid ivars, concat quests, concat exclams )


expandChanOffer :: ChanId
                -> (ChanOffer,Int)
                -> IOB.IOB ( IVar, [(VarId,IVar)], [(IVar,VExpr)] )
expandChanOffer chid (choff,pos)  =  do
     curs <- gets IOB.stateid
     case choff of
       Quest  vid  -> do let ivar = IVar { ivname = ChanId.name chid
                                         , ivuid  = ChanId.unid chid
                                         , ivpos  = pos
                                         , ivstat = curs
                                         , ivsrt  = vsort vid
                                         }
                         return ( ivar, [(vid,ivar)], [] )
       Exclam vexp -> do let ivar = IVar { ivname = ChanId.name chid
                                         , ivuid  = ChanId.unid chid
                                         , ivpos  = pos
                                         , ivstat = curs
                                         , ivsrt  = sortOf vexp
                                         }
                         return ( ivar, [], [(ivar,vexp)] )


-- ----------------------------------------------------------------------------------------- --
-- hide channels in CTBranch


hideCTBranch :: [ Set.Set TxsDefs.ChanId ] -> Set.Set ChanId -> CTBranch -> IOB.IOB CTBranch
hideCTBranch _ chans (CTpref ctoffs hidvars pred' next) = do
    tds <- gets IOB.tdefs
    let (hctoffs,vctoffs) = Set.partition ((`elem` chans).ctchan) ctoffs
        hvars             = concatMap ctchoffers (Set.toList hctoffs)
    hvarlist              <- sequence [ liftP2 (hvar, uniHVar hvar) | hvar <- hvars ]
    let hvarmap           = Map.fromList hvarlist
        unihvars          = Map.elems hvarmap
        hvarenv           = Map.map cstrVar hvarmap
        ctnext1'          = let chans' = Set.delete chanIdExit chans
                              in if null chans'
                                   then next
                                   else BNhide chans' next
    return CTpref { ctoffers  = vctoffs
                  , cthidvars = hidvars ++ unihvars
                  , ctpred    = ValExpr.subst hvarenv (funcDefs tds) pred'
                  , ctnext    = let f :: (WEnv VarId, VarEnv VarId IVar) -> (WEnv VarId, VarEnv VarId IVar)
                                    f (we, ivenv) =
                                        (we, Map.map (ValExpr.subst hvarenv (funcDefs tds)) ivenv)
                                 in fmap f ctnext1'
                  }

-- ----------------------------------------------------------------------------------------- --
-- transform IVar/VarId into unique IVar (HVar)

getUnid :: IOB.IOB Id
getUnid = do
    unid'   <- gets IOB.unid
    let newUnid = unid' + 1
    modify $ \env -> env { IOB.unid = newUnid }
    return newUnid

uniHVar :: IVar -> IOB.IOB IVar
uniHVar (IVar ivname' ivuid' ivpos' ivstat' ivsrt')  =  do
     newUnid <- getUnid
     return $ IVar (ivname'<>"$$$"<> (T.pack . show) ivuid') newUnid ivpos' ivstat' ivsrt'


uniIVar :: VarId -> IOB.IOB IVar
uniIVar (VarId nm uid srt)  =  do
     newUnid <- getUnid
     return $ IVar (nm<>"$H$"<>(T.pack . show) uid) newUnid 0 0 srt


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

