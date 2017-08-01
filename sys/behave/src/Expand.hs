{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}


-- ----------------------------------------------------------------------------------------- --
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
, Relabel (..)   -- relabel :: (Map.Map ChanId ChanId) -> e -> e
)

-- ----------------------------------------------------------------------------------------- --
-- import

where

import           Control.Monad.State

import qualified Data.List           as List
import qualified Data.Map            as Map
import qualified Data.Set            as Set

import qualified EnvBTree            as IOB
import qualified EnvData

import           StdTDefs
import           TxsDefs
import           TxsUtils

import           BTree
import qualified Eval                as Eval
import           Utils

import           ChanId

-- ----------------------------------------------------------------------------------------- --
-- expand :  expansion of CNode into communication tree, recursively over CNode structure
--           CNode must be closed and have no free, symbolic, or interaction variables


expand :: [ Set.Set TxsDefs.ChanId ] -> CNode -> IOB.IOB CTree


-- ----------------------------------------------------------------------------------------- --
-- expand  :  for  BNbexpr WEnv BExpr
--
-- ----------------------------------------------------------------------------------------- --

expand _ (BNbexpr _ Stop)  = return []

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (ActionPref (ActOffer offs cnrs) bexp))  =  do
     (ctoffs, quests, exclams) <- expandOffers chsets offs
     let ivenv = Map.fromList [ (vid, cstrVar ivar) | (vid, ivar) <- quests ]
         we'   = Map.fromList [ (vid, wal)
                              | (vid, wal) <- Map.toList we
                              , vid `Map.notMember` ivenv
                              ]
     exclams' <- sequence [ liftP2 ( ivar, Eval.eval (cstrEnv (Map.map cstrConst we) vexp) )
                          | (ivar, vexp) <- exclams
                          ]
     return [ CTpref { ctoffers  = ctoffs
                       , cthidvars = []
                       , ctpreds   = [ compSubst ivenv (walSubst we' vexp) | vexp <- cnrs ]
                                     ++ [ cstrEqual (cstrVar ivar) (cstrConst wal)
                                        | (ivar, wal) <- exclams'
                                        ]
                       , ctnext    = BNbexpr (we',ivenv) bexp
                       }
            ]

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (Guard cnrs bexp))  =  do
     guardVal <- Eval.evalCnrs $ map (cstrEnv (Map.map cstrConst we)) cnrs
     if  guardVal
       then expand chsets (BNbexpr we bexp)
       else return []

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (Choice bexps))  =  do
     expands <- sequence [ expand chsets (BNbexpr we bexp) | bexp <- bexps ]
     return $ concat expands

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (Parallel chans bexps))  =
     expand chsets $ BNparallel chans [ BNbexpr we bexp | bexp <- bexps ]

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (Enable bexp1 chanoffs bexp2))  =  do
     chanoffs' <- mapM (evalChanOffer we) chanoffs
     expand chsets $ BNenable (BNbexpr we bexp1) chanoffs' (BNbexpr we bexp2)

  where

     evalChanOffer :: WEnv VarId -> ChanOffer -> IOB.IOB ChanOffer

     evalChanOffer _ (Quest vid) =
          return $ Quest vid

     evalChanOffer we (Exclam vexp)  =  do
          wal <- Eval.eval $ cstrEnv (Map.map cstrConst we) vexp
          return $ Exclam (cstrConst wal)

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (Disable bexp1 bexp2))  =
     expand chsets $ BNdisable (BNbexpr we bexp1) (BNbexpr we bexp2)

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (Interrupt bexp1 bexp2))  =
     expand chsets $ BNinterrupt (BNbexpr we bexp1) (BNbexpr we bexp2)

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (ProcInst procid chans vexps))  =  do
     tdefs <- gets IOB.tdefs
     case Map.lookup procid (procDefs tdefs) of
       Just (ProcDef chids vids bexp)
         -> do let chanmap = Map.fromList (zip chids chans)
               wals    <- mapM (Eval.eval . cstrEnv (Map.map cstrConst we)) vexps
               let we' = Map.fromList (zip vids wals)
               expand chsets $ BNbexpr we' (relabel chanmap bexp)
       _ -> do IOB.putMsgs [ EnvData.TXS_CORE_SYSTEM_ERROR
                               "Expand: Undefined process name in expand" ]
               return []

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (Hide chans bexp))  =
     expand chsets $ BNhide chans (BNbexpr we bexp)

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (ValueEnv venv bexp))  =  do
     we'  <- sequence [ liftP2 ( vid, Eval.eval (cstrEnv (Map.map cstrConst we) vexp) )
                      | (vid, vexp) <- Map.toList venv
                      ]
     we'' <- return $ Map.fromList we'
     expand chsets $ BNbexpr (combineWEnv we we'') bexp

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNbexpr we (StAut init ve trns))  =  do
     let envwals = Map.fromList [ (vid, wal)
                                | (vid, wal) <- Map.toList we
                                , vid `Map.notMember` ve
                                ]
     vewals  <- sequence [ liftP2 ( vid, Eval.eval (cstrEnv (Map.map cstrConst we) vexp) )
                         | (vid, vexp) <- Map.toList ve
                         ]
     let stswals = Map.fromList vewals
     mapM (expandTrans chsets envwals stswals) [ tr | tr <- trns, from tr == init ]

  where

     expandTrans :: [ Set.Set TxsDefs.ChanId ] -> WEnv VarId -> WEnv VarId -> Trans
                    -> IOB.IOB CTBranch
     expandTrans chsets envwals stswals (Trans from (ActOffer offs cnrs) update to)  =  do
          (ctoffs, quests, exclams) <- expandOffers chsets offs
          let we'   = envwals `combineWEnv` stswals
              ivenv = Map.fromList [ (vid, cstrVar ivar) | (vid, ivar) <- quests ]
          exclams' <- sequence [ liftP2 ( ivar, Eval.eval (cstrEnv (Map.map cstrConst we') vexp) )
                               | (ivar, vexp) <- exclams
                               ]
          let we'' = Map.fromList [ (vid, wal)
                                  | (vid, wal) <- Map.toList we'
                                  , vid `Map.notMember` ivenv
                                  ]
              envwals' = Map.fromList [ (vid, wal)
                                      | (vid, wal) <- Map.toList envwals
                                      , vid `Map.notMember` ivenv
                                      ]
              ve' = Map.fromList [ (vid, walSubst we'' vexp)
                                 | (vid, vexp) <- Map.toList update
                                 ]
          return CTpref { ctoffers  = ctoffs
                          , cthidvars = []
                          , ctpreds   =   [ compSubst ivenv (walSubst we'' vexp)
                                          | vexp <- cnrs
                                          ]
                                       ++ [ cstrEqual (cstrVar ivar) (cstrConst wal)
                                          | (ivar, wal) <- exclams'
                                          ]
                          , ctnext    = BNbexpr (envwals',ivenv) (StAut to ve' trns)
                          }

-- ----------------------------------------------------------------------------------------- --
-- expand  :  for genuine BNode
--
-- ----------------------------------------------------------------------------------------- --

expand chsets (BNparallel chans cnodes)  = do
    let chans'  = Set.fromList $ chanId_Exit : chans
    ctpairs <- sequence [ liftP2 ( cnode, expand chsets cnode ) | cnode <- cnodes ]
    return $ asyncs chans' ctpairs ++ syncs chans' ctpairs
  where
    asyncs ::  Set.Set ChanId -> [(CNode,CTree)] -> CTree
    asyncs chans' ctpairs
        = [ let ctprefs = map fst trees in
              CTpref ( Set.unions $ map ctoffers ctprefs )
                     ( concatMap cthidvars ctprefs )
                     ( concatMap ctpreds ctprefs )
                     ( BNparallel chans $ map ctnext ctprefs ++ map (fmap (\we->(we,Map.empty))) nodes )
          | (nodes, trees) <- snd ( foldl allAsyncs ([],[]) (map (calcSets chans') ctpairs) )
          -- not (null nodes)            -- handle not synchronizing, but all synchronous events as well
          ]
      where
        calcSets :: Set.Set ChanId -> (CNode,CTree) -> (CNode, [(CTBranch, (Set.Set ChanId, Bool) )] )
        calcSets chans (node, ctree) = (node, [ let set = Set.map ctchan ctoffs in
                                                  (ctpref, (set, Set.null (set `Set.intersection` chans) ) )
                                              | ctpref@(CTpref ctoffs _ _ _) <- ctree
                                              ])

        allAsyncs ::    ([CNode], [([CNode],[(CTBranch, (Set.Set ChanId, Bool) )] )] )
                     -> (CNode, [(CTBranch, (Set.Set ChanId, Bool) )] )
                     -> ([CNode], [([CNode],[(CTBranch, (Set.Set ChanId, Bool) )] )] )
        allAsyncs (nodes,transitions) (node,branches) =
                        let newTransitions =  map (\(ns,bs) -> (node:ns, bs)) transitions
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
                pairwiseNull (_,(set,_)) list = and [Set.null (set `Set.intersection` elem) | (_,(elem,_)) <- list ]

    syncs ::  Set.Set ChanId -> [(CNode,CTree)] -> CTree
    syncs chans' ctpairs
        = [ let ctprefs = map fst zips in
              CTpref ( Set.unions $ map ctoffers ctprefs )
                     ( concat $ map cthidvars ctprefs )
                     ( concat $ map ctpreds ctprefs )
                     ( BNparallel chans $ map ctnext ctprefs )
          | zips <- foldl allPairsMatch [[]] (map ( (calcSets chans') . snd ) ctpairs)
          ]
      where
        calcSets :: Set.Set ChanId -> CTree -> [(CTBranch, (Set.Set ChanId, Set.Set ChanId) )]
        calcSets chans ctree = [ let set = Set.map ctchan ctoffs in
                                   (ctpref, (set, set `Set.intersection` chans))
                               | ctpref@(CTpref ctoffs _ _ _) <- ctree
                               ]

        allPairsMatch :: [[(CTBranch, (Set.Set ChanId, Set.Set ChanId) )]] -> [(CTBranch, (Set.Set ChanId, Set.Set ChanId) )] -> [[(CTBranch, (Set.Set ChanId, Set.Set ChanId) )]]
        allPairsMatch acc branches = [ branch:a | branch <- branches, a <- acc, pairwiseMatch branch a]
            where
                pairwiseMatch (_,(si,icsi)) acc = and [ let mm = si `Set.intersection` sj in
                                                          not (Set.null mm) &&       -- only handle synchronizing events : non-synchronzing events on all channels already handled
                                                          (icsi == mm) && (icsj == mm)
                                                      | (_,(sj, icsj)) <- acc
                                                      ]

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNenable cnode1 chanoffs cnode2)  =  do
     ctree1      <- expand chsets cnode1
     (ctoff, quests, exclams) <- expandOffer chsets (Offer chanId_Exit chanoffs)
     ivenv       <- return $ Map.fromList [ (vid, cstrVar ivar) | (vid, ivar) <- quests ]
     exclams'    <- sequence [ liftP2 ( ivar, Eval.eval vexp ) | (ivar, vexp) <- exclams ]
     accpreds    <- return $ [ cstrEqual (cstrVar ivar) (cstrConst wal) | (ivar, wal) <- exclams' ]
     let (exits, noExits) = List.partition (\(CTpref ctoffs1 _ _ _) -> chanId_Exit `Set.member` (Set.map ctchan ctoffs1)) ctree1 in do
         leftExits   <- sequence [ hideCTBranch chsets
                                         [chanId_Exit]
                                         ( CTpref ctoffs1
                                                  cthidvars1
                                                  ( ctpreds1 ++ accpreds )
                                                  ( fmap (\we->(we,ivenv)) cnode2 )
                                         )
                                 | CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1 <- exits
                                 ]
         leftNoExits <- return $ [ CTpref ctoffs1
                                          cthidvars1
                                          ctpreds1
                                          ( BNenable ctnext1
                                                     chanoffs
                                                     ( fmap (\we->(we,Map.empty)) cnode2 )
                                          )
                                 | CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1 <- noExits
                                 ]
         return $ leftExits ++ leftNoExits

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNdisable cnode1 cnode2)  =  do
     ctree1  <- expand chsets cnode1
     ctree2  <- expand chsets cnode2
     ctree1' <- return $ [ CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1
                         | CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1 <- ctree1
                         , chanId_Exit `Set.member` (Set.map ctchan ctoffs1)
                         ]
     ctree2' <- return $ [ CTpref ctoffs1
                                  cthidvars1
                                  ctpreds1
                                  ( BNdisable ctnext1
                                              ( fmap (\we->(we,Map.empty)) cnode2 )
                                  )
                         | CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1 <- ctree1
                         , chanId_Exit `Set.notMember` (Set.map ctchan ctoffs1)
                         ]
     return $ ctree1' ++ ctree2' ++ ctree2

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNinterrupt cnode1 cnode2)  =  do
     ctree1  <- expand chsets cnode1
     ctree2  <- expand chsets cnode2
     ctree1' <- return $ [ CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1
                         | CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1 <- ctree1
                         , chanId_Exit `Set.member` (Set.map ctchan ctoffs1)
                         ]
     ctree2' <- return $ [ CTpref ctoffs1
                                  cthidvars1
                                  ctpreds1
                                  ( BNinterrupt ctnext1
                                                ( fmap (\we->(we,Map.empty)) cnode2 )
                                  )
                         | CTpref ctoffs1 cthidvars1 ctpreds1 ctnext1 <- ctree1
                         , chanId_Exit `Set.notMember` (Set.map ctchan ctoffs1)
                         ]
     ctree3' <- sequence [ hideCTBranch chsets
                                 [chanId_Exit]
                                 ( CTpref ctoffs2
                                          cthidvars2
                                          ctpreds2
                                          ( BNinterrupt ( fmap (\we->(we,Map.empty)) cnode1 )
                                                        ( fmap (\we->(we,Map.empty)) cnode2 )
                                          )
                                 )
                         | CTpref ctoffs2 cthidvars2 ctpreds2 ctnext2 <- ctree2
                         , chanId_Exit `Set.member` (Set.map ctchan ctoffs2)
                         ]
     ctree4' <- return $ [ CTpref ctoffs2
                                  cthidvars2
                                  ctpreds2
                                  ( BNenable ctnext2
                                             []
                                             ( BNinterrupt ( fmap (\we->(we,Map.empty)) cnode1 )
                                                           ( fmap (\we->(we,Map.empty)) cnode2 )
                                             )
                                  )
                         | CTpref ctoffs2 cthidvars2 ctpreds2 ctnext2 <- ctree2
                         , chanId_Exit `Set.notMember` (Set.map ctchan ctoffs2)
                         ]
     return $ ctree1' ++ ctree2' ++ ctree3' ++ ctree4'

-- ----------------------------------------------------------------------------------------- --

expand chsets (BNhide chans cnode)  =  do
     ctree   <- expand chsets cnode
     mapM (hideCTBranch chsets chans) ctree


-- ----------------------------------------------------------------------------------------- --
-- helper functions
--
-- ----------------------------------------------------------------------------------------- --
-- expand Offers


expandOffers :: [ Set.Set TxsDefs.ChanId ] -> (Set.Set Offer) -> IOB.IOB ( Set.Set CTOffer, [(VarId,IVar)], [(IVar,VExpr)] )
expandOffers chsets offs  =  do
     ctofftuples <- mapM (expandOffer chsets) (Set.toList offs)
     ( ctoffs, quests, exclams ) <- return $ unzip3 ctofftuples
     return $ ( Set.fromList ctoffs, concat quests, concat exclams )


expandOffer :: [ Set.Set TxsDefs.ChanId ] -> Offer -> IOB.IOB ( CTOffer, [(VarId,IVar)], [(IVar,VExpr)] )
expandOffer chsets (Offer chid choffs)  =  do
     ctchoffs <- mapM (expandChanOffer chsets chid) ( zip choffs [1..(length choffs)] )
     ( ivars, quests, exclams ) <- return $ unzip3 ctchoffs
     return $ ( CToffer chid ivars, concat quests, concat exclams )


expandChanOffer :: [ Set.Set TxsDefs.ChanId ] -> ChanId -> (ChanOffer,Int) -> IOB.IOB ( IVar, [(VarId,IVar)], [(IVar,VExpr)] )
expandChanOffer chsets chid (choff,pos)  =  do
     curs <- gets IOB.stateid
     case choff of
       Quest  vid  -> do ivar <- return $ IVar { ivname = ChanId.name chid
                                               , ivuid  = ChanId.unid chid
                                               , ivpos  = pos
                                               , ivstat = curs
                                               , ivsrt  = vsort vid
                                               }
                         return $ ( ivar, [(vid,ivar)], [] )
       Exclam vexp -> do ivar <- return $ IVar { ivname = ChanId.name chid
                                               , ivuid  = ChanId.unid chid
                                               , ivpos  = pos
                                               , ivstat = curs
                                               , ivsrt  = sortOf vexp
                                               }
                         return $ ( ivar, [], [(ivar,vexp)] )


-- ----------------------------------------------------------------------------------------- --
-- hide channels in CTBranch


hideCTBranch :: [ Set.Set TxsDefs.ChanId ] -> [ChanId] -> CTBranch -> IOB.IOB CTBranch
hideCTBranch chsets chans (CTpref ctoffs hidvars preds next)  =  do
     (hctoffs,vctoffs) <- return $ Set.partition ((`elem` chans).ctchan) ctoffs
     hvars             <- return $ concat ( map ctchoffers (Set.toList hctoffs) )
     hvarlist          <- sequence [ liftP2 (hvar, uniHVar hvar) | hvar <- hvars ]
     hvarmap           <- return $ Map.fromList hvarlist
     unihvars          <- return $ Map.elems hvarmap
     hvarenv           <- return $ Map.map cstrVar hvarmap
     ctnext1'          <- return $ let chans' = chans \\\ [chanId_Exit]
                                    in if  null chans'
                                         then next
                                         else BNhide chans' next
     return $ CTpref { ctoffers  = vctoffs
                     , cthidvars = hidvars ++ unihvars
                     , ctpreds   = map (partSubst hvarenv) preds
                     , ctnext    = let f (we, ivenv) = (we, Map.map (partSubst hvarenv) ivenv)
                                    in fmap f ctnext1'
                     }


-- ----------------------------------------------------------------------------------------- --
-- relabel


class Relabel e
  where
    relabel :: (Map.Map ChanId ChanId) -> e -> e


instance Relabel BExpr

  where

    relabel chanmap Stop
      =  Stop

    relabel chanmap (ActionPref (ActOffer offs cnrs) bexp)
      =  ActionPref (ActOffer (Set.map (relabel chanmap) offs) cnrs) (relabel chanmap bexp)

    relabel chanmap (Guard cnrs bexp)
      =  Guard cnrs (relabel chanmap bexp)

    relabel chanmap (Choice bexps)
      =  Choice (map (relabel chanmap) bexps)

    relabel chanmap (Parallel chids bexps)
      =  Parallel (map (relabel chanmap) chids) (map (relabel chanmap) bexps)

    relabel chanmap (Enable bexp1 choffs bexp2)
      =  Enable (relabel chanmap bexp1) choffs (relabel chanmap bexp2)

    relabel chanmap (Disable bexp1 bexp2)
      =  Disable (relabel chanmap bexp1) (relabel chanmap bexp2)

    relabel chanmap (Interrupt bexp1 bexp2)
      =  Interrupt (relabel chanmap bexp1) (relabel chanmap bexp2)

    relabel chanmap (ProcInst pid chans vexps)
      =  ProcInst pid (map (relabel chanmap) chans) vexps

    relabel chanmap (Hide chans bexp)
      =  Hide chans (relabel (Map.filterWithKey (\k _->k`notElem`chans) chanmap) bexp)

    relabel chanmap (ValueEnv venv bexp)
      =  ValueEnv venv (relabel chanmap bexp)

    relabel chanmap (StAut stid venv trans)
      =  StAut stid venv (map (relabel chanmap) trans)


instance Relabel Offer
  where
    relabel chanmap (Offer chid choffs)
      =  Offer (relabel chanmap chid) choffs


instance Relabel ChanId
  where
    relabel chanmap chid
      =  Map.findWithDefault chid chid chanmap


instance Relabel Trans
  where
    relabel chanmap (Trans from (ActOffer offs cnrs) venv to)
      =  Trans from (ActOffer (Set.map (relabel chanmap) offs) cnrs) venv to


-- ----------------------------------------------------------------------------------------- --
-- transform IVar into unique IVar (HVar)


uniHVar :: IVar -> IOB.IOB IVar
uniHVar (IVar ivname ivuid ivpos ivstat ivsrt)  =  do
     unid   <- gets IOB.unid
     unid'  <- return $ unid + 1
     modify $ \env -> env { IOB.unid = unid' }
     return $ IVar (ivname++"$$$"++(show ivuid)) unid' ivpos ivstat ivsrt


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

