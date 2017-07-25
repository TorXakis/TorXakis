{-
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
-- ----------------------------------------------------------------------------------------- --
module TxsShow

-- ----------------------------------------------------------------------------------------- --
--
-- Pretty and Formatted Show for TxsDefs 
--
-- ----------------------------------------------------------------------------------------- --

where

import Prelude hiding (id)

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.String.Utils as Utils

import TxsDefs
import SortId
import CstrId
import FuncId
import ProcId
import ChanId
import VarId
import StatId
import PurpId
import GoalId


specialOpChars :: String
specialOpChars  =  "=+-*/\\^<>|@&%"                 -- must be equal to $special in TxsAlex
                                                    -- for use with PShow to show as infix



-- ----------------------------------------------------------------------------------------- --
-- class PShow


class (Show t) => PShow t
  where
    pshow :: t -> String      -- pretty show without formatting
    fshow :: t -> String      -- pretty show with formatting
    fshow    =  pshow         -- for now, no formatting
    pshow    =  show          -- default normal show


-- ----------------------------------------------------------------------------------------- --
-- is special operator to be shown as infix


isSpecialOp :: FuncId -> Bool
isSpecialOp (FuncId nm _ _ _)
  =  not $ null ( nm `List.intersect` specialOpChars )



-- ----------------------------------------------------------------------------------------- --
-- PShow: TxsDefs

instance PShow TxsDefs
  where
    pshow tdefs =  foldl showElem "\n" (TxsDefs.toList tdefs) 
        where
            showElem :: String ->  (Ident, TxsDef) -> String
            showElem s ( _ , DefChan ) = 
                s ++ "\n" 
            showElem s ( _ , DefVar ) = 
                s ++ "\n" 
            showElem s ( _ , DefStat ) = 
                s ++ "\n" 
            showElem s ( _ , DefGoal ) = 
                s ++ "\n" 
            showElem s (IdSort (SortId nm _), DefSort SortDef{} ) = 
                s ++ "\nSORTDEF " ++ nm ++ " ;\n"
            showElem s (IdCstr (CstrId nm _ a srt), DefCstr CstrDef{} ) =
                s ++ "\nCSTRDEF " ++ nm
                             ++ " :: " ++ Utils.join " # " (map pshow a)
                             ++ " -> " ++ pshow srt ++  " ;\n"
            showElem s (IdFunc (FuncId nm _ a srt), DefFunc (FuncDef vids vexp) ) =
                s ++ "\nFUNCDEF " ++ nm
                  ++ " ( " ++ Utils.join "; " [ n ++ " :: " ++ pshow vsrt
                                              | VarId n _ vsrt <- vids
                                              ]
                            ++ " ) "
                  ++ " :: " ++ Utils.join " # " (map pshow a)
                            ++ " -> " ++ pshow srt ++ " ;\n"
                  ++ "  ::=  " ++ pshow vexp ++ " ;\n"
            showElem s (IdProc (ProcId nm _ chans pvars xt), DefProc (ProcDef _ _ bexp) ) =
                s ++ "\nPROCDEF " ++ nm
                  ++ " [ " ++ Utils.join "; "
                                [ n ++ " :: " ++ Utils.join " # " (map pshow srts)
                                | ChanId n _ srts <- chans
                                ]
                            ++ " ] "
                ++ " ( " ++ Utils.join "; " [ n ++ " :: " ++ pshow srt
                                            | VarId n _ srt <- pvars
                                            ] 
                            ++ " ) "
                ++ case xt of
                   { NoExit     -> "NOEXIT\n"
                   ; Exit xsrts -> "EXIT " ++ Utils.join " # " (map pshow xsrts) ++ " \n"
                   ; Hit        -> "HIT\n"
                   }
                ++ "  ::=\n" ++ pshow bexp ++  "\nENDDEF\n"
            showElem s (IdModel (ModelId nm _), DefModel (ModelDef chins chouts _ bexp) ) =
                s ++ "\nMODELDEF " ++ nm ++"  ::=\n"
                    ++ "  CHAN IN   " ++ Utils.join "," (map pshow chins)  ++ "\n"
                    ++ "  CHAN OUT  " ++ Utils.join "," (map pshow chouts) ++ "\n"
                    ++ "  BEHAVIOUR " ++ pshow bexp   ++ "\n"
                    ++ "ENDDEF\n"
            showElem s (IdPurp (PurpId nm _), DefPurp (PurpDef chins chouts _ goals) ) =
                s ++ "\nPURPDEF " ++ nm ++"  ::=\n"
                    ++ "  CHAN IN   " ++ Utils.join "," (map pshow chins)  ++ "\n"
                    ++ "  CHAN OUT  " ++ Utils.join "," (map pshow chouts) ++ "\n"
                    ++ "  BEHAVIOUR " ++ pshow goals   ++ "\n"
                    ++ "ENDDEF\n"
            showElem s (IdMapper (MapperId nm _), DefMapper (MapperDef chins chouts _ bexp) ) =
                s ++ "\nMAPPERDEF " ++ nm ++"  ::=\n"
                    ++ "  CHAN IN   " ++ Utils.join "," (map pshow chins)  ++ "\n"
                    ++ "  CHAN OUT  " ++ Utils.join "," (map pshow chouts) ++ "\n"
                    ++ "  BEHAVIOUR " ++ pshow bexp   ++ "\n"
                    ++ "ENDDEF\n"
            showElem s (IdCnect (CnectId nm _), DefCnect (CnectDef cnecttype conndefs) ) =
                s ++ "\nCNECTDEF " ++ nm ++"  ::=\n"
                    ++ pshow cnecttype ++ "\n"
                    ++ pshow conndefs ++ "\n"
                    ++ "ENDDEF\n"
            showElem _ _ =
                error "illegal list" 

-- ----------------------------------------------------------------------------------------- --
-- PShow: BExpr


instance PShow BExpr
  where
    pshow Stop
      =  " STOP "
    pshow (ActionPref actoff bexp)
      =  pshow actoff ++ "\n"
         ++ "  >->  " ++ "( " ++ pshow bexp ++ " )"
    pshow (Guard cnrs bexp)
      =  case cnrs of
         { []    -> pshow bexp
         ; cnrs' -> "[[ " ++ Utils.join ", " (map pshow cnrs')
                    ++ " ]] =>> \n" ++ "( " ++ pshow bexp ++ " )"
         }
    pshow (Choice bexps)
      =  case bexps of
         { [] -> "STOP\n"
         ; be -> "( " ++ Utils.join (" )\n"++ "##\n( ") (map pshow be) ++ " )"
         }
    pshow (Parallel chans bexps)
      =  case bexps of
         { [] -> "STOP\n"
         ; be -> "( " ++
                  Utils.join (" )\n"++"|[ "++ Utils.join ", " (map pshow chans) ++" ]|\n( ")
                             (map pshow be)
                 ++ " )"
         }
    pshow (Enable bexp1 chofs bexp2)
      =  "( " ++ pshow bexp1 ++ " )\n" ++ ">>>"
         ++ case chofs of
            { []    -> "\n"
            ; _     -> " ACCEPT\n" ++ Utils.join ",\n" (map pshow chofs)
                       ++ "\n" ++ "IN\n"
            }
         ++ "( " ++ pshow bexp2 ++ " )"
    pshow (Disable bexp1 bexp2)
      =  "( " ++ pshow bexp1 ++ " )\n"
         ++ "[>>\n" ++ "( " ++ pshow bexp2 ++ " )"
    pshow (Interrupt bexp1 bexp2)
      =  "( " ++ pshow bexp1 ++ " )\n" ++ "[><\n"
         ++ "( " ++ pshow bexp2 ++ " )"
    pshow (ProcInst pid chans vexps)
      =  pshow pid
         ++ " [" ++ Utils.join "," (map pshow chans) ++ "]"
         ++ " ( " ++ Utils.join ", " (map pshow vexps) ++ " )\n"
    pshow (Hide chans bexp)
      =  "HIDE "
         ++ Utils.join "; " [ n ++ " :: " ++ Utils.join " # " (map pshow srts)
                            | ChanId n _ srts <- chans
                            ] ++ " IN\n"
         ++ pshow bexp ++ "\n"
         ++ "NI\n"
    pshow (ValueEnv ve bexp)
      =  "VALENV "
         ++ Utils.join ";\n"
                   (map (\(k,v) -> pshow k ++ " = " ++ pshow v) (Map.assocs ve))
         ++ " IN " ++ pshow bexp ++ " NI"
    pshow (StAut init' ve trns)
      =  "STAUT " ++ pshow init' ++ "\n"  ++ pshow ve ++ "\n"
           ++ Utils.join ";\n" (map pshow trns)


-- ----------------------------------------------------------------------------------------- --
-- PShow: ActOffer


instance PShow ActOffer
  where
    pshow (ActOffer ofs cnrs)
      =  "{ " ++ Utils.join " | " (map pshow (Set.toList ofs)) ++ "} "
         ++ case cnrs of
            { [] -> ""
            ; cnrs' -> "\n   [[ " ++ Utils.join ", " (map pshow cnrs') ++ " ]]" 
            }


instance PShow Offer
  where
    pshow (Offer chid choffs)
      =  pshow chid ++ concatMap pshow choffs


instance PShow ChanOffer
  where
    pshow (Quest (VarId nm _ vs))
      =  " ? " ++ nm ++ " :: " ++ pshow vs
    pshow (Exclam vexp)
      =  " ! " ++ pshow vexp


-- ----------------------------------------------------------------------------------------- --
-- PShow: ValExpr


instance (PShow v) => PShow (ValExpr v)
  where
    pshow (view -> Vfunc fid vexps)
      =  if isSpecialOp fid
           then
             case vexps of
               [x]   -> pshow fid ++ " " ++ pshow x ++ " "
               [a,b] -> "( " ++ pshow a ++ " " ++ pshow fid ++ " " ++ pshow b ++ " )"
               _     -> error "TXS: Operator should have one or two arguments"
           else
             pshow fid ++ "( " ++ Utils.join ", " (map pshow vexps) ++ " )"
    pshow (view -> Vcstr cid [])
      =  pshow cid
    pshow (view -> Vcstr cid vexps)
      =  pshow cid ++ "(" ++ Utils.join "," (map pshow vexps) ++ ")"
    pshow (view -> Viscstr cid vexp)
      = "is"++ CstrId.name cid ++ "(" ++ pshow vexp ++ ")" 
    pshow (view -> Vaccess cid p vexp)
      = "access "++ CstrId.name cid ++ " " ++ show p ++ " (" ++ pshow vexp ++ ")" -- TODO: use the accessor name?
    pshow (view -> Vconst con)
      =  pshow con
    pshow (view -> Vvar vid)
      =  pshow vid
    pshow (view -> Vite cnrs vexp1 vexp2)
      =  " ( IF " ++ Utils.join ", " (map pshow cnrs) ++ 
         " THEN " ++ pshow vexp1 ++
         " ELSE " ++ pshow vexp2 ++
         " FI )"
    pshow (view -> Venv ve vexp)
      =  " ( LET " ++ pshow ve ++ " IN " ++ pshow vexp ++ " NI )"
    pshow (view -> Vequal vexp1 vexp2)
      =  "( " ++ pshow vexp1 ++ " == " ++ pshow vexp2 ++ " )"
    pshow (view -> Vpredef _ fid vexps)
      =  if isSpecialOp fid
           then
             case vexps of
               [x]   -> pshow fid ++ " " ++ pshow x ++ " " 
               [a,b] -> "( " ++ pshow a ++ " " ++ pshow fid ++ " " ++ pshow b ++ " )"
               _     -> error "TXS: Operator should have one or two arguments"
           else
             pshow fid ++ "( " ++ Utils.join ", " (map pshow vexps) ++ " )"
    pshow (view -> Verror s)
        = "ERROR " ++ s
    pshow _
        = error "pshow: item not in view"


instance PShow Const
  where
    pshow (Cbool b)
      =  show b
    pshow (Cint i)
      =  show i
    pshow (Cstring s)
      =  "\"" ++ s ++ "\""
    pshow (Cregex r)
      = r
    pshow (Cstr cid [])
      = pshow cid
    pshow (Cstr cid a)
      = pshow cid ++ "(" ++ Utils.join "," (map pshow a) ++ ")"
    pshow (Cerror s)
      = "ERROR " ++ s

-- ----------------------------------------------------------------------------------------- --
-- PShow: VarEnv


instance (PShow v, PShow w) => PShow (VarEnv v w)
  where
    pshow venv
      =  Utils.join "\n" [ pshow vid ++ " = " ++ pshow vexp
                         | (vid,vexp) <- Map.toList venv
                         ]


-- ----------------------------------------------------------------------------------------- --
-- PShow: Staut


instance PShow Trans
  where
    pshow (Trans from' actoff update' to')
      = "FROM " ++ pshow from' ++
        "  TO " ++ pshow to' ++ "\n" ++
        "    VIA " ++ pshow actoff ++ "\n" ++
        case Map.toList update' of
        { []          -> "\n"
        ; [(v,e)]     -> pshow v ++ " := " ++
                         pshow e ++ "\n\n"
        ; ups         -> "{ " ++ Utils.join "; " [ pshow v ++ " := " ++ pshow e ++ "\n"
                                                 | (v,e) <- ups
                                                 ]        
                              ++ "} " ++ "\n"
        }


-- ----------------------------------------------------------------------------------------- --
-- PShow: Ident


instance PShow Ident
  where
    pshow = vname

instance PShow ChanId
  where
    pshow = ChanId.name

instance PShow CstrId
  where
    pshow = CstrId.name

instance PShow FuncId
  where
    pshow = FuncId.name

instance PShow GoalId
  where
    pshow = GoalId.name

instance PShow ProcId
  where
    pshow = ProcId.name

instance PShow PurpId
  where
    pshow = PurpId.name
    
instance PShow SortId
  where
    pshow = SortId.name

instance PShow StatId
  where
    pshow = StatId.name
    
instance PShow VarId
  where
    pshow = VarId.name
    
-- ----------------------------------------------------------------------------------------- --
-- ----------------------------------------------------------------------------------------- --


instance PShow ConnDef
  where
     pshow (ConnDtoW  chn _ _ _ _)  =  pshow chn
     pshow (ConnDfroW chn _ _ _ _)  =  pshow chn


instance PShow CnectType
  where
     pshow  =  show


-- ----------------------------------------------------------------------------------------- --
-- PShow: parameterization


instance (PShow t, PShow u) => PShow (t,u)
  where
    pshow (a,b)  =  "( " ++ pshow a ++
                    ", " ++ pshow b ++ " )"


instance (PShow t, PShow u, PShow v) => PShow (t,u,v)
  where
    pshow (a,b,c)  =  "( " ++ pshow a ++
                      ", " ++ pshow b ++
                      ", " ++ pshow c ++ " )"


instance PShow t => PShow [t]
  where
    pshow []    = "[]"
    pshow list  =  "[ " ++ Utils.join " " ( map pshow list ) ++ " ]"
                                 

instance PShow t => PShow (Set.Set t)
  where
    pshow set | Set.null set = "{}"
    pshow set                = "{ " ++ Utils.join " " ( map pshow (Set.toList set) ) ++ " }"


instance PShow t => PShow (Maybe t)
  where
    pshow Nothing   =  ""
    pshow (Just x)  =  pshow x


instance PShow Bool


instance PShow Int


showN :: Int -> Int -> String
showN n p  =  let ns = show n in replicate (p- length ns) '.' ++ ns


-- ----------------------------------------------------------------------------------------- --
--                                                                                           --
-- ----------------------------------------------------------------------------------------- --

