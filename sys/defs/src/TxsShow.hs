{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}
-- ----------------------------------------------------------------------------------------- --
module TxsShow

-- ----------------------------------------------------------------------------------------- --
--
-- Pretty and Formatted Show for TxsDefs
--
-- ----------------------------------------------------------------------------------------- --

where

import           Prelude           hiding (id)

import qualified Data.List         as List
import qualified Data.Map.Strict   as Map
import qualified Data.Set          as Set
import qualified Data.String.Utils as Utils
import qualified Data.Text         as T

import           ChanId
import           CnectId
import           ConstDefs
import qualified FreeMonoidX       as FMX
import           FuncDef
import           FuncId
import           GoalId
import           Ident
import           MapperId
import           ModelId
import           Name
import           ProcId
import           Product
import           PurpId
import           Sort
import           StatId
import           Sum
import           TxsDef
import           TxsDefs
import           ValExpr
import           VarId

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
  =  not $ null ( T.unpack (toText nm) `List.intersect` specialOpChars )

-- | PShow: Sort
instance PShow Sort where
  pshow (SortADT r) = "ADT " ++ show r
  pshow SortError   = "Error" 
  pshow SortBool    = "Bool" 
  pshow SortInt     = "Int" 
  pshow SortChar    = "Char" 
  pshow SortString  = "String" 
  pshow SortRegex   = "Regex" 
  
-- | PShow: TxsDefs
instance PShow TxsDefs where
  pshow tdefs = foldl showElem "\n" (TxsDefs.toList tdefs)
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
      showElem s (_ , DefADT ADTDef{adtName=nm, constructors=cstrDfs}) =
        s ++ "\nTYPEDEF " ++ T.unpack (toText nm) ++ " ::= " ++ show cstrDfs ++ " ENDDEF\n" -- todo: print cstrDfs pretty
      showElem s (IdFunc (FuncId nm _ a srt), DefFunc (FuncDef vids vexp) ) =
        s ++ "\nFUNCDEF " ++ T.unpack (toText nm)
        ++ " ( " ++ Utils.join "; " [ T.unpack (toText n) ++ " :: " ++ pshow vsrt
                                    | VarId n _ vsrt <- vids
                                    ]
        ++ " ) "
        ++ " :: " ++ Utils.join " # " (map pshow a)
        ++ " -> " ++ pshow srt ++ " ;\n"
        ++ "  ::=  " ++ pshow vexp ++ " ;\n"
      showElem s (IdProc (ProcId nm _ chans pvars xt), DefProc (ProcDef _ _ bexp) ) =
        s ++ "\nPROCDEF " ++ T.unpack (toText nm)
        ++ " [ " ++ Utils.join "; "
        [ T.unpack (toText n) ++ " :: " ++ Utils.join " # " (map pshow srts)
        | ChanId n _ srts <- chans
        ]
        ++ " ] "
        ++ " ( " ++ Utils.join "; " [ T.unpack (toText n) ++ " :: " ++ pshow srt
                                    | VarId n _ srt <- pvars
                                    ]
        ++ " ) "
        ++ case xt of
             NoExit     -> "NOEXIT\n"
             Exit xsrts -> "EXIT " ++ Utils.join " # " (map pshow xsrts) ++ " \n"
             Hit        -> "HIT\n"
        ++ "  ::=\n" ++ pshow bexp ++  "\nENDDEF\n"
      showElem s (IdModel (ModelId nm _), DefModel (ModelDef chins chouts _ bexp) ) =
        s ++ "\nMODELDEF " ++ T.unpack (toText nm) ++"  ::=\n"
        ++ "  CHAN IN   " ++ Utils.join "," (map pshow chins)  ++ "\n"
        ++ "  CHAN OUT  " ++ Utils.join "," (map pshow chouts) ++ "\n"
        ++ "  BEHAVIOUR " ++ pshow bexp   ++ "\n"
        ++ "ENDDEF\n"
      showElem s (IdPurp (PurpId nm _), DefPurp (PurpDef chins chouts _ goals) ) =
        s ++ "\nPURPDEF " ++ T.unpack (toText nm) ++"  ::=\n"
        ++ "  CHAN IN   " ++ Utils.join "," (map pshow chins)  ++ "\n"
        ++ "  CHAN OUT  " ++ Utils.join "," (map pshow chouts) ++ "\n"
        ++ "  BEHAVIOUR " ++ pshow goals   ++ "\n"
        ++ "ENDDEF\n"
      showElem s (IdMapper (MapperId nm _), DefMapper (MapperDef chins chouts _ bexp) ) =
        s ++ "\nMAPPERDEF " ++ T.unpack (toText nm) ++"  ::=\n"
        ++ "  CHAN IN   " ++ Utils.join "," (map pshow chins)  ++ "\n"
        ++ "  CHAN OUT  " ++ Utils.join "," (map pshow chouts) ++ "\n"
        ++ "  BEHAVIOUR " ++ pshow bexp   ++ "\n"
        ++ "ENDDEF\n"
      showElem s (IdCnect (CnectId nm _), DefCnect (CnectDef cnecttype conndefs) ) =
        s ++ "\nCNECTDEF " ++ T.unpack (toText nm) ++"  ::=\n"
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
    pshow (Guard c bexp)
      =  "[[ " ++ pshow c ++ " ]] =>> \n" ++ "( " ++ pshow bexp ++ " )"
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
      =  let psBexp2 = pshow bexp2
            in
              "( " ++ pshow bexp1 ++ " )\n" ++ ">>>"
                 ++ case chofs of
                    { []    -> psBexp2 ++ "\n"
                    ; _     -> " ACCEPT\n" ++ Utils.join "\n" (map pshow chofs)
                               ++ "\nIN\n" ++ psBexp2 ++ "\nNI\n"
                    }
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
         ++ Utils.join "; " [ T.unpack (toText n) ++ " :: " ++ Utils.join " # " (map pshow srts)
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
    pshow (ActOffer ofs c)
      =  "{ " ++ Utils.join " | " (map pshow (Set.toList ofs)) ++ "} "
         ++ case view c of
            { Vconst (Cbool True) -> ""
            ; _                   -> "\n   [[ " ++ pshow c ++ " ]]"
            }


instance PShow Offer where
  pshow (Offer chid choffs) = pshow chid ++ concatMap pshow choffs

instance PShow ChanOffer
  where
    pshow (Quest (VarId nm _ vs))
      =  " ? " ++ T.unpack (toText nm) ++ " :: " ++ pshow vs
    pshow (Exclam vexp)
      =  " ! " ++ pshow vexp

-- | PShow: ValExpr
instance PShow v => PShow (ValExpr v) where
    pshow (view -> Vfunc fid vexps)
      =  if isSpecialOp fid
           then
             case vexps of
               [x]   -> pshow fid ++ " " ++ pshow x ++ " "
               [a,b] -> "( " ++ pshow a ++ " " ++ pshow fid ++ " " ++ pshow b ++ " )"
               _     -> error "TXS: Operator should have one or two arguments"
           else
             pshow fid ++ "( " ++ Utils.join ", " (map pshow vexps) ++ " )"
    pshow (view -> Vcstr aRf cRf [])
      =  show aRf ++ "-Cstr:" ++ show cRf -- TODO: new show will have available definitions; pshow will go
    pshow (view -> Vcstr aRf cRf vexps)
      =  show aRf  ++ "-Cstr:" ++ show cRf ++ "(" ++ Utils.join "," (map pshow vexps) ++ ")"
    pshow (view -> Viscstr aRf cRf vexp)
      = "is"++ show aRf  ++ "-Cstr:" ++ show cRf ++ "(" ++ pshow vexp ++ ")"
    pshow (view -> Vaccess aRf cRf p _ vexp)
      =  "access "++ show aRf  ++ "-Cstr:" ++ show cRf ++ " " ++ show p
      ++ " (" ++ pshow vexp ++ ")"
    pshow (view -> Vconst con)
      =  pshow con
    pshow (view -> Vvar vid)
      =  pshow vid
    pshow (view -> Vite cond vexp1 vexp2)
      =  " ( IF " ++ pshow cond ++
         " THEN " ++ pshow vexp1 ++
         " ELSE " ++ pshow vexp2 ++
         " FI )"
    pshow (view -> Vequal vexp1 vexp2)
      =  "( " ++ pshow vexp1 ++ " == " ++ pshow vexp2 ++ " )"
    pshow (view -> Vnot vexp)
      =  "(not (" ++ pshow vexp ++ ") )"
    pshow (view -> Vand vexps)
      =  "(" ++ Utils.join " /\\ " (map pshow (Set.toList vexps)) ++ " )"
    pshow (view -> Vsum s)
      = listShow (FMX.toOccurList s)
      where
        listShow []     = "0"
        listShow [x]    = elemShow x
        listShow (x:xs) = "( " ++ elemShow x ++ " + " ++ listShow xs ++ " )"

        elemShow (t,1)  = pshow t
        elemShow (t,-1) = "(- " ++ pshow t ++ " )"
        elemShow (t,p)  = "( " ++ show p ++ " * " ++ pshow t ++ " )"
    pshow (view -> Vproduct s)
      = listShow (FMX.toDistinctAscOccurListT s)
      where
        listShow []     = "1"
        listShow [x]    = elemShow x
        listShow (x:xs) = "( " ++ elemShow x ++ " * " ++ listShow xs ++ " )"

        elemShow (t,1)  = pshow t
        elemShow (t,p)  | p > 0 = "( " ++ pshow t ++ " ^ "  ++ show p ++ " )"
        elemShow (_,p)  = error ("TxsShow - pshow VExpr - illegal power: p = " ++ show p)

    pshow (view -> Vdivide t n)
      =  "(" ++ pshow t ++ " / " ++ pshow n ++ " )"
    pshow (view -> Vmodulo t n)
      =  "(" ++ pshow t ++ " % " ++ pshow n ++ " )"
    pshow (view -> Vgez v)
      =  "( 0 <= " ++ pshow v ++ " )"
    pshow (view -> Vlength v)
      =  "len( " ++ pshow v ++ " )"
    pshow (view -> Vat s p)
      =  "at( " ++ pshow s ++ ", "++ pshow p ++ " )"
    pshow (view -> Vconcat vexps)
      =  "( " ++ Utils.join " ++ " (map pshow vexps) ++ " )"
    pshow (view -> Vstrinre s r)
      =  "strinre( " ++ pshow s ++ ", "++ pshow r ++ " )"
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
        = "ERROR " ++ show s
    pshow _
        = error "pshow: item not in view"


instance PShow Const where
  pshow (Cbool b) = show b
  pshow (Cint i) = show i
  pshow (Cstring s) = "\"" ++ T.unpack s ++ "\""
  pshow (Cregex r) = show r
  pshow (Cstr aRf cRf []) = show aRf ++ "-Cstr:" ++ show cRf -- TODO: Show cstr name
  pshow (Cstr aRf cRf a) = show aRf ++ "-Cstr:" ++ show cRf ++ "(" ++ Utils.join "," (map pshow a) ++ ")"
  pshow (Cerror s) = "ERROR " ++ s
  pshow (Cany srt) = "(ANY :: " ++ pshow srt ++ ")"

-- |PShow: VarEnv
instance (PShow v, PShow w) => PShow (VarEnv v w)
  where
    pshow venv
      =  Utils.join "\n" [ pshow vid ++ " = " ++ pshow vexp
                         | (vid,vexp) <- Map.toList venv
                         ]

-- | PShow: Staut
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
-- PShow: TxsDef


instance PShow TxsDef
  where
    pshow (DefADT (ADTDef nm cstrs))
      = "CONSTRUCTOR\n" ++
        "      " ++ T.unpack (toText nm)  ++ "\n" ++
        "      " ++ show cstrs  ++ "\n" ++ "\n" -- TODO: show cstrs

    pshow (DefFunc   (FuncDef vids vexp))
      = "FUNCTION\n" ++
        "      " ++ pshow vids  ++ "\n" ++
        "  ::=\n" ++
        "    " ++ fshow vexp ++ "\n"

    pshow (DefProc   (ProcDef chids vids bexp))
      = "PROCESS\n" ++
        "      " ++ pshow chids  ++ "\n" ++
        "      " ++ pshow vids ++ "\n" ++
        "  ::=\n" ++
        "    " ++ fshow bexp ++ "\n"

    pshow (DefModel  (ModelDef insyncs outsyncs splsyncs bexp))
      = "MODEL\n" ++
        "      " ++ pshow insyncs  ++ "\n" ++
        "      " ++ pshow outsyncs ++ "\n" ++
        "      " ++ pshow splsyncs ++ "\n" ++
        "  BEHAVIOUR\n" ++
        "    " ++ fshow bexp ++ "\n"

    pshow (DefPurp   (PurpDef insyncs outsyncs splsyncs goals))
      = "PURPOSE\n" ++
        "      " ++ pshow insyncs  ++ "\n" ++
        "      " ++ pshow outsyncs ++ "\n" ++
        "      " ++ pshow splsyncs ++ "\n" ++
        "  BEHAVIOUR\n" ++
        "    " ++ fshow goals ++ "\n"

    pshow (DefMapper (MapperDef inchids outchids syncs bexp))
      = "MAPPER\n" ++
        "      " ++ pshow inchids  ++ "\n" ++
        "      " ++ pshow outchids ++ "\n" ++
        "      " ++ pshow syncs ++ "\n" ++
        "  BEHAVIOUR\n" ++
        "    " ++ fshow bexp ++ "\n"

    pshow _ = "No PShow for this Definition\n"


-- ----------------------------------------------------------------------------------------- --
-- PShow: Ident

instance PShow Ident where
    pshow (IdADT    r)  =  show r
    pshow (IdFunc   id) =  pshow id
    pshow (IdProc   id) =  pshow id
    pshow (IdChan   id) =  pshow id
    pshow (IdVar    id) =  pshow id
    pshow (IdStat   id) =  pshow id
    pshow (IdModel  id) =  pshow id
    pshow (IdPurp   id) =  pshow id
    pshow (IdGoal   id) =  pshow id
    pshow (IdMapper id) =  pshow id
    pshow (IdCnect  id) =  pshow id

instance PShow ChanId where
  pshow = T.unpack . toText . ChanId.name

instance PShow FuncId where
  pshow = T.unpack . toText . FuncId.name

instance PShow GoalId where
  pshow = T.unpack . toText . GoalId.name

instance PShow ProcId where
  pshow = T.unpack . toText . ProcId.name

instance PShow PurpId where
  pshow = T.unpack . toText . PurpId.name

instance PShow StatId where
  pshow = T.unpack . toText . StatId.name

instance PShow VarId where
  pshow = T.unpack . toText . VarId.name

instance PShow ModelId where
  pshow = T.unpack . toText . ModelId.name

instance PShow MapperId where
  pshow = T.unpack . toText . MapperId.name

instance PShow CnectId where
  pshow = T.unpack . toText . CnectId.name

instance PShow ConnDef
  where
     pshow (ConnDtoW  chn _ _ _ _) =  pshow chn
     pshow (ConnDfroW chn _ _ _ _) =  pshow chn

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
    pshow []   = "[]"
    pshow list =  "[ " ++ Utils.join " " ( map pshow list ) ++ " ]"


instance PShow t => PShow (Set.Set t)
  where
    pshow set | Set.null set = "{}"
    pshow set = "{ " ++ Utils.join " " ( map pshow (Set.toList set) ) ++ " }"


instance PShow t => PShow (Maybe t)
  where
    pshow Nothing  =  ""
    pshow (Just x) =  pshow x

instance (PShow t, PShow u) => PShow (Either t u)
  where
    pshow (Left  x) = pshow x
    pshow (Right x) = pshow x

instance PShow a => PShow (SumTerm a) where
    pshow (SumTerm a) = pshow a
    fshow (SumTerm a) = fshow a

instance PShow a => PShow (ProductTerm a) where
    pshow (ProductTerm a) = pshow a
    fshow (ProductTerm a) = fshow a

instance PShow Bool

instance PShow Int

showN :: Int -> Int -> String
showN n p  =  let ns = show n in replicate (p- length ns) '.' ++ ns

-- ----------------------------------------------------------------------------------------- --
