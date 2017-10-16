{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --
--
--   TorXakis :  Grammar Definition for Happy
--
-- ----------------------------------------------------------------------------------------- --


-- ----------------------------------------------------------------------------------------- --
-- uninterpreted haskell preamble


{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module TxsHappy

( txsParser                             -- exporting
, constdefParser
, funcdefParser
, bexprParser
, chandeclsParser
, prefoffsParser
, vexprParser
, vardeclsParser
, valdefsParser
)

where

import TxsAlex                          -- importing
                                        -- data Token(..), AlexPosn(..)
                                        -- txsLexer :: String --> [Token]
import TxsDefs                          -- types for parseVal main attribute
import ChanId
import CnectId
import CstrId
import FuncId
import GoalId
import MapperId
import ModelId
import ProcId
import PurpId
import SortId
import StatId
import VarId
import FuncTable
import TxsUtils                         -- some utilities on TxsDefs 
import TxsShow                          -- pretty pshow for error messages
import StdTDefs                         -- predefined, standard Txs data types

import qualified Sigs

import qualified Data.List   as List
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified Data.String.Utils as Utils
import Data.Monoid
}


-- ----------------------------------------------------------------------------------------- --
--  happy preamble


%name txsParser       TxsRoot           -- txsParser       :: [Token] -> ( Int, TxsDefs )
%name constdefParser  ExConstDef        -- constdefParser  :: [Token] -> ( Int, TxsDefs )
%name funcdefParser   ExFuncDef         -- funcdefParser   :: [Token] -> ( Int, TxsDefs )
%name bexprParser     ExBehaviourExpr   -- bexprParser     :: [Token] -> ( Int, BExpr )
%name chandeclsParser ExChannelDecls    -- chandeclsParser :: [Token] -> ( Int, [ChanId] )
%name prefoffsParser  ExPrefOfferList   -- prefoffsParser  :: [Token] -> ( Int, Set.Set Offer )
%name vexprParser     ExValExpr         -- vexprParser     :: [Token] -> ( Int, VExpr )
%name vardeclsParser  ExVarDeclList     -- vardeclsParser  :: [Token] -> ( Int, [VarId] )
%name valdefsParser   ExNeValueDefs     -- valdefsParser   :: [Token] -> ( Int, VEnv )

%tokentype { Token }
%error { parseError }

%attributetype           { Attrs a v }
%attribute parseVal      { a }                       -- synthesized: parsed value

%attribute inhNodeUid    { Int }                     -- unique id for each nonterminal
%attribute synMaxUid     { Int }                     -- last unique id for subtree

%attribute synSigs       { (Sigs.Sigs VarId) }                    -- defined Sigs for collection
%attribute inhSigs       { (Sigs.Sigs VarId) }                    -- defined Sigs for usage

%attribute inhDefgSort   {   SortId   }              -- defining sort in a TypeDef
%attribute inhDefCstrId  {   CstrId   }              -- defining CstrId in a TypeDef

%attribute synChanSigs   { [ ChanId ] }              -- locally defined ChanId's for collection
%attribute inhChanSigs   { [ ChanId ] }              -- locally defined ChanId's for usage

%attribute synVarSigs    { [ VarId ] }               -- defined VarId's for collection
%attribute inhVarSigs    { [ VarId ] }               -- defined VarId's for usage
 
%attribute synExpdSort   { [ SortId ] }              -- synth possible sorts of ValExpr
%attribute synExpdSorts  { [ [ SortId ] ] }          -- synth possible sorts of [ValExpr]
%attribute inhSolvSort   {   Maybe SortId   }        -- context-enforced sort of ValExpr
%attribute inhSolvSorts  { [ Maybe SortId ] }        -- context-enforced sorts of [ValExpr]
                                                     -- top level: no context = Nothing
                                                     -- lower level:  Nothing = no solution

%attribute synExitSorts  {   ExitSort }              -- exit sorts of BehaviourExpr

%attribute synStateSigs  { [ StatId ] }              -- defined StatId's for collection
%attribute inhStateSigs  { [ StatId ] }              -- defined StatId's for usage

%attribute synStVarSigs  { [ VarId ] }               -- defined State VarId's for collection
%attribute inhStVarSigs  { [ VarId ] }               -- used State VarId's for usage


-- ----------------------------------------------------------------------------------------- --
-- tokens


%token
    TYPEDEF       { Ttypedef          pos }
    FUNCDEF       { Tfuncdef          pos }
    CONSTDEF      { Tconstdef         pos }
    PROCDEF       { Tprocdef          pos }
    CHANDEF       { Tchandef          pos }
    STAUTDEF      { Tstautdef         pos }
    MODELDEF      { Tmodeldef         pos }
    PURPDEF       { Tpurpdef          pos }
    MAPPERDEF     { Tmapperdef        pos }
    CNECTDEF      { Tcnectdef         pos }
    ENDDEF        { Tenddef           pos }
    GOAL          { Tgoal             pos }
    CHAN          { Tchan             pos }
    IN            { Tin               pos }
    OUT           { Tout              pos }
    SYNC          { Tsync             pos }
    CLIENTSOCK    { Tclientsock       pos }
    SERVERSOCK    { Tserversock       pos }
    HOST          { Thost             pos }
    PORT          { Tport             pos }
    ENCODE        { Tencode           pos }
    DECODE        { Tdecode           pos }
    STATE         { Tstate            pos }
    VAR           { Tvar              pos }
    INIT          { Tinit             pos }
    TRANS         { Ttrans            pos }
    FROM          { Tfrom             pos }
    VIA           { Tvia              pos }
    TO            { Tto               pos }
    VALUE         { Tvalue            pos }
    BEHAVIOUR     { Tbehaviour        pos }
    STOP          { Tstop             pos }
    EXIT          { Texit             pos }
    HIT           { Thit              pos }
    MISS          { Tmiss             pos }
    ACCEPT        { Taccept           pos }
    HIDE          { Thide             pos }
    LET           { Tlet              pos }
    NI            { Tni               pos }
    BEGIN         { Tbegin            pos }
    END           { Tend              pos }
    IF            { Tif               pos }
    THEN          { Tthen             pos }
    ELSE          { Telse             pos }
    FI            { Tfi               pos }
    ISTEP         { Tistep            pos }
    QSTEP         { Tqstep            pos }
    ERROR         { Terror            pos }
    REGEX         { Tregex            pos }
    ANY           { Tany              pos }
    True          { Tbool             posbool True }
    False         { Tbool             posbool  False }
    "->"          { Tarrow            pos }
    "<-"          { Tbarrow           pos }
    "[]"          { Tchoice           pos }
    "##"          { Taltchoice        pos }
    "||"          { Tsynchronization  pos }
    "|||"         { Tinterleaving     pos }
    "|["          { Tleftcommunicate  pos }
    "]|"          { Trightcommunicate pos }
    ">->"         { Tprefix           pos }
    ">>>"         { Tenable           pos }
    "[>>"         { Tdisable          pos }
    "[><"         { Tinterrupt        pos }
    "[["          { Topenpred         pos }
    "]]"          { Tclosepred        pos }
    "["           { Topenlist         pos }
    "]"           { Tcloselist        pos }
    "{"           { Topenbrace        pos }
    "}"           { Tclosebrace       pos }
    "("           { Topenpar          pos }
    ")"           { Tclosepar         pos }
    "::"          { Tsortof           pos }
    "::="         { Tisdef            pos }
    ":="          { Tassign           pos }
    "="           { Tequal            pos }
    "=>>"         { Tguard            pos }
    "|"           { Tbar              pos }
    "?"           { Tquestion         pos }
    "!"           { Texclam           pos }
    "#"           { Tsharp            pos }
    ";"           { Tsemicolon        pos }
    ","           { Tcomma            pos }
    capid         { Tcapid            poscapid    $$ }
    smallid       { Tsmallid          possmallid  $$ }
    operator      { Toperator         posoperator $$ }
    integer       { Tinteger          posinteger  $$ }
    string        { Tstring           posstring   $$ }
    regexval      { Tregexval         posregexval $$ }
    TDEFS         { Ctdefs            $$ }
    SIGS          { Csigs             $$ }
    CHANENV       { Cchanenv          $$ }
    VARENV        { Cvarenv           $$ }
    UNID          { Cunid             $$ }


%% ----------------------------------------------------------------------------------------- --
-- grammar for token variations; quasi terminals, i.e., no Uid


EndDef          -- :: { () }
              : ENDDEF
                {  $$ = ()
                }
--            | {- empty -}
--              {  $$ = ()
--              }
--            | ";"
--              {  $$ = ()
--              }
--            | END
--              {  $$ = ()
--              }

EndIn           -- :: { () }
              : NI
                {  $$ = ()
                }
--            | END
--              {  $$ = ()
--              }

EndIf           -- :: { () }
              : FI
                {  $$ = ()
                }
--            | END
--              {  $$ = ()
--              }


-- ----------------------------------------------------------------------------------------- --
-- happy grammar for syntax and attributes


TxsRoot         -- :: { TxsDefs }
                -- root of Txs grammar; delivers a list of TorXakis definitions
                -- attrs inh : -
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : synSortSigs -> inhSortSigs: defined sorts to usable sorts
                --           : synCstrSigs -> inhCstrSigs: defined constructors to usable ones
                --           : synFuncSigs -> inhFuncSigs: defined functions to usable funcs
                --           : synProcSigs -> inhProcSigs: defined processes to usable procs
                --           : synChanSigs -> inhChanSigs: defined channels to usable procs
                -- constrs   : all defined synSortSigs ++ stdSortSigs shall be unique
                --           : all defined synCstrSigs ++ stdCstrSigs shall be unique
                --           : all defined synFuncSigs ++ stdFuncSigs shall be unique
                --           : all defined synProcSigs shall be unique
                --           : all defined synChanSigs shall be unique
                --           : all defined ModelDefs shall be unique
                --           : all defined PurpDefs shall be unique
                --           : all defined MapperDefs shall be unique
                --           : all defined CnectDefs shall be unique
              : TorXakisDefns
                {  $$.inhNodeUid   = 1000
                ;  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = Sigs.uniqueCombine Sigs.empty { Sigs.sort = stdSortTable
                                                                   , Sigs.func = stdFuncTable
                                                                   }
                                                        $1.synSigs
                ;  $$ = ( ($$.synMaxUid+1), TxsDefs.fromList ( $1 ++ stdTDefs ), $1.inhSigs) 
                ;  where let dbls = doubles [ nm | (IdModel (ModelId nm uid), DefModel modeldef) <- $1 ]
                          in if null dbls then () else
                             error $ "\nTXS0016: " ++
                                     "Double defined models: "++(show dbls)++"\n"
                ;  where let dbls = doubles [ nm | (IdPurp (PurpId nm uid), DefPurp purpdef) <- $1 ]
                          in if null dbls then () else
                             error $ "\nTXS0017: " ++
                                     "Double defined test purposes: "++(show dbls)++"\n"
                ;  where let dbls = doubles [ nm | (IdMapper (MapperId nm uid), DefMapper mapperdef) <- $1 ]
                          in if null dbls then () else
                             error $ "\nTXS0018: " ++
                                     "Double defined mappers: "++(show dbls)++"\n"
                ;  where let dbls = doubles [ nm | (IdCnect (CnectId nm uid), DefCnect sutdef) <- $1 ]
                          in if null dbls then () else
                             error $ "\nTXS0019: " ++
                                     "Double defined connections: "++(show dbls)++"\n"
                ;  where let dbls = doubles (map (TxsDefs.unid . fst) ($1 ++ stdTDefs))
                          in if null dbls then () else
                             error $ "\nTXS ERROR 0010\n"
                ;  where let { tdefs = TxsDefs.fromList ( $1 ++ stdTDefs )
                             ; fids  = checkENDECdefs tdefs
                             }
                          in if null fids then () else
                             error $ "\nTXS0020: " ++
                                     "Function(s) can only be used in EN/DECODING\n" ++
                                     (fshow fids) ++ "\n"
                }

TorXakisDefns   -- :: { [ (Ident,TxsDef) ] }
                -- list of TorXakis definitions with their identifiers
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                --           : inhProcSigs: usable processes
                --           : inhChanSigs: usable channels
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synSortSigs: defined sorts
                --           : synCstrSigs: defined constructors
                --           : synFuncSigs: defined functions
                --           : synProcSigs: defined processes
                --           : synChanSigs: defined channels
                -- mirroring : -
                -- constrs   : -
              : {- empty -}
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synSigs      = Sigs.empty
                ;  $$ = []
                }
              | TorXakisDefns TorXakisDefn
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $2.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $$.synSigs      = Sigs.uniqueCombine $1.synSigs $2.synSigs
                ;  $1.inhSigs      = $$.inhSigs
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$ = $1 ++ $2
                }

TorXakisDefn    -- :: { [ (Ident,TxsDef) ] }
                -- TorXakis definition with its identifier
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                --           : inhProcSigs: usable processes
                --           : inhChanSigs: usable channels
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synSortSigs: defined sorts
                --           : synCstrSigs: defined constructors
                --           : synFuncSigs: defined functions
                --           : synProcSigs: defined processes
                --           : synChanSigs: defined channels
                -- mirroring : 
                -- constrs   : -
              : TypeDefList
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $$.synSigs      = $1.synSigs
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$ = $1
                }
              | FuncDefList
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $$.synSigs      = $1.synSigs
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$ = $1
                }
              | ConstDefList
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $$.synSigs      = $1.synSigs
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$ = $1
                }
              | ProcDefList
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $$.synSigs      = $1.synSigs
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$ = $1
                }
              | StautDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $$.synSigs      = $1.synSigs
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$ = [ $1 ]
                }
              | ChannelDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $$.synSigs      = $1.synSigs
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$ = []
                }
              | ModelDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $$.synSigs      = $1.synSigs
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$ = [ $1 ]
                }
              | PurpDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $$.synSigs      = $1.synSigs
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$ = [ $1 ]
                }
              | MapperDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $$.synSigs      = $1.synSigs
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$ = [ $1 ]
                }
              | CnectDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $$.synSigs      = $1.synSigs
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$ = [ $1 ]
                }

TypeDefList     -- :: { [ (Ident,TxsDef) ] }
                -- definitions of algebraic types
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synSortSigs: defined sorts
                --           : synCstrSigs: defined constructors
                --           : synFuncSigs: (implicitly) defined functions
                -- mirroring : -
                -- constrs   : -
              : TYPEDEF TypeDefs EndDef
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $$.synSigs      = $2.synSigs
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$ = $2
                }

TypeDefs        -- :: { [ (Ident,TxsDef) ] }
                -- definitions of algebraic types
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synSortSigs: defined sorts
                --           : synCstrSigs: constructors
                --           : synFuncSigs: (implicitly) defined functions
                -- mirroring : -
                -- constrs   : -
              : TypeDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $$.synSigs      = $1.synSigs
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$ = $1
                }
              | TypeDefs ";" TypeDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $$.synSigs      = Sigs.uniqueCombine $1.synSigs $3.synSigs
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $$ = $1 ++ $3
                }

TypeDef         -- :: { [ (Ident,TxsDef) ] }
                -- definition of an algebraic type with:
                -- one sort, its constructors, and its implicit functions
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synSortSigs: defined sort
                --           : synCstrSigs: defined constructors
                --           : synFuncSigs: (implicitly) defined selector functions,
                --                          constructor checking functions, and equality
                -- mirroring : -
                -- constrs   : all used sorts shall be defined 
                --           : all constructors shall have unique names
                --           : all fields (implicit functions) shall have unique names 
              : capid "::=" Constructors 
                {  $3.inhNodeUid   = $$.inhNodeUid + 15
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $$.synSigs      = let dsort = SortId $1 $$.inhNodeUid in
                                        Sigs.uniqueCombine  Sigs.empty { Sigs.sort = Map.singleton $1 dsort
                                                                       , Sigs.func = FuncTable (Map.fromList [ (eqName, Map.singleton (Signature [dsort,dsort] sortId_Bool) equalHandler)
                                                                                                        , (neqName, Map.singleton (Signature [dsort,dsort] sortId_Bool) notEqualHandler)
                                                                                                        , (toStringName, Map.singleton (Signature [dsort] sortId_String) (cstrPredef AST (FuncId toStringName ($$.inhNodeUid+3) [dsort] sortId_String) ) )
                                                                                                        , (fromStringName, Map.singleton (Signature [sortId_String] dsort) (cstrPredef ASF (FuncId fromStringName ($$.inhNodeUid+4) [sortId_String] dsort) ) )
                                                                                                        , (toXmlName, Map.singleton (Signature [dsort] sortId_String) (cstrPredef AXT (FuncId toStringName ($$.inhNodeUid+5) [dsort] sortId_String) ) )
                                                                                                        , (fromXmlName, Map.singleton (Signature [sortId_String] dsort) (cstrPredef AXF (FuncId fromStringName ($$.inhNodeUid+6) [sortId_String] dsort) ) )
                                                                                                        ] ) 
                                                                       }
                                                            $3.synSigs  
                ;  $3.inhSigs      = $$.inhSigs
                ;  $3.inhDefgSort  = SortId $1 $$.inhNodeUid
                ;  $$ = $3 ++ [ ( IdSort (SortId $1 $$.inhNodeUid), DefSort (SortDef) ) ]
                } 
                -- unique sort implies unique functions

Constructors    -- :: { [ (Ident,TxsDef) ] }
                -- definition of constructors and implicit functions of an algebraic type
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhDefgSort: defining sort 
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synCstrSigs: defined constructors
                --           : synFuncSigs: (implicitly) defined field selector functions
                -- mirroring : -
                -- constrs   : all used sorts shall be defined
              : {- empty -}
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synSigs      = Sigs.empty
                ;  $$ = []
                }
              | Constructor
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhDefgSort  = $$.inhDefgSort
                ;  $$.synSigs      = $1.synSigs
                ;  $$ = $1
                }
              | Constructors "|" Constructor
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $1.inhDefgSort  = $$.inhDefgSort
                ;  $3.inhDefgSort  = $$.inhDefgSort
                ;  $$.synSigs      = Sigs.uniqueCombine $1.synSigs $3.synSigs
                ;  $$ = $1 ++ $3
                }

Constructor     -- :: { [ (Ident,TxsDef) ] }
                -- definition of one constructor and implicit functions of an algebraic type
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhDefgSort: defining sort
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synCstrSigs: defined constructor
                --           : synFuncSigs: (implicitly) defined field selector functions
                -- mirroring : -
                -- constrs   : all used sorts shall be defined
              : capid FieldList
                {  $2.inhNodeUid   = $$.inhNodeUid + 2*(length $2) + 3
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$.synSigs = let { cas = map snd $2
                                    ; cid = CstrId $1 $$.inhNodeUid cas $$.inhDefgSort
                                    } in Sigs.empty{Sigs.func = FuncTable( Map.fromList $ [($1, Map.singleton (Signature cas $$.inhDefgSort) (cstrHandler cid))
                                                                                     ,("is" <> $1, Map.singleton (Signature [$$.inhDefgSort] sortId_Bool) (iscstrHandler cid))
                                                                                     ]
                                                                                     ++
                                                                                     [ ( nm , Map.singleton (Signature [$$.inhDefgSort] s) (accessHandler cid pos) ) | ((nm,s),pos) <- zip $2 [0..] ]
                                                                    ) 
                                                   }
                ;  $$ = let { cas = map snd $2
                            ; cid = CstrId $1 $$.inhNodeUid cas $$.inhDefgSort
                            ; cfid = FuncId ("is" <> $1) ($$.inhNodeUid+1) [$$.inhDefgSort] sortId_Bool
                            ; x =  VarId "x" ($$.inhNodeUid+2) $$.inhDefgSort
                            ; fs = [ let y = VarId "y" vid $$.inhDefgSort in
                                    (IdFunc (FuncId nm uid [$$.inhDefgSort] s), DefFunc (FuncDef [y] (cstrAccess cid pos (cstrVar y))))
                                   | ((nm,s), pos, uid, vid) <- List.zip4 $2 [0..] [($$.inhNodeUid + 3)..] [($$.inhNodeUid +3 + length $2)..] 
                                   ]
                            }
                            in [ ( IdCstr cid, DefCstr (CstrDef cfid (map (\(IdFunc f,_) -> f) fs) ) ) 
                               , ( IdFunc cfid, DefFunc (FuncDef [x] (cstrIsCstr cid (cstrVar x) ) ) )
                               ]
                               ++
                               fs
                ;  where let dbls = doubles ([$1, "is" <> $1] ++ map fst $2)
                          in if null dbls then () else
                             error $ "\nTXS0803: " ++
                                     "Double defined names: "++(show dbls)++"\n"
                               
                }
                -- TODO: remove addition of constructor and isConstructorFunction: add ADT info to TxsDefs in another way!

FieldList       -- :: { [ (String, SortId) ] }
                -- definition of the fields with implicit functions of an algebraic type
                -- attrs inh : inhNodeUid : unique node identification
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : all used sorts shall be defined 
              : {- empty -}
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ =  []
                }  
              | "{" "}"
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = []
                }
              | "{" Fields "}"
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$ = $2
                }

Fields          -- :: { [ (String,SortId) ] }
                -- definition of the fields with implicit functions of an algebraic type
                -- attrs inh : inhNodeUid : unique node identification
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : all used sorts shall be defined
              : Field
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$ = $1
                }
              | Fields ";" Field
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $$ = $1 ++ $3
                ;  where let dbls = doubles (map fst ($1 ++ $3))
                          in if null dbls then () else
                             error $ "\nTXS0802: " ++
                                     "Double defined fields: "++(show dbls)++"\n"
                }

Field           -- :: { [ (String, SortId) ] }
                -- definition of field(s) of same sort with implicit functions
                -- attrs inh : inhNodeUid : unique node identification
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : used sort shall be defined
              : NeSmallIdList OfSort
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$ = [ (nm, $2) | nm <- $1]
                ;  where let dbls = doubles $1
                          in if null dbls then () else
                             error $ "\nTXS0801: " ++
                                     "Double defined fields: "++(show dbls)++"\n"
                }

FuncDefList     -- :: { [ (Ident,TxsDef) ] }
                -- definitions of prefix- or infix-functions;
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synFuncSigs: defined function
                -- mirroring : -
                -- constrs   : -
              : FUNCDEF FuncDefs EndDef
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$.synSigs      = $2.synSigs
                ;  $$ = $2
                }

FuncDefs        -- :: { [ (Ident,TxsDef) ] }
                -- definitions of prefix- or infix-functions;
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synFuncSigs: defined function
                -- mirroring : -
                -- constrs   : all used sorts and functions shall be defined
              : FuncDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$.synSigs      = $1.synSigs
                ;  $$ = [ $1 ]
                }
              | FuncDefs ";" FuncDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $$.synSigs      = Sigs.uniqueCombine $1.synSigs $3.synSigs
                ;  $$ = $1 ++ [ $3 ]
                }

ExFuncDef       -- :: { ( Int, TxsDef ) }
                -- top-level function definition for external use with multiple parsers
                -- attrs inh : SIGS  : Signatures
                --           : UNID  : unique node identification
                -- constrs   : defined function shall have unique function name 
              : SIGS UNID FuncDef
                { $3.inhSigs      = $1
                ; $3.inhNodeUid   = $2
                ; $$ = ( $3.synMaxUid, TxsDefs.fromList [$3] )
                }
                
FuncDef         -- :: { (Ident,TxsDef) }
                -- definition of a prefix- or infix-function;
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synFuncSigs: defined function
                -- mirroring : -
                -- constrs   : all used sorts and functions shall be defined
                --           : infix-function shall have two operands
                --           : free variables in ValExpr are from FormalVars
                --           : inhSolvSort of ValExpr is determined by OfSort
              : smallid FormalVars OfSort "::=" ValExpr
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $2.synMaxUid + 1
                ;  $5.inhNodeUid   = $3.synMaxUid + 1
                ;  $$.synMaxUid    = $5.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $5.inhSigs      = $$.inhSigs
                ;  $5.inhVarSigs   = $2
                ;  $5.inhSolvSort  = Just $3
                ;  $$.synSigs      = Sigs.empty { Sigs.func = FuncTable (Map.singleton $1 (Map.singleton (Signature (map varsort $2) $3) ( cstrFunc (Map.empty::Map.Map FuncId (FuncDef VarId)) (FuncId $1 $$.inhNodeUid (map varsort $2) $3) )  ) ) }
                ;  $$ = ( IdFunc (FuncId $1 $$.inhNodeUid (map varsort $2) $3), 
                          DefFunc (FuncDef $2 $5) )
                }
              | operator FormalVars OfSort "::=" ValExpr
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $2.synMaxUid + 1
                ;  $5.inhNodeUid   = $3.synMaxUid + 1
                ;  $$.synMaxUid    = $5.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $5.inhSigs      = $$.inhSigs
                ;  $5.inhVarSigs   = $2
                ;  $5.inhSolvSort  = Just $3
                ;  $$.synSigs      = Sigs.empty { Sigs.func = FuncTable (Map.singleton $1 (Map.singleton (Signature (map varsort $2) $3) ( cstrFunc (Map.empty::Map.Map FuncId (FuncDef VarId)) (FuncId $1 $$.inhNodeUid (map varsort $2) $3) ) ) ) }
                ;  $$ = ( IdFunc (FuncId $1 $$.inhNodeUid (map varsort $2) $3), 
                          DefFunc (FuncDef $2 $5) )
                ;  where if (length $2) == 1 || (length $2) == 2 then () else
                         error $ "\nTXS0101: "++
                           "Operator shall have one or two arguments: '"++ show $1 ++"'\n"
                }

ConstDefList    -- :: { [ (Ident,TxsDef) ] }
                -- definitions of constants as nullary functions;
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors 
                --           : inhFuncSigs: usable functions
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synFuncSigs: defined nullary function (constant)
                -- mirroring : -
                -- constrs   : all used sorts and functions shall be defined
              : CONSTDEF ConstDefs EndDef
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$.synSigs      = $2.synSigs
                ;  $$ = $2
                }

ConstDefs       -- :: { [ (Ident,TxsDef) ] }
                -- definitions of constants as nullary functions;
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors 
                --           : inhFuncSigs: usable functions
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synFuncSigs: defined nullary function (constant)
                -- mirroring : -
                -- constrs   : all used sorts and functions shall be defined
                --           : no free variables in ValExpr
                --           : inhSolvSort of ValExpr determined by OfSort
              : ConstDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$.synSigs      = $1.synSigs
                ;  $$ = [ $1 ]
                }
              | ConstDefs ";" ConstDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $$.synSigs      = Sigs.uniqueCombine $1.synSigs $3.synSigs
                ;  $$ = $1 ++ [ $3 ]
                }

ExConstDef      -- :: { ( Int, TxsDef ) }
                -- top-level constant definition for external use with multiple parsers
                -- attrs inh : SIGS  : Signatures
                --           : UNID  : unique node identification
                -- constrs   : defined constant shall have unique function name 
              : SIGS UNID ConstDef
                {  $3.inhSigs      = $1
                ;  $3.inhNodeUid   = $2
                ;  $$ = ( $3.synMaxUid, TxsDefs.fromList [$3] )
                }

ConstDef        -- :: { (Ident,TxsDef) }
                -- definition of a constant as a nullary function;
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors 
                --           : inhFuncSigs: usable functions
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synFuncSigs: defined nullary function (constant)
                -- mirroring : -
                -- constrs   : all used sorts and functions shall be defined
                --           : no free variables in ValExpr
                --           : inhSolvSort of ValExpr determined by OfSort
              : smallid OfSort "::=" ValExpr
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $4.inhNodeUid   = $2.synMaxUid + 1
                ;  $$.synMaxUid    = $4.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $4.inhSigs      = $$.inhSigs
                ;  $4.inhSolvSort  = Just $2
                ;  $$.synSigs      = Sigs.empty { Sigs.func = FuncTable (Map.singleton $1 (Map.singleton (Signature [] $2) ( cstrFunc (Map.empty::Map.Map FuncId (FuncDef VarId)) (FuncId $1 $$.inhNodeUid [] $2) ) ) ) }
                ;  $$ = ( IdFunc (FuncId $1 $$.inhNodeUid [] $2), DefFunc (FuncDef [] $4 ) )
                }

ProcDefList     -- :: { [ (Ident,TxsDef) ] }
                -- definitions of processes;
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors 
                --           : inhFuncSigs: usable functions
                --           : inhProcSigs: usable processes
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synProcSigs: defined processes
                -- mirroring : -
                -- constrs   : all used sorts, functions, and processes shall be defined
              : PROCDEF ProcDefs EndDef
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$.synSigs      = $2.synSigs
                ;  $$ = $2
                }

ProcDefs        -- :: { [ (Ident,TxsDef) ] }
                -- definitions of processes;
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors 
                --           : inhFuncSigs: usable functions
                --           : inhProcSigs: usable processes
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synProcSigs: defined process
                -- mirroring : -
                -- constrs   : all used sorts, functions, and processes shall be defined
              : ProcDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$.synSigs      = $1.synSigs
                ;  $$ = [ $1 ]
                }
              | ProcDefs ";" ProcDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $$.synSigs      = Sigs.combine $1.synSigs $3.synSigs
                ;  $$ = $1 ++ [ $3 ]
                }

ProcDef         -- :: { (Ident,TxsDef) }
                -- definition of a process;
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors 
                --           : inhFuncSigs: usable functions
                --           : inhProcSigs: usable processes
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- attrs syn : synProcSigs: defined process
                -- mirroring : -
                -- constrs   : all used sorts, functions, and processes shall be defined
                --           : free channels in BehaviourExpr from FormalChannels
                --           : free variables in BehaviourExpr from FormalVars
              : Id FormalChannels FormalVars ExitKind "::=" BehaviourExpr
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $2.synMaxUid + 1
                ;  $4.inhNodeUid   = $3.synMaxUid + 1
                ;  $6.inhNodeUid   = $4.synMaxUid + 1
                ;  $$.synMaxUid    = $6.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $4.inhSigs      = $$.inhSigs
                ;  $6.inhSigs      = $$.inhSigs
                ;  $6.inhChanSigs  = $2
                ;  $6.inhVarSigs   = $3
                ;  $$.synSigs      = Sigs.empty { Sigs.pro = [ ProcId $1 $$.inhNodeUid $2 $3 $4 ] }
                ;  $$ = ( IdProc (ProcId $1 $$.inhNodeUid $2 $3 $4), 
                          DefProc (ProcDef $2 $3 $6 ) )
                ;  where if $6.synExitSorts == $4 then () else
                         error $ "\nTXS2242: " ++
                                 "Defined exit kind does not match actual one: "++ show $1 ++"\n"
                }

StautDef        -- :: { (Ident,TxsDef) }
                -- definition of a state automaton;
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors 
                --           : inhFuncSigs: usable functions
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synProcSigs: defined process
                -- mirroring : $$.synProcSigs -> $6.inhProcSigs
                -- constrs   : all used sorts, functions, and processes shall be defined
                --           : free channels in StautItems from FormalChannels
                --           : free variables in StautItems from FormalVars

                --     ;  $7.inhSigs      = $$.inhSigs { pro = [ ProcId $2 $$.inhNodeUid $3 $4 $5 ] } 
                --                                              ++ (pro $$.inhSigs)
                --  PVDL: are global procdefs not available in the stautdef context?? 

              : STAUTDEF Id FormalChannels FormalVars ExitKind "::=" StautItemList EndDef
                {  $3.inhNodeUid   = $$.inhNodeUid + 1
                ;  $4.inhNodeUid   = $3.synMaxUid + 1
                ;  $5.inhNodeUid   = $4.synMaxUid + 1
                ;  $7.inhNodeUid   = $5.synMaxUid + 1
                ;  $$.synMaxUid    = $7.synMaxUid
                ;  $3.inhSigs      = $$.inhSigs
                ;  $4.inhSigs      = $$.inhSigs
                ;  $5.inhSigs      = $$.inhSigs
                ;  $7.inhSigs      = $$.inhSigs { Sigs.pro = [ ProcId $2 $$.inhNodeUid $3 $4 $5 ] } 
                ;  $7.inhChanSigs  = $3
                ;  $7.inhVarSigs   = $4
                ;  $$.synSigs      = Sigs.empty { Sigs.pro = [ ProcId $2 $$.inhNodeUid $3 $4 $5 ] }
                ;  $$ = ( IdProc (ProcId $2 $$.inhNodeUid $3 $4 $5), 
                          DefProc (ProcDef $3 $4 $7) )
                ;  where if $7.synExitSorts == $5 then () else
                         error $ "\nTXS2244: " ++
                                "Defined exit kind does not match actual one: "++ show $2 ++"\n"
                }

ChannelDef      -- :: { [] }
                -- definition of all free channels with sort signatures
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synChanSigs: defined channels
                -- mirroring : -
                -- constrs   : used sorts shall be defined
                --           : all channel names shall be unqiue
              : CHANDEF capid "::=" ChannelDeclList EndDef
                {  $4.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $4.synMaxUid
                ;  $4.inhSigs      = $$.inhSigs
                ;  $$.synSigs      = Sigs.empty { Sigs.chan = $4 }
                ;  $$ = []
                ;  where let dbls = doubles (map ( sig . IdChan ) $4)
                          in if null dbls then () else
                             error $ "\nTXS0536: " ++
                                     "Double defined channels: "++ show dbls ++"\n"
                }

-- ----------------------------------------------------------------------------------------- --
-- model definition

ModelDef        -- :: { (Ident,TxsDef) }
                -- definition of a model
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors 
                --           : inhFuncSigs: usable functions
                --           : inhProcSigs: usable processes
                --           : inhChanSigs: usable channels
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : synChanSigs --> inhChanSigs: used ChanIds to usable ChanIds
                -- constrs   : model-used ChanIds shall have unique names 
                --           : free channels in BehaviourExpr from CHAN IN and CHAN OUT
                --           : channelsets shall be input xor output
                --           : exit sort of behaviour shall be (NO)EXIT
              : MODELDEF capid "::=" CHAN IN ChannelUsedList CHAN OUT ChannelUsedList
                SyncChannels BEHAVIOUR BehaviourExpr EndDef
                {  $6.inhNodeUid   = $$.inhNodeUid + 1
                ;  $9.inhNodeUid   = $6.synMaxUid  + 1
                ;  $10.inhNodeUid  = $9.synMaxUid  + 1
                ;  $12.inhNodeUid  = $10.synMaxUid  + 1
                ;  $$.synMaxUid    = $12.synMaxUid
                ;  $6.inhChanSigs  = Sigs.chan $$.inhSigs
                ;  $9.inhChanSigs  = Sigs.chan $$.inhSigs
                ;  $10.inhChanSigs = $6.synChanSigs ++ $9.synChanSigs
                ;  $12.inhSigs     = $$.inhSigs 
                ;  $12.inhChanSigs = $6.synChanSigs ++ $9.synChanSigs
                ;  $12.inhVarSigs  = []
                ;  $$.synSigs      = Sigs.empty
                ;  $$ = let { insyncs  = [ chset
                                         | chset <- $10
                                         , chset `Set.isSubsetOf` (Set.fromList $6)
                                         ]
                            ; outsyncs = [ chset
                                         | chset <- $10
                                         , chset `Set.isSubsetOf` (Set.fromList $9)
                                         ]
                            ; errsyncs = [ chset
                                         | chset <- $10
                                         , not $ chset `Set.isSubsetOf` (Set.fromList $6)
                                         , not $ chset `Set.isSubsetOf` (Set.fromList $9)
                                         ]
                            ; splsyncs = case $12.synExitSorts of
                                         { NoExit  -> []
                                         ; Exit [] -> [ Set.singleton chanId_Exit ]
                                         ; _       -> error $ "\nTXS0540: Exit-kind " ++
                                                        "in ModelDef shall be EXIT or NOEXIT\n"
                                         }
                            }
                         in if  null errsyncs
                              then ( IdModel (ModelId $2 $$.inhNodeUid)
                                   , DefModel (ModelDef insyncs outsyncs splsyncs $12)
                                   )
                              else error $ "\nTXS0541: "++
                                           "ModelDef Channelset shall be input xor output\n"
                ;  where let dbls = doubles ( map ChanId.name ($6.synChanSigs ++ $9.synChanSigs) )
                          in if null dbls then () else
                             error $ "\nTXS0542: "++
                                     "Double used channels in model: "++(show dbls)++"\n"
                }


-- ----------------------------------------------------------------------------------------- --
-- test purpose definition

PurpDef         -- :: { (Ident,TxsDef) }
                -- definition of a collection (conjunction) of test purposes
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                --           : inhProcSigs: usable processes
                --           : inhChanSigs: usable channels
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : synChanSigs --> inhChanSigs: used ChanIds to usable ChanIds
                -- constrs   : purpdef-defined ChanIds shall have unique names 
                --           : free channels in BehaviourExpr from CHAN IN and CHAN OUT
                --           : channelsets shall be input xor output
              : PURPDEF capid "::=" CHAN IN ChannelUsedList CHAN OUT ChannelUsedList
                SyncChannels TestGoals EndDef
                {  $6.inhNodeUid   = $$.inhNodeUid + 1
                ;  $9.inhNodeUid   = $6.synMaxUid  + 1
                ;  $10.inhNodeUid  = $9.synMaxUid  + 1
                ;  $11.inhNodeUid  = $10.synMaxUid  + 1
                ;  $$.synMaxUid    = $11.synMaxUid
                ;  $6.inhChanSigs  = Sigs.chan $$.inhSigs
                ;  $9.inhChanSigs  = Sigs.chan $$.inhSigs
                ;  $10.inhChanSigs = $6.synChanSigs ++ $9.synChanSigs
                ;  $11.inhSigs     = $$.inhSigs
                ;  $11.inhChanSigs = $6.synChanSigs ++ $9.synChanSigs
                ;  $$.synSigs      = Sigs.empty
                ;  $$ = let { insyncs  = [ chset
                                         | chset <- $10
                                         , chset `Set.isSubsetOf` (Set.fromList $6)
                                         ]
                            ; outsyncs = [ chset
                                         | chset <- $10
                                         , chset `Set.isSubsetOf` (Set.fromList $9)
                                         ]
                            ; errsyncs = [ chset
                                         | chset <- $10
                                         , not $ chset `Set.isSubsetOf` (Set.fromList $6)
                                         , not $ chset `Set.isSubsetOf` (Set.fromList $9)
                                         ]
                            ; splsyncs = [ Set.singleton chanId_Qstep
                                         , Set.singleton chanId_Hit
                                         , Set.singleton chanId_Miss
                                         ]
                            }
                         in if  null errsyncs
                              then ( IdPurp (PurpId $2 $$.inhNodeUid)
                                   , DefPurp (PurpDef insyncs outsyncs splsyncs $11)
                                   )
                              else error $ "\nTXS0546: "++
                                           "PurpDef Channelset shall be input xor output\n"
                ;  where let dbls = doubles ( map ChanId.name ($6.synChanSigs ++ $9.synChanSigs) )
                          in if null dbls then () else
                             error $ "\nTXS0547: "++
                                     "Double used channels in purpose: "++(show dbls)++"\n"
                }

TestGoals       -- :: { [ (GoalId,BExpr) ] }
                -- definition of a collection (conjunction) of test goals
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                --           : inhProcSigs: usable processes
                --           : inhChanSigs: usable channels
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : goal ids shall have unique names 
              : TestGoal
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $$ = [ $1 ]
                }
              | TestGoals TestGoal
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $2.inhNodeUid   = $1.synMaxUid  + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $2.inhSigs      = $$.inhSigs
                ;  $2.inhChanSigs  = $$.inhChanSigs
                ;  $$ = $1 ++ [ $2 ]
                ;  where if (GoalId.name . fst) $2 `notElem` map (GoalId.name . fst) $1 then () else
                         error $ "\nTXS0551: "++ 
                                 "Double defined goal names in test purpose\n"
                }

TestGoal         -- :: { (GoalId,BExpr) }
                -- definition of a test goal
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                --           : inhProcSigs: usable processes
                --           : inhChanSigs: usable channels
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : exit sort of behaviour shall be Hit
              : GOAL Id "::=" BehaviourExpr
                {  $4.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $4.synMaxUid
                ;  $4.inhSigs      = $$.inhSigs
                ;  $4.inhChanSigs  = $$.inhChanSigs
                ;  $4.inhVarSigs   = []
                ;  $$ = ( GoalId $2 $$.inhNodeUid, $4 )
                ;  where if $4.synExitSorts == Hit then () else
                         error $ "\nTXS0550: "++ 
                                 "Exit-kind in Purpose Definition shall be HIT\n"
                }


-- ----------------------------------------------------------------------------------------- --
-- mapper definition

MapperDef       -- :: { (Ident,TxsDef) }
                -- definition of a mapper
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors 
                --           : inhFuncSigs: usable functions
                --           : inhProcSigs: usable processes
                --           : inhChanSigs: usable channels
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : synChanSigs --> inhChanSigs: used ChanIds to usable ChanIds
                -- constrs   : mapper-defined ChanIds shall have unique names 
                --           : free channels in BehaviourExpr from CHAN IN and CHAN OUT
                --           : channelsets shall be input xor output
                --           : exit sort of behaviour shall be NOEXIT
              : MAPPERDEF capid "::=" CHAN IN ChannelUsedList CHAN OUT ChannelUsedList
                SyncChannels BEHAVIOUR BehaviourExpr EndDef
                {  $6.inhNodeUid   = $$.inhNodeUid + 1
                ;  $9.inhNodeUid   = $6.synMaxUid  + 1
                ;  $10.inhNodeUid  = $9.synMaxUid  + 1
                ;  $12.inhNodeUid  = $10.synMaxUid  + 1
                ;  $$.synMaxUid    = $12.synMaxUid
                ;  $6.inhChanSigs  = Sigs.chan $$.inhSigs
                ;  $9.inhChanSigs  = Sigs.chan $$.inhSigs
                ;  $10.inhChanSigs = $6.synChanSigs ++ $9.synChanSigs
                ;  $12.inhSigs     = $$.inhSigs
                ;  $12.inhChanSigs = $6.synChanSigs ++ $9.synChanSigs
                ;  $12.inhVarSigs  = []
                ;  $$.synSigs      = Sigs.empty
                ;  $$ = ( IdMapper (MapperId $2 $$.inhNodeUid)
                        , DefMapper (MapperDef $6 $9 $10 $12)
                        )
                ;  where if $12.synExitSorts == NoExit then () else
                         error $ "\nTXS0555: "++
                                 "Exit-kind in MapperDef shall be NOEXIT\n"
                ;  where let dbls = doubles ( map ChanId.name ($6.synChanSigs ++ $9.synChanSigs) )
                          in if null dbls then () else
                             error  $ "\nTXS0556: "++
                                      "Double used channels in mapper: "++(show dbls)++"\n"
                }


-- ----------------------------------------------------------------------------------------- --
-- cnect connection definition

CnectDef        -- :: { (Ident,TxsDef) }
                -- definition of a connection with endecoding to the outside world
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                --           : inhChanSigs: usable channels
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : used ChanIds shall have unique names
                --           : (host,port) can be used at most once for OUT=ConnDtoW
                --           : (host,port) can be used at most once for IN=ConnDfroW
                --           : each OUT-channel shall have exactly one encoding
                --           : each  IN-channel shall have exactly one decoding
                --
              : CNECTDEF capid "::=" ConnectionType ConnectionItems EndDef
                {  $4.inhNodeUid   = $$.inhNodeUid + 1
                ;  $5.inhNodeUid   = $4.synMaxUid  + 1
                ;  $$.synMaxUid    = $5.synMaxUid
                ;  $5.inhSigs      = $$.inhSigs
                ;  $$.synSigs      = Sigs.empty
                ;  $$ = let { conntows  = [ ConnDtoW chid hsn prn vars vexp
                                          | ConnDtoW chid hsn prn [] (view -> Vconst (Cstring "")) <- $5
                                          , ConnDtoW chid' "" (-1) vars vexp <- $5
                                          , not $ prn == (-1)
                                          , chid == chid'
                                          ]
                            ; connfrows = [ ConnDfroW chid hsn prn var vexps
                                          | ConnDfroW chid hsn prn (VarId "" (-1) srt) [] <- $5
                                          , ConnDfroW chid' "" (-1) var vexps <- $5
                                          , not $ prn == (-1)
                                          , chid == chid'
                                          ]
                            }
                         in ( IdCnect (CnectId $2 $$.inhNodeUid), DefCnect (CnectDef $4 (conntows ++ connfrows) ) )
                ;  where let dbls = doubles $ map ChanId.name $5.synChanSigs
                          in if null dbls then () else
                             error $ "\nTXS0221: "++
                               "Double channels in Connection definition:"++(show dbls)++"\n"
                ;  where let dbls = doubles [ (hs,pr)
                                            | ConnDtoW chid hs pr [] (view -> Vconst (Cstring "")) <- $5
                                            , not $ pr == (-1)
                                            ]
                          in if null dbls then () else 
                             error $ "\nTXS0222: "++ 
                               "Double (hostname,portnr) for OUT: "++(show dbls)++"\n"
                ;  where let dbls = doubles [ (hs,pr)
                                            | ConnDfroW chid hs pr (VarId "" (-1) srt) [] <- $5
                                            , not $ pr == (-1)
                                            ]
                          in if null dbls then () else 
                             error $ "\nTXS0223: "++ 
                               "Double (hostname,portnr) for IN: "++(show dbls)++"\n"
	        ;  where let { towchids  = [ chid
                                           | ConnDtoW chid hs pr [] (view -> Vconst (Cstring "")) <- $5
                                           , not $ pr == (-1)
                                           ]
                             ; encchids  = [ chid
                                           | ConnDtoW chid "" (-1) vars vexp <- $5
                                           ]
                             } in if  ( length towchids == length encchids ) &&
                                      ( Set.fromList towchids == Set.fromList encchids )
                                    then ()
                                    else error $ "\nTXS0224: "++
                                           "No bijection between OUT channels and Encodings\n"
	        ;  where let { frowchids = [ chid
                                           | ConnDfroW chid hs pr (VarId "" (-1) srt) [] <- $5
                                           , not $ pr == (-1)
                                           ]
                             ; decchids  = [ chid
                                           | ConnDfroW chid "" (-1) var vexps <- $5
                                           ]
                             } in if  ( length frowchids == length decchids ) &&
                                      ( Set.fromList frowchids == Set.fromList decchids )
                                    then ()
                                    else error $ "\nTXS0225: "++
                                           "No bijection between IN channels and decodings\n"
                }

ConnectionType  -- :: { CnectType }
                -- type of connection, either server of client on a socket
                -- attrs inh : inhNodeUid : unique node identification
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : -
              : CLIENTSOCK
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = ClientSocket
                }
              | SERVERSOCK
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = ServerSocket
                }

ConnectionItems -- :: { [ ConnDef ] }
                -- delivers Cnect EnDeCodings
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhChanSigs : usable channels
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synChanSigs : used channels
                -- mirroring : -
                -- constrs   : -
              : {- empty -}
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synChanSigs  = []
                ;  $$ = []
                }
              | ConnectionItems ConnectionItem 
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $2.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$.synChanSigs  = $1.synChanSigs ++ $2.synChanSigs
                ;  $$ = $1 ++ [ $2 ]
                }

ConnectionItem  -- :: { ConnDef }
                -- delivers Connection Definitions
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhChanSigs : usable channels
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synChanSigs: used channels
                -- mirroring : -
                -- constrs   : -
              : ConnectionOut
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$.synChanSigs  = $1.synChanSigs
                ;  $$ = $1
                }
              | ConnectionIn
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$.synChanSigs  = $1.synChanSigs
                ;  $$ = $1
                }
              | Encoding
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$.synChanSigs  = []
                ;  $$ = $1
                }
              | Decoding
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$.synChanSigs  = []
                ;  $$ = $1
                }

ConnectionOut   -- :: { ConnChan }
                -- definition of Connection Channel with ChannelId, Host, Portnumber
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhChanSigs: usable channels
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synChanSigs: used channel
                -- mirroring : -
                -- constrs   : one channel in ChannelDecls
              : CHAN OUT ChannelUsedList HOST string PORT integer
                {  $3.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $3.inhChanSigs  = Sigs.chan $$.inhSigs
                ;  $$.synChanSigs  = $3
                ;  $$ = case $3 of
                        { [chid] -> ConnDtoW chid $5 $7 [] (cstrConst (Cstring ""))
                        ; _      -> error "\nTXS0228: Only single channel in out-connection\n"

                        }
                }

ConnectionIn    -- :: { ConnChan }
                -- definition of Connection Channel with ChannelId, Host, Portnumber
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhChanSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synChanSigs: defined channels
                -- mirroring : -
                -- constrs   : one channel in ChannelDecls
              : CHAN IN ChannelUsedList HOST string PORT integer
                {  $3.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $3.inhChanSigs  = Sigs.chan $$.inhSigs
                ;  $$.synChanSigs  = $3
                ;  $$ = case $3 of
                        { [chid] -> ConnDfroW chid $5 $7 (VarId "" (-1) sortId_String) []
                        ; _      -> error "\nTXS0229: Only single channel in connection\n"
                        }
                }

Encoding        -- :: { ConnDef }
                -- delivers a Cnect Encoding for one connection channel
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhChanSigs : usable channels
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                -- mirroring : synVarSigs --> inhVarSigs: defined VarIds to usable VarIds
                -- constrs   : used sorts, constr's, func's, channels, var's shall be defined
                --           : ENCODE: ChannelOffer is one '! VExpr' of sort 'String'
                --           : ENCODE: used channels in NeOfferList IN shall be unique
                --           : ENCODE: variables in NeOfferList shall have unique names
                --           : ENCODE: ChanOffers in NeOfferList are of form 'Quest VarId'
                -- PvdL In Encoding, Offer uses globally defined channels. In Procdef, Offer uses locally defined channels
              : ENCODE Offer "->" ChannelOffer
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $4.inhNodeUid   = $2.synMaxUid + 1
                ;  $$.synMaxUid    = $4.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $4.inhSigs      = $$.inhSigs
                ;  $2.inhChanSigs  = Sigs.chan $$.inhSigs
                ;  $4.inhChanSigs  = Sigs.chan $$.inhSigs
                ;  $2.inhVarSigs   = []
                ;  $4.inhVarSigs   = $2.synVarSigs
                ;  $4.inhSolvSorts = [Just sortId_String]
                ;  $$ = case ( $2, $4 ) of
                        { ( Offer chid choffs, [ Exclam vexp ] ) | sortOf vexp == sortId_String
                            -> ConnDtoW chid "" (-1) [ vid | Quest vid <- choffs ] vexp
                        ; _ -> error $ "\nTXS0231: ENCODE range shall be one '!' of String\n"
                        }
                ;  where let dbls = doubles [ VarId.name v | Quest v <- chanoffers $2 ]
                          in if null dbls then () else
                             error $ "\nTXS0232: ENCODE domain shall have unique variables\n"
                ;  where let excls = [ e | e@(Exclam vexp) <- chanoffers $2 ]
                          in if null excls then () else
                             error $ "\nTXS0233: ENCODE domain shall not have '!'\n"
                }

Decoding        -- :: { ConnDef }
                -- delivers a Cnect Decoding for one connection channel
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhChanSigs : usable channels
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                -- mirroring : synVarSigs --> inhVarSigs: defined VarIds to usable VarIds
                -- constrs   : used sorts, constr's, func's, channels, var's shall be defined
                --           : DECODE: ChannelOffer is one '? VarId' of sort 'String'
                --           : DECODE: ChanOffers in NeOfferList are of form 'Exclam VExpr'
              : DECODE Offer "<-" ChannelOffer
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $4.inhNodeUid   = $2.synMaxUid + 1
                ;  $$.synMaxUid    = $4.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $4.inhSigs      = $$.inhSigs
                ;  $2.inhChanSigs  = Sigs.chan $$.inhSigs
                ;  $4.inhChanSigs  = Sigs.chan $$.inhSigs
                ;  $2.inhVarSigs   = $4.synVarSigs
                ;  $4.inhVarSigs   = []
                ;  $4.inhSolvSorts = [Just sortId_String]
                ;  $$ = case ( $2, $4 ) of
                        { ( Offer chid choffs, [ Quest vid ] ) | sortOf vid == sortId_String
                            -> ConnDfroW chid "" (-1) vid [ vexp | Exclam vexp <- choffs ]
                        ; _ -> error $ "\nTXS0241: DECODE domain shall be one '?' of String\n"
                        }
                ;  where let qstns = [ q | q@(Quest vid) <- chanoffers $2 ]
                          in if null qstns then () else
                             error $ "\nTXS0242: DECODE range shall not have '?'\n"
                }


-- ----------------------------------------------------------------------------------------- --
-- sync channels, exists, sorts, channels, variables

SyncChannels    -- :: { [ Set.Set ChanId ] } 
                -- sets of used channels for synchronized actions
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhChanSigs: usable channels
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : -
              : {- empty -}
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = [ Set.singleton chid | chid <- $$.inhChanSigs ]
                }
              | SYNC ChannelSets
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhChanSigs  = $$.inhChanSigs
                ;  $$ = $2
                }

ChannelSets     -- :: { [ Set.Set ChanId ] }
                -- sets of used channels for synchronized actions
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhChanSigs: usable channels
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : used channels shall be uniquely defined
                --           : channels shall occur only once in a channelset
              : ChannelSet
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $$ = [ $1 ]
                }
              | ChannelSets "," ChannelSet
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid  + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $3.inhChanSigs  = $$.inhChanSigs
                ;  $$ = $1 ++ [ $3 ]
                }

ChannelSet      -- :: { Set.Set ChanId }
                -- set of used channels for synchronized action
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhChanSigs: usable channels
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : used channels shall be uniquely defined
                --           : channels shall occur only once in a channelset
              : "{" NeBarCapIdList "}"
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = let { chans = [ case bindOnName nm (map IdChan $$.inhChanSigs) of
                                        { []   -> error $ "\nTXS0531: "++
                                                  "Undefined sync channel: "++(show nm)++"\n"
                                        ; [IdChan ch] -> ch
                                        ; _    -> error $ "\nTXS0532: "++
                                                  "More sync channels: "++(show nm)++"\n"
                                        }
                                      | nm <- $2
                                      ]
                            ; dbls = doubles chans 
                            }
                         in if  null dbls
                              then Set.fromList chans
                                  else error $ "\nTXS0533: "++
                                               "Double used sync channel: "++(show dbls)++"\n"
                }

ExitKind        -- :: { ExitSort }
                -- exit or hit/miss definition
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : -
              : {- empty -}
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = NoExit
                }
              | EXIT OfSorts
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$ = Exit $2
                }
              | HIT
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = Hit
                }

OfSort          -- :: { SortId }
                -- explicit sort definition
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : sort shall be uniquely defined
              : "::" capid
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = case Map.lookup $2 (Sigs.sort $$.inhSigs) of
                        { Nothing       -> error ("\nTXS0131: " ++
                                                    "Explicit sort not defined: "++(show $2)++"\n")
                        ; Just sid      -> sid
                        }
                }

OfSorts         -- :: { [SortId] }
                -- explicit sortlist definition
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : sorts shall be uniquely defined
              : SharpCapIdList
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = [ case Map.lookup nm (Sigs.sort $$.inhSigs) of
                          { Nothing         -> error ("\nTXS0151: " ++
                                                        "Explicit sort not defined: "++(show nm)++"\n")
                          ; Just sid        -> sid
                          }
                        | nm <- $1
                        ]
                }

NeOfSorts       -- :: { [SortId] }
                -- non-empty explicit sortlist definition
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : sorts shall be uniquely defined
              : NeSharpCapIdList
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = [ case Map.lookup nm (Sigs.sort $$.inhSigs) of
                          { Nothing         -> error ("\nTXS0151: " ++
                                                        "Explicit sort not defined: "++(show nm)++"\n")
                          ; Just sid        -> sid
                          }
                        | nm <- $1
                        ]
                }

FormalChannels  -- :: { [ChanId] }
                -- definition of formal channels with sort signatures
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : defined channel names shall be unique
                --           : used sorts shall be defined
              : "[" ChannelDeclList "]"
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$ = $2
                ;  where let dbls = doubles (map ChanId.name $2)
                          in if null dbls then () else 
                             error ("\nTXS0171: " ++
                                    "Double defined formal channels: "++(show dbls)++"\n")
                }

ChannelDeclList -- :: { [ChanId] }
                -- definition of formal channels with sort signatures
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : used sorts shall be defined
              : {- empty -}
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = []
                }
              | ChannelDecls
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$ = $1
                }
              | ChannelDeclList ";" ChannelDecls 
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $$ = $1 ++ $3
                }

ExChannelDecls  -- :: { ( Int, [ChanId] ) }
                -- definition of (a) formal channel(s) with its sort signature for external use
                -- attrs inh : SIGS  : Signatures
                --           : UNID  : unique node identification
                -- attrs syn : $$: MaxUid: maximum uid in whole subtree
                --           : $$: [VEnv]: locally defined value definitions
              : SIGS UNID ChannelDecls
                {  $3.inhSigs      = $1
                ;  $3.inhNodeUid   = $2
                ;  $$ = ( $3.synMaxUid, $3 )
                }

ChannelDecls    -- :: { [ChanId] }
                -- definition of (a) formal channel(s) with its sort signature
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : used sorts shall be defined
              : NeIdList
                {  $$.synMaxUid    = $$.inhNodeUid + (length $1)
                ;  $$ = [ ChanId nm uid [] | (nm,uid) <- zip $1 [$$.inhNodeUid..] ]
                }
              | NeIdList "::" NeOfSorts
                {  $3.inhNodeUid   = $$.inhNodeUid + (length $1)
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $3.inhSigs      = $$.inhSigs
                ;  $$ = [ ChanId nm uid $3 | (nm,uid) <- zip $1 [$$.inhNodeUid..] ]
                }

FormalVars      -- :: { [VarId] }
                -- definition of formal variables
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : defined variables shall be unique modulo their sort
                --           : used sorts shall be defined
              : "(" ")"
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = []
                }
              | "(" VarDeclList ")"
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$ = $2
                ;  where let dbls = doubles ( map ( sig . IdVar ) $2 )
                          in if null dbls then () else
                             error ("\nTXS0211: " ++
                                    "Double defined formal variables: "++(show dbls)++"\n")
                }

ExVarDeclList   -- :: { ( Int, [VarId] ) }
                -- definition of formal variables for external use with multiple parsers
                -- attrs inh : SIGS  : Signatures
                --           : UNID  : unique node identification
                -- attrs syn : $$: MaxUid: maximum uid in whole subtree
                --           : $$: [VEnv]: locally defined value definitions
              : SIGS UNID VarDeclList
                {  $3.inhSigs      = $1
                ;  $3.inhNodeUid   = $2
                ;  $$ = ( $3.synMaxUid, $3 )
                }

VarDeclList     -- :: { [VarId] }
                -- definition of formal variables
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : used sorts shall be defined
              : VarDecls
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$ = $1
                }
              | VarDeclList ";" VarDecls
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs  = $$.inhSigs
                ;  $3.inhSigs  = $$.inhSigs
                ;  $$ = $1 ++ $3
                }

VarDecls        -- :: { [VarId] }
                -- definition of formal variables
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : used sorts shall be defined
              : NeSmallIdList OfSort
                {  $2.inhNodeUid   = $$.inhNodeUid + (length $1)
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$ = [ VarId nm uid $2 | (nm,uid) <- zip $1 [$$.inhNodeUid..] ]
                }

VarDecl         -- :: { VarId }
                -- definition of formal variable
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : used sorts shall be defined
              : smallid OfSort
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$ = VarId $1 $$.inhNodeUid $2
                }

-- ----------------------------------------------------------------------------------------- --
-- Behaviour expressions

ExBehaviourExpr -- :: { ( Int, BExpr ) }
                -- top-level behaviour expression for external use with multiple parsers
                -- attrs inh : VARENV: variable declarations environment
                --           : UNID  : unique node identification
                -- attrs syn : $$: MaxUid: maximum uid in whole subtree
              : SIGS CHANENV VARENV UNID BehaviourExpr
                {  $5.inhSigs      = $1
                ;  $5.inhChanSigs  = $2
                ;  $5.inhVarSigs   = $3
                ;  $5.inhNodeUid   = $4
                ;  $$ = ( $5.synMaxUid, $5 )
                }

BehaviourExpr   -- :: { BExpr }
                -- top-level behaviour expression
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhProcSigs : usable processes
                --           : inhChanSigs : usable channels
                --           : inhVarSigs  : usable variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                -- attrs syn : synExitSorts: exit sorts
                -- mirroring : -
                -- constrs   : -
              : BehaviourExpr1
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = $1.synExitSorts
                ;  $$ = $1
                }

BehaviourExpr1  -- :: { BExpr }
                -- behaviour expression for enable >>>, disable [>>, and interrupt [><
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhProcSigs : usable processes
                --           : inhChanSigs : usable channels
                --           : inhVarSigs  : usable variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synExitSorts: exit sorts
                -- mirroring : -
                -- constrs   : exit sorts must match
              : BehaviourExpr1 ">>>" BehaviourExpr2
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $3.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = if $1.synExitSorts == Exit []
                                       then $3.synExitSorts
                                       else error ("\nTXS2231: " ++
                                                   "Exit does not match in Enable\n")
                ;  $$ = Enable $1 [] $3
                }
              | BehaviourExpr1 ">>>" ACCEPT ChannelOffList IN BehaviourExpr2 EndIn
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $4.inhNodeUid   = $1.synMaxUid + 1
                ;  $6.inhNodeUid   = $4.synMaxUid + 1
                ;  $$.synMaxUid    = $6.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $4.inhSigs      = $$.inhSigs
                ;  $6.inhSigs      = $$.inhSigs
                ;  $4.inhSolvSorts = case $1.synExitSorts of
                                     { Exit sids -> [ Just sid | sid <- sids ]
                                     ; _         -> error $ "\nTXS2232: " ++
                                                            "No-exit process in Enable\n"
                                     }
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $6.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $4.inhVarSigs   = $$.inhVarSigs
                ;  $6.inhVarSigs   = map (\(IdVar v) -> v ) $ scopeMerge (map IdVar $$.inhVarSigs) (map IdVar $4.synVarSigs)
                ;  $$.synExitSorts = $6.synExitSorts
                ;  $$ = Enable $1 $4 $6
                }
              | BehaviourExpr1 "[>>" BehaviourExpr2
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $3.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = $1.synExitSorts <<+>> $3.synExitSorts
                ;  $$ = Disable $1 $3
                }
              | BehaviourExpr1 "[><" BehaviourExpr2
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $3.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = if $3.synExitSorts == Exit []
                                       then $1.synExitSorts
                                       else error ("\nTXS2233: " ++
                                                   "Exit sorts do not match in Interrupt\n")
                ;  $$ = Interrupt $1 $3
                }
              | BehaviourExpr2
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = $1.synExitSorts
                ;  $$ = $1
                }

BehaviourExpr2  -- :: { BExpr }
                -- behaviour expression for parallel '||', '|||', and '|[..]|'
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhProcSigs : usable processes
                --           : inhChanSigs : usable channels
                --           : inhVarSigs  : usable variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synExitSorts: exit sorts
                -- mirroring : -
                -- constrs   : -
              : BehaviourExpr2 "||" BehaviourExpr3
                {  $1.inhNodeUid   = $$.inhNodeUid + 2
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $3.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = $1.synExitSorts <<->> $3.synExitSorts
                ;  $$ = case $1 of
                        { Parallel chids bexps
                            -> if (Set.fromList chids) == (Set.fromList $$.inhChanSigs)
                                 then Parallel $$.inhChanSigs (bexps ++ [$3])
                                 else Parallel $$.inhChanSigs [$1,$3]
                        ; _ -> Parallel $$.inhChanSigs [$1,$3]
                        }
                }
              | BehaviourExpr2 "|||" BehaviourExpr3
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $3.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = $1.synExitSorts <<->> $3.synExitSorts
                ;  $$ = case $1 of
                        { Parallel [] bexps -> Parallel [] (bexps ++ [$3])
                        ; _                 -> Parallel [] [$1,$3]
                        }
                }
              | BehaviourExpr2 "|[" IdList "]|" BehaviourExpr3
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $5.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $5.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $5.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $5.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $5.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = $1.synExitSorts <<->> $5.synExitSorts
                ;  $$ = let chans = [ case bindOnName nm (map IdChan $$.inhChanSigs) of
                                      { []              -> error ("\nTXS0301: "++
                                                                    "Channel not defined: "++(show nm)++"\n")
                                      ; [IdChan chid]   -> chid
                                      ; _               -> error ("\nTXS0302: "++
                                                                    "Channel double defined: "++(show nm)++"\n")
                                      }
                                    | nm <- $3
                                    ]
                         in case $1 of
                            { Parallel chids bexps
                                -> if (Set.fromList chids) == (Set.fromList chans)
                                     then Parallel chans (bexps ++ [$5])
                                     else Parallel chans [$1,$5]
                            ; _ -> Parallel chans [$1,$5]
                            }
                }
              | BehaviourExpr3
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = $1.synExitSorts
                ;  $$ = $1
                }  

BehaviourExpr3  -- :: { BExpr }
                -- behaviour expression for choice '[]'
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhProcSigs : usable processes
                --           : inhChanSigs : usable channels
                --           : inhVarSigs  : usable variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synExitSorts: exit sorts
                -- mirroring : -
                -- constrs   : -
              : BehaviourExpr3 "##" BehaviourExpr4
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $3.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = $1.synExitSorts <<+>> $3.synExitSorts
                ;  $$ = Choice [$1,$3]
                }  
              | BehaviourExpr4
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $$.synExitSorts = $1.synExitSorts
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$ = $1
                }

BehaviourExpr4  -- :: { BExpr }
                -- behaviour expression for guard, action prefix, process insatntiation,
                -- stop, let, hide, and bracketing
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhProcSigs : usable processes
                --           : inhChanSigs : usable channels
                --           : inhVarSigs  : usable variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synExitSorts: exit sorts
                -- mirroring : synExpdSorts -> inhSolvSorts: must be 'Bool' for guard
                -- constrs   : ValueExprs in NeValExprs must have sort 'Bool' for guard
              : "[[" NeValExprs "]]" "=>>" BehaviourExpr4
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $5.inhNodeUid   = $2.synMaxUid + 1
                ;  $$.synMaxUid    = $5.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $5.inhSigs      = $$.inhSigs
                ;  $2.inhSolvSorts = [ if sortId_Bool `elem` sids
                                         then Just sortId_Bool
                                         else error  ("\nTXS0312: " ++
                                                      "Sort of guard must be 'Bool'\n")
                                     | sids <- $2.synExpdSorts
                                     ]
                ;  $5.inhChanSigs  = $$.inhChanSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $5.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = $5.synExitSorts
                ;  $$ = Guard (cstrAnd (Set.fromList $2)) $5
                }
              | PrefOfferList ">->" BehaviourExpr4
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $3.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = map (\(IdVar v) -> v ) $ scopeMerge (map IdVar $$.inhVarSigs) (map IdVar $1.synVarSigs)
                ;  $$.synExitSorts = $1.synExitSorts <<+>> $3.synExitSorts
                ;  $$ = ActionPref (ActOffer $1 (cstrConst (Cbool True))) $3
                }
              | PrefOfferList "[[" NeValExprs "]]" ">->" BehaviourExpr4
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $6.inhNodeUid   = $3.synMaxUid + 1
                ;  $$.synMaxUid    = $6.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $6.inhSigs      = $$.inhSigs
                ;  $3.inhSolvSorts = [ if sortId_Bool `elem` sids
                                         then Just sortId_Bool
                                         else error  ("\nTXS0313: " ++
                                                      "Sort of constraint must be 'Bool'\n")
                                     | sids <- $3.synExpdSorts
                                     ]
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $6.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = map (\(IdVar v) -> v ) $ scopeMerge (map IdVar $$.inhVarSigs) (map IdVar $1.synVarSigs)
                ;  $6.inhVarSigs   = map (\(IdVar v) -> v ) $ scopeMerge (map IdVar $$.inhVarSigs) (map IdVar $1.synVarSigs)
                ;  $$.synExitSorts = $1.synExitSorts <<+>> $6.synExitSorts
                ;  $$ = ActionPref (ActOffer $1 (cstrAnd (Set.fromList $3))) $6
                }
              | PrefOfferList
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = $1.synExitSorts
                ;  $$ = ActionPref (ActOffer $1 (cstrConst (Cbool True))) Stop
                }
              | PrefOfferList "[[" NeValExprs "]]"
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid  + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $3.inhSolvSorts = [ if sortId_Bool `elem` sids
                                         then Just sortId_Bool
                                         else error  ("\nTXS0313: " ++
                                                      "Sort of constraint must be 'Bool'\n")
                                     | sids <- $3.synExpdSorts
                                     ]
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = map (\(IdVar v) -> v ) $ scopeMerge (map IdVar $$.inhVarSigs) (map IdVar $1.synVarSigs)
                ;  $$.synExitSorts = $1.synExitSorts
                ;  $$ = ActionPref (ActOffer $1 (cstrAnd (Set.fromList $3))) Stop
                }
              | STOP
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synExitSorts = NoExit
                ;  $$ = Stop
                }
              | Id ActualChannels ActualValExprs
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $2.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $2.inhChanSigs  = $$.inhChanSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $3.inhSolvSorts
                     = let actsorts = [ map varsort vids
                                      | ProcId nm uid chids vids exs <- Sigs.pro $$.inhSigs
                                      , nm == $1
                                      , map chansorts chids == map chansorts $2.synChanSigs
                                      , length vids == length $3.synExpdSorts
                                      , and [ (varsort v) `elem` r
                                            | (v,r) <- zip vids $3.synExpdSorts
                                            ]
                                      ]
                        in if (length actsorts) == 1
                             then [ Just argsort | argsort <- head actsorts ]
                             else error $ "\nTXS0321: " ++
                                          "Process not uniquely resolved: "++(show $1)++"\n"
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts
                     = let extsorts = [ exs
                                      | ProcId nm uid chids vids exs <- Sigs.pro $$.inhSigs
                                      , nm == $1
                                      , map chansorts chids == map chansorts $2.synChanSigs
                                      , length vids == length $3.synExpdSorts
                                      , and [ (varsort v) `elem` r
                                            | (v,r) <- (zip vids $3.synExpdSorts)
                                            ]
                                      ]
                        in if (length extsorts) == 1
                             then head extsorts
                             else error $ "\nTXS0322: " ++
                                          "Process not uniquely resolved: "++(show $1)++"\n"
                ;  $$ = let ppids = [ pid
                                    | pid@(ProcId nm uid chids vids exs) <- Sigs.pro $$.inhSigs
                                    , nm == $1
                                    , map chansorts chids == map chansorts $2
                                    , length vids == length $3.synExpdSorts
                                    , map varsort vids == map sortOf $3
                                    ]
                         in case ppids of
                            { []    -> error $ "\nTXS0323: "++
                                               "Process not resolved: "++ show $1 ++"\n" ++
                                               "Processes with the same name are\n* " ++ 
                                               Utils.join "\n* " (map show [pid | pid@(ProcId nm _ _ _ _) <- Sigs.pro $$.inhSigs
                                                                                , nm == $1 ])
                            ; [pid] -> ProcInst pid $2 $3
                            ; _     -> error $ "\nTXS0324: "++ "Process "++
                                               "not uniquely resolved: "++ show $1 ++"\n"  ++
                                               "Possible processes are\n* " ++ 
                                               Utils.join "\n* " (map show ppids)
                            }
                }
              | LET NeValueDefList IN BehaviourExpr1 EndIn
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $4.inhNodeUid   = $2.synMaxUid + 1
                ;  $$.synMaxUid    = $4.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $4.inhSigs      = $$.inhSigs
                ;  $4.inhChanSigs  = $$.inhChanSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $4.inhVarSigs   = map (\(IdVar v) -> v ) $ scopeMerge (map IdVar $$.inhVarSigs) (map IdVar $2.synVarSigs)
                ;  $$.synExitSorts = $4.synExitSorts
                ;  $$ = foldr ValueEnv $4 $2
                }
              | HIDE FormalChannels IN BehaviourExpr1 EndIn
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $4.inhNodeUid   = $2.synMaxUid + 1
                ;  $$.synMaxUid    = $4.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $4.inhSigs      = $$.inhSigs
                ;  $4.inhChanSigs  = map (\(IdChan c) -> c ) $ scopeMerge (map IdChan $$.inhChanSigs) (map IdChan $2)
                ;  $4.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = $4.synExitSorts
                ;  $$ = Hide $2 $4
                }
              | "(" BehaviourExpr1 ")"
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $2.inhChanSigs  = $$.inhChanSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = $2.synExitSorts
                ;  $$ = $2
                }

ActualChannels  -- :: { [ChanId] }
                -- actual channel parameter list to substitute for FormalChannels
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhChanSigs: usable channels
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synChanSigs: actual channels
                -- mirroring : -
                -- constrs   : used channels shall be uniquely defined
                --           : used channels shall be unique 
              : "[" ChannelUsedList "]"
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhChanSigs  = $$.inhChanSigs
                ;  $$.synChanSigs  = $2.synChanSigs
                ;  $$ = $2
                }

ChannelUsedList -- :: { [ChanId] }
                -- used channel list
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhChanSigs: usable channels
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synChanSigs: actual channels
                -- mirroring : -
                -- constrs   : used channels shall be uniquely defined
                --           : used channels shall be unique 
              : IdList
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synChanSigs
                     = [ case bindOnName nm (map IdChan $$.inhChanSigs) of
                         { []               -> error $ "\nTXS0331: "++
                                                        "Undefined used channel: "++(show nm)++"\n"
                         ; [IdChan chid]    -> chid
                         ; _                -> error $ "\nTXS0332: "++
                                                        "Double defined used channel: "++(show nm)++"\n"
                         }
                       | nm <- $1
                       ]
                ;  $$ = $$.synChanSigs
                ;  where let dbls = doubles $1
                          in if null dbls then () else
                                error $ "\nTXS0330: " ++
                                        "Double used channels: "++(show dbls)++"\n"
                }

ActualValExprs  -- :: { [VExpr] }
                -- actual value expressions to substitute for FormalVars
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhVarSigs  : usable variables
                --           : inhSolvSorts: uniquely resolved sorts of value expressions
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synExpdSorts: bottom-up potential sorts of value expressions
                -- mirroring : -
                -- constrs   : used sorts, functions, and vars shall be defined
              : "(" ValExprs ")"
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$.synExpdSorts = $2.synExpdSorts
                ;  $2.inhSolvSorts = $$.inhSolvSorts
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $$ = $2
                }

ExPrefOfferList -- :: { ( Int, Set.Set Offer ) }
                -- PrefOfferList for external use with multiple parsers
                -- attrs inh : SIGS
                --           : CHANENV
                --           : VARENV: variable declarations environment
                --           : UNID  : unique node identification
                -- attrs syn : $$: MaxUid: maximum uid in whole subtree
                --           : $$: Set.Set Offer: parsed offers
                -- constrs   :  ChanOffers are of form 'Exclam VExpr'               
              : SIGS CHANENV VARENV UNID PrefOfferList
                {  $5.inhSigs      = $1
                ;  $5.inhChanSigs  = $2
                ;  $5.inhVarSigs   = $3
                ;  $5.inhNodeUid   = $4
                ;  $$ = ( $5.synMaxUid, $5 )
                }

PrefOfferList   -- :: { Set.Set Offer }
                -- top-level set of Offers, ie, set of (Offer ChanId [ChanOffer])
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhChanSigs : usable channels
                --           : inhVarSigs  : usable variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synVarSigs  : defined variables in offers
                --           : synExitSorts: exit sorts
                -- mirroring : -
                -- constrs   : used channels shall be unique
                --           : defined variables shall be unique modulo their sort
              : ISTEP
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synVarSigs   = []
                ;  $$.synExitSorts = NoExit
                ;  $$ = Set.empty
                }
              | QSTEP
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synVarSigs   = []
                ;  $$.synExitSorts = Hit
                ;  $$ = Set.singleton $ Offer { chanid     = chanId_Qstep
                                              , chanoffers = []
                                              }
                }
              | HIT
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synVarSigs   = []
                ;  $$.synExitSorts = Hit
                ;  $$ = Set.singleton $ Offer { chanid     = chanId_Hit
                                              , chanoffers = []
                                              }
                }
              | MISS
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synVarSigs   = []
                ;  $$.synExitSorts = Hit
                ;  $$ = Set.singleton $ Offer { chanid     = chanId_Miss
                                              , chanoffers = []
                                              }
                }
              | NeOfferList
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$.synVarSigs   = $1.synVarSigs
                ;  $$.synExitSorts = $1.synExitSorts
                ;  $$ = ( Set.fromList $1 )::(Set.Set Offer)
                ;  where let dbls = doubles ( map ChanId.name $1.synChanSigs )
                          in if null dbls then () else
                             error ("\nTXS0351: " ++
                                    "Double used channels in action: "++(show dbls)++"\n")
                ;  where let dbls = doubles ( map ( sig . IdVar ) $1.synVarSigs )
                          in if null dbls then () else
                             error ("\nTXS0352: "++
                               "Double defined variables in action : "++(show dbls)++"\n")
                }
              | "{" OfferList "}"
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $2.inhChanSigs  = $$.inhChanSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $$.synVarSigs   = $2.synVarSigs
                ;  $$.synExitSorts = $2.synExitSorts
                ;  $$ = ( Set.fromList $2 )::(Set.Set Offer)
                ;  where let dbls = doubles ( map ChanId.name $2.synChanSigs )
                          in if null dbls then () else
                             error ("\nTXS0355: " ++
                                    "Double used channels in action: "++(show dbls)++"\n")
                ;  where let dbls = doubles ( map ( sig . IdVar ) $2.synVarSigs )
                          in if null dbls then () else
                             error ("\nTXS0356: "++
                               "Double defined variables in action: "++(show dbls)++"\n")
                }

OfferList       -- :: { [Offer] }
                -- delivers a list of Offer, ie, list of (Offer ChanId [ChanOffer])
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhChanSigs : usable channels
                --           : inhVarSigs  : usable variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synChanSigs : used channels for checking uniqueness
                --           : synVarSigs  : defined variables in offers
                --           : synExitSorts: exit sorts
                -- mirroring : - 
                -- constrs   : -
              : {- empty -}
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synChanSigs  = []
                ;  $$.synVarSigs   = []
                ;  $$.synExitSorts = NoExit
                ;  $$ = []
                }
              | NeOfferList
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $$.synChanSigs  = $1.synChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$.synVarSigs   = $1.synVarSigs
                ;  $$.synExitSorts = $1.synExitSorts
                ;  $$ = $1
                }

NeOfferList     -- :: { [Offer] }
                -- delivers a list of Offer, ie, list of (Offer ChanId [ChanOffer])
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhChanSigs : usable channels
                --           : inhVarSigs  : usable variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synChanSigs : used channels for checking uniqueness
                --           : synVarSigs  : defined variables in offers
                --           : synExitSorts: exit sorts
                -- mirroring : -
                -- constrs   : -
              : Offer
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $$.synChanSigs  = $1.synChanSigs 
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$.synVarSigs   = $1.synVarSigs
                ;  $$.synExitSorts = $1.synExitSorts
                ;  $$ = [ $1 ]
                }
              | NeOfferList "|" Offer
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $3.inhChanSigs  = $$.inhChanSigs
                ;  $$.synChanSigs  = $1.synChanSigs ++ $3.synChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $$.synVarSigs   = $1.synVarSigs ++ $3.synVarSigs
                ;  $$.synExitSorts = $1.synExitSorts <<+>> $3.synExitSorts
                ;  $$ = $1 ++ [ $3 ]
                }

Offer           -- :: { Offer }
                -- delivers an Offer, ie, (Offer ChanId [ChanOffer])
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhChanSigs : usable channels
                --           : inhVarSigs  : usable variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synChanSigs : used channels for checking uniqueness
                --           : synVarSigs  : defined variables in offers
                --           : synExitSorts: exit sorts
                -- mirroring : -
                -- constrs   : -
              : EXIT ChannelOffList
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $2.inhSolvSorts = [ Nothing | s <- $2.synExpdSorts ]
                ;  $$.synChanSigs  = [ chanId_Exit ]
                ;  $$.synVarSigs   = $2.synVarSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = Exit (map sortOf $2)
                ;  $$ = Offer chanId_Exit $2
                }
              | Id ChannelOffList
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $2.inhSolvSorts = case bindOnName $1 (map IdChan $$.inhChanSigs) of
                                     { []               -> error ("\nTXS0371: "++ 
                                                                    "Used gate not defined: "++
                                                                    (show $1)++"\n")
                                     ; [IdChan chid]    -> let cs = chansorts chid
                                                            in if (length cs) == (length $2.synExpdSorts)
                                                                then [ Just sid | sid <- cs ]
                                                                else error ("\nTXS0372: "++
                                                                                "Number of gate sorts does "++
                                                                                "not match: "++(show $1)++"\n")
                                     ; _                -> error ("\nTXS0373: "++
                                                                   "Used gate double defined: "++
                                                                   (show $1)++"\n")
                                     }
                ;  $$.synChanSigs  = case bindOnName $1 (map IdChan $$.inhChanSigs) of
                                     { []               -> error ("\nTXS0375: "++
                                                                   "Used gate not defined: "++
                                                                   (show $1)++"\n")
                                     ; [IdChan chid]    -> [chid]
                                     ; _                -> error ("\nTXS0376: "++
                                                                   "Used gate double defined: "++
                                                                   (show $1)++"\n")
                                     }
                ;  $$.synVarSigs   = $2.synVarSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExitSorts = NoExit
                ;  $$ = Offer (head $$.synChanSigs) $2
                }

ChannelOffList  -- :: { [ChanOffer] }
                -- delivers a list of ChanOffer, ie, ?- and !- offers
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhVarSigs  : usable variables
                --           : inhSolvSorts: sorts expected according to channel sorts
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synExpdSorts: number sorts according to channel offer
                --                           only used to determine the length of inhSolvSorts
                --           : synVarSigs  : defined variables in offers
                -- mirroring : -
                -- constrs   : -
              : {- empty -}
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synExpdSorts = []
                ;  $$.synVarSigs   = []
                ;  $$ = []
                ;  where if $$.inhSolvSorts == [] then () else
                           if head $$.inhSolvSorts == Nothing then () else
                             error ("\nTXS0381: " ++
                                    "Channel offers do not match channel definition\n")
                }
              | ChannelOffer ChannelOffList
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $2.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$.synExpdSorts = $1.synExpdSorts ++ $2.synExpdSorts
                ;  $1.inhSolvSorts = if (length $1.synExpdSorts) > (length $$.inhSolvSorts)
                                       then error ("\nTXS0382: :" ++
                                            "Number of offers exceeds channel definition\n")
                                       else take (length $1.synExpdSorts) $$.inhSolvSorts
                ;  $2.inhSolvSorts = drop (length $1.synExpdSorts) $$.inhSolvSorts
                ;  $$.synVarSigs   = $1.synVarSigs ++ $2.synVarSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $$ = $1 ++ $2
                }

ChannelOffer    -- :: { [ChanOffer] }
                -- delivers a list of ChanOffer, ie, a list ?-offers or one !-offer
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhVarSigs  : usable variables
                --           : inhSolvSorts: sorts expected according to channel sorts
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synExpdSorts: number sorts according to channel offer
                --                           only used to determine the length of inhSolvSorts
                --           : synVarSigs  : defined variables in offers
                -- mirroring : -
                -- constrs   : -
              : "?" VarDecl
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $$.synExpdSorts = [ [] ]
                ;  $$.synVarSigs   = [ $2 ]
                ;  $$ = [ Quest $2 ]
                ;  where let { actsl = [ Just sid | VarId nm uid sid <- [$2] ]
                             ; misma = [ (esid,asid)
                                       | (esid,asid) <- zip $$.inhSolvSorts actsl
                                       , not ( esid==asid || esid == Nothing )
                                       ]
                             }
                          in if null misma then () else
                             error ("\nTXS0391: Sort mismatch in channel offer " ++
                                    "(expected,actual):\n"++(show misma)++"\n")
                }
              | "?" smallid
                {  $$.synMaxUid    = $$.inhNodeUid + 1
                ;  $$.synExpdSorts = [ [] ]
                ;  $$.synVarSigs
                     = [ VarId nm
                               uid
                               ( case msid of
                                 { Nothing  -> error ("\nTXS0392: "++
                                                 "No sort for offer variable: "++
                                                 (show nm)++"\n")
                                 ; Just sid -> sid
                                 }
                               )
                       | (nm,msid,uid) <- zip3 [$2] $$.inhSolvSorts [$$.inhNodeUid]
                       , if 1 == (length $$.inhSolvSorts) then True
                           else error ("\nTXS ERROR 0183: " ++ show($2) ++"\n")
                       ]
                ;  $$ = map Quest $$.synVarSigs
                }
              | "!" ValExpr
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs  = $$.inhSigs
                ;  $$.synExpdSorts = [ [] ]
                ;  $2.inhSolvSort  = case $$.inhSolvSorts of
                                     { []           -> error "\nTXS ERROR 0184\n"
                                     ; [Nothing]    -> Nothing
                                     ; [Just sid]   -> Just sid
                                     ; _            -> error "\nTXS ERROR 0185\n"
                                     }
                ;  $$.synVarSigs   = []
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $$ = [ Exclam $2 ]
                }


-- ----------------------------------------------------------------------------------------- --
-- Value expressions


ExValExpr       -- :: { ( Int, VExpr ) }
                -- top-level value expression for external use with multiple parsers
                -- attrs inh : SIGS : 
                --           : VARENV: variable declarations environment
                --           : UNID  : unique node identification
                -- attrs syn : $$: MaxUid: maximum uid in whole subtree
                --           : $$: VExpr : VExpr parsed value expression
              : SIGS VARENV UNID ValExpr
                {  $4.inhSigs      = $1
                ;  $4.inhVarSigs   = $2
                ;  $4.inhSolvSort  = Nothing
                ;  $4.inhNodeUid   = $3
                ;  $$ = ( $4.synMaxUid, $4 )
                }

ValExpr         -- :: { VExpr }
                -- top-level value expression
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                --           : inhVarSigs : usable variables
                --           : inhSolvSort: Maybe externally forced sort of ValExpr
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : synExpdSort -> inhSolvSort
                -- constrs   : synExpdSort and inhSolvSort lead to unique sort for ValExpr
              : ValExpr1
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $1.inhSolvSort
                     = case ($$.inhSolvSort, $1.synExpdSort) of
                       { (Nothing, [])
                            -> error ("\nTXS0411: " ++
                                      "Sort of value expression not resolved\n")
                       ; (Nothing, [sid])
                            -> Just sid
                       ; (Nothing, sids)
                            -> error ("\nTXS0412: " ++
                                      "Sort of value expression not uniquely resolved: "++
                                      (show sids)++"\n")
                       ; (Just sid, ss)
                            -> if sid `elem` ss
                                 then Just sid
                                 else error ("\nTXS0413: " ++
                                             "Expected sort of value expression: "++
                                             (show sid)++"\n  does not match any of the"++
                                             " possible actual sort(s): "++(show ss)++"\n")
                       }
                ;  $$ = $1
                }

ValExpr1        -- :: { VExpr }
                -- value expression
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                --           : inhVarSigs : usable variables
                --           : inhSolvSort: uniquely solved sort of ValExpr
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synExpdSort: potential sorts of ValExpr
                -- mirroring : -
                -- constrs   : identifiers must be uniquely resolved based on sorts
                --           : evaluation from-left-to-right
              : LET NeValueDefList IN ValExpr1 EndIn
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $4.inhNodeUid   = $2.synMaxUid + 1
                ;  $$.synMaxUid    = $4.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $4.inhSigs      = $$.inhSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $4.inhVarSigs   = map (\(IdVar v) -> v ) $ scopeMerge (map IdVar $$.inhVarSigs) (map IdVar $2.synVarSigs)
                ;  $$.synExpdSort  = $4.synExpdSort 
                ;  $4.inhSolvSort  = $$.inhSolvSort
                ;  $$ = foldr (\x -> subst x (Map.empty :: Map.Map FuncId (FuncDef VarId)) ) $4 $2
                }
              | IF NeValExprs THEN ValExpr1 ELSE ValExpr1 EndIf
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $4.inhNodeUid   = $2.synMaxUid + 1
                ;  $6.inhNodeUid   = $4.synMaxUid + 1
                ;  $$.synMaxUid    = $6.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $4.inhSigs      = $$.inhSigs
                ;  $6.inhSigs      = $$.inhSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $4.inhVarSigs   = $$.inhVarSigs
                ;  $6.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExpdSort  = $4.synExpdSort `List.intersect` $6.synExpdSort
                ;  $2.inhSolvSorts = [ if sortId_Bool `elem` sids
                                         then Just sortId_Bool
                                         else error ("\nTXS0421: " ++
                                                     "Sort of constraint must be 'Bool', "++
                                                     " instead of: "++(show sids)++"\n")
                                     | sids <- $2.synExpdSorts
                                     ]
                ;  $4.inhSolvSort = $$.inhSolvSort
                ;  $6.inhSolvSort = $$.inhSolvSort
                ;  $$ = cstrITE (cstrAnd (Set.fromList $2)) $4 $6
                }
              | ValExpr1 operator ValExpr2
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExpdSort  = [ res
                                     | Signature [l,r] res <- signatures $2 (Sigs.func $$.inhSigs)
                                     , l `elem` $1.synExpdSort
                                     , r `elem` $3.synExpdSort
                                     ]
                ;  $1.inhSolvSort
                     = let pargs = [ l
                                   | Signature [l,r] res <- signatures $2 (Sigs.func $$.inhSigs)
                                   , l `elem` $1.synExpdSort
                                   , r `elem` $3.synExpdSort
                                   , case $$.inhSolvSort of
                                            { Nothing  -> True
                                            ; Just sid -> res == sid
                                            }
                                   ]
                        in if (length pargs) == 1 then Just (head pargs) else Nothing
                ;  $3.inhSolvSort
                     = let pargs = [ r
                                   | Signature [l,r] res <- signatures $2 (Sigs.func $$.inhSigs)
                                   , l `elem` $1.synExpdSort
                                   , r `elem` $3.synExpdSort
                                   , case $$.inhSolvSort of
                                            { Nothing  -> True
                                            ; Just sid -> res == sid
                                            }
                                   ]
                        in if (length pargs) == 1 then Just (head pargs) else Nothing
                ;  $$ = let vexps = [ h [$1,$3]
                                    | (Signature [l,r] res, h) <- Map.toList (signHandler $2 (Sigs.func $$.inhSigs))
                                    , l `elem` $1.synExpdSort
                                    , r `elem` $3.synExpdSort
                                    , case $$.inhSolvSort of
                                             { Nothing  -> True
                                             ; Just sid -> res == sid
                                             }
                                    ]
                         in case length vexps of
                            { 0 -> error ("\nTXS0422: " ++
                                          "Operator not resolved: '" ++ show $2 ++ "'\n" ++
                                          "Argument 0: " ++ show $1 ++ "\n" ++
                                          "Argument 1: " ++ show $3 ++ "\n" )
                            ; 1 -> head vexps
                            ; n -> error ("\nTXS0423: "++
                                     "Operator not uniquely resolved: '" ++ show $2 ++ "'\n")
                            }
                }
              | ValExpr2 OfSort
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $2.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $2.inhSigs      = $$.inhSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExpdSort  = [ $2 ]
                ;  $1.inhSolvSort  = Just $2
                ;  $$ = $1
                ;  where if $$.inhSolvSort == $1.inhSolvSort then () else
                         error ("\nTXS0424 : " ++
                                "Explicit sort does not correspond with actual sort:"++
                                (show (SortId.name $2))++"\n")
                }
              | ValExpr2
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExpdSort  = $1.synExpdSort 
                ;  $1.inhSolvSort  = $$.inhSolvSort
                ;  $$ = $1
                }

ValExpr2        -- :: { VExpr }
                -- value expression
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                --           : inhVarSigs:  usable variables
                --           : inhSolvSort: uniquely solved sort of ValExpr
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synExpdSort: potential sorts of ValExpr
                -- mirroring : -
                -- constrs   : identifiers must be uniquely resolved based on sorts
                -- evaluation from-left-to-right
              : smallid
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synExpdSort
                     =   [ res | Signature [] res <- signatures $1 (Sigs.func $$.inhSigs) ]
                      ++ [ srt | VarId nm uid srt <- $$.inhVarSigs
                               , nm == $1 ]
                ;  $$ = let vexps =   [ h []
                                      | (Signature [] res, h) <- Map.toList (signHandler $1 (Sigs.func $$.inhSigs))
                                      , case $$.inhSolvSort of
                                               { Nothing  -> True
                                               ; Just sid -> res == sid
                                               }
                                      ]
                                   ++ [ cstrVar vid 
                                      | vid@(VarId nm uid srt) <- $$.inhVarSigs
                                      , nm == $1
                                      , case $$.inhSolvSort of
                                               { Nothing  -> True
                                               ; Just sid -> srt == sid
                                               }
                                      ]
                         in if (length vexps) == 1
                              then head vexps
                              else if (length vexps) == 0
                                      then error ("\nTXS0430: Identifier not resolved: " ++ show $1 ++"\n")
                                      else error ("\nTXS0431: Identifier not uniquely resolved: " ++ show $1 ++"\n")
                }
              | smallid "(" ValExprs ")"
                {  $3.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $3.inhSigs      = $$.inhSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExpdSort  = [ res
                                     | Signature args res <- signatures $1 (Sigs.func $$.inhSigs)
                                     , length args == length $3.synExpdSorts
                                     , and [ a`elem`as | (a,as) <- zip args $3.synExpdSorts ]
                                     ]
                ;  $3.inhSolvSorts
                     = let pargs = [ args 
                                   | Signature args res <- signatures $1 (Sigs.func $$.inhSigs)
                                   , length args == length $3.synExpdSorts
                                   , and [ a`elem`as | (a,as) <- (zip args $3.synExpdSorts) ]
                                   , case $$.inhSolvSort of
                                            { Nothing  -> True
                                            ; Just sid -> res == sid
                                            }
                                   ]
                        in if (length pargs) == 1
                             then [ Just arg | arg <- concat pargs ]
                             else [ Nothing | x <- $3.synExpdSorts ]
                ;  $$ = let vexps = [ h $3
                                    | (Signature args res, h) <- Map.toList (signHandler $1 (Sigs.func $$.inhSigs))
                                    , length args == length $3.synExpdSorts
                                    , and [ a`elem`as | (a,as) <- (zip args $3.synExpdSorts) ]
                                    , case $$.inhSolvSort of
                                             { Nothing  -> True
                                             ; Just sid -> res == sid
                                             }
                                    ]
                         in if (length vexps) == 1
                              then head vexps
                              else error $ "\nTXS0432: " ++
                                           "Function not (uniquely) resolved: " ++
                                           (show $1) ++
                                           "\nExpected Args: " ++ (pshow $3.synExpdSorts) ++
                                           "\nFound: " ++ (pshow vexps)
                }
              | operator ValExpr2
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExpdSort  = [ res
                                     | Signature [a] res <- signatures $1 (Sigs.func $$.inhSigs)
                                     , a `elem` $2.synExpdSort
                                     ]
                ;  $2.inhSolvSort
                     = let pargs = [ a
                                   | Signature [a] res <- signatures $1 (Sigs.func $$.inhSigs)
                                   , a `elem` $2.synExpdSort
                                   , case $$.inhSolvSort of
                                            { Nothing  -> True
                                            ; Just sid -> res == sid
                                            }
                                   ]
                        in if (length pargs) == 1 then Just (head pargs) else Nothing
                ;  $$ = let vexps = [ h [$2]
                                    | (Signature [a] res, h) <- Map.toList (signHandler $1 (Sigs.func $$.inhSigs))
                                    , a `elem` $2.synExpdSort
                                    , case $$.inhSolvSort of
                                             { Nothing  -> True
                                             ; Just sid -> res == sid
                                             }
                                    ]
                         in case (length vexps) of
                                {   1   -> head vexps
                                ;   0  ->  error ("\nTXS0433: Operator not resolved: '" ++ show $1 ++ "'\n")
                                ;   _  ->  error ("\nTXS0433: Operator not uniquely resolved: '" ++ show $1 ++ "'\n")
                                }
                }
              | capid
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synExpdSort  =  [ res
                                      | Signature [] res <- FuncTable.signatures $1 (Sigs.func $$.inhSigs)
                                      ]
                ;  $$ = let vexps = [ handler []
                                    | (Signature [] res, handler) <- Map.toList (FuncTable.signHandler $1 (Sigs.func $$.inhSigs))
                                    , case $$.inhSolvSort of
                                             { Nothing  -> True
                                             ; Just sid -> res == sid
                                             }
                                    ]
                         in case (length vexps) of
                                { 1 -> head vexps
                                ; 0 -> error ("\nTXS0436: Constructor not resolved: " ++ show $1 ++"\n" 
                                                ++ "Available signatures for " ++ show $1 ++ " are \n* "
                                                ++ Utils.join "\n* " (map show (FuncTable.signatures $1 (Sigs.func $$.inhSigs)))
                                             )
                                ; _ -> error ("\nTXS0436: Constructor not uniquely resolved: " ++ show $1 ++"\n" 
                                                ++ "Matching signatures for " ++ show $1 ++ " are \n* "
                                                ++ Utils.join "\n* " (map show [ s | s@(Signature [] _) <- FuncTable.signatures $1 (Sigs.func $$.inhSigs) ])
                                             )
                                }
                }
              | capid "(" ValExprs  ")"
                {  $3.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $3.inhSigs      = $$.inhSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExpdSort  = [ res
                                     | Signature args res <- signatures $1 (Sigs.func $$.inhSigs)
                                     , length args == length $3.synExpdSorts
                                     , and [ a `elem` as | (a,as) <- zip args $3.synExpdSorts ]
                                     ]
                ;  $3.inhSolvSorts
                     = let pargs = [ args 
                                   | Signature args res <- signatures $1 (Sigs.func $$.inhSigs)
                                   , length args == length $3.synExpdSorts
                                   , and [ a `elem` as | (a,as) <- (zip args $3.synExpdSorts) ]
                                   , case $$.inhSolvSort of
                                            { Nothing  -> True
                                            ; Just sid -> res == sid
                                            }
                                   ]
                        in if (length pargs) == 1
                             then [ Just arg | arg <- concat pargs ]
                             else [ Nothing | x <- $3.synExpdSorts ]
                ;  $$ = let vexps = [ handler $3
                                    | (Signature args res, handler) <- Map.toList (FuncTable.signHandler $1 (Sigs.func $$.inhSigs))
                                    , length args == length $3.synExpdSorts
                                    , and [ a `elem` as | (a,as) <- (zip args $3.synExpdSorts) ]
                                    , case $$.inhSolvSort of
                                            { Nothing  -> True
                                            ; Just sid -> res == sid
                                            }
                                    ]
                         in case (length vexps) of
                                { 1 -> head vexps
                                ; 0 -> error ("\nTXS0434: Constructor not resolved: " ++ show $1 ++ "\n"
                                               ++ "Available signatures for " ++ show $1 ++ " are \n* "
                                               ++ Utils.join "\n* " (map show (FuncTable.signatures $1 (Sigs.func $$.inhSigs)))
                                             )
                                ; _ -> error ("\nTXS0434: Constructor not uniquely resolved: " ++ show $1 ++"\n" 
                                                ++ "Matching signatures for " ++ show $1 ++ " are \n* "
                                                ++ Utils.join "\n* " (map show [ s
                                                                               | s@(Signature args _) <- FuncTable.signatures $1 (Sigs.func $$.inhSigs)
                                                                               , length args == length $3.synExpdSorts
                                                                               , and [ a `elem` as | (a,as) <- zip args $3.synExpdSorts ]
                                                                               ])
                                             )
                                }
                }
              | Constant
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$.synExpdSort  = $1.synExpdSort
                ;  $1.inhSolvSort  = $$.inhSolvSort
                ;  $$ = cstrConst $1
                }
              | "(" ValExpr1 ")"
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExpdSort  = $2.synExpdSort
                ;  $2.inhSolvSort  = $$.inhSolvSort
                ;  $$ = $2
                }
              | ANY
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synExpdSort  = Map.elems (Sigs.sort $$.inhSigs)
                ;  $$ = case $$.inhSolvSort of
                        { Nothing  -> error $ "\nTXS0435: " ++
                                              "Sort of ANY cannot be deduced\n"
                        ; Just srt -> cstrAny srt
                        }
                }
              | ERROR string
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synExpdSort  = Map.elems (Sigs.sort $$.inhSigs)
                ;  $$ = cstrError $2
                }

ValExprs        -- :: { [ VExpr] }
                -- list of value expressions
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhVarSigs  : usable variables
                --           : inhSolvSorts: list of uniquely solved sorts of ValExprs
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synExpdSorts: list of potential sorts of ValExprs
                -- mirroring : -
                -- constrs   : -
              : {- empty -}
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synExpdSorts = []
                ;  $$ = []
                }
              | NeValExprs
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExpdSorts = $1.synExpdSorts
                ;  $1.inhSolvSorts = $$.inhSolvSorts
                ;  $$ = $1
                }

NeValExprs      -- :: { [VExpr] }
                -- non-empty list of value expressions
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhVarSigs  : usable variables
                --           : inhSolvSorts: list of uniquely solved sorts of ValExprs
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synExpdSorts: list of potential sorts of ValExprs
                -- mirroring : -
                -- constrs   : -
              : ValExpr1
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExpdSorts = [ $1.synExpdSort ]
                ;  $1.inhSolvSort  = case $$.inhSolvSorts of
                                     { []     -> Nothing
                                     ; (s:ss) -> s
                                     }
                ;  $$ = [$1]
                }
              | NeValExprs "," ValExpr1 
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $$.synExpdSorts = $1.synExpdSorts ++ [ $3.synExpdSort ]
                ;  $1.inhSolvSorts = if  null $$.inhSolvSorts
                                       then []
                                       else init $$.inhSolvSorts
                ;  $3.inhSolvSort  = if  null $$.inhSolvSorts
                                       then Nothing
                                       else last $$.inhSolvSorts
                ;  $$ = $1 ++ [$3]
                }

NeValueDefList  -- :: { [ VEnv ] }
                -- non-empty list of value definitions
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                --           : inhVarSigs : usable variables
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synVarSigs : locally defined variables
                -- mirroring : -
                -- constrs   : variable identifiers shall be unique
              : NeValueDefs
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$.synVarSigs   = $1.synVarSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$ = [ $1 ]
                }
              | NeValueDefs ";" NeValueDefList
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $$.synVarSigs   = map (\(IdVar v) -> v ) $ scopeMerge (map IdVar $1.synVarSigs) (map IdVar $3.synVarSigs)
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = map (\(IdVar v) -> v ) $ scopeMerge (map IdVar $$.inhVarSigs) (map IdVar $1.synVarSigs)         
                ;  $$ = $1 : $3      
                }

ExNeValueDefs   -- :: { ( Int, VEnv ) }
                -- non-empty list of simultaneaous value definitions
                -- for external use with multiple parsers
                -- attrs inh : SIGS 
                --           : VARENV: variable declarations environment
                --           : UNID  : unique node identification
                -- attrs syn : $$: MaxUid: maximum uid in whole subtree
                --           : $$: VEnv  : locally defined value definitions
              : SIGS VARENV UNID NeValueDefs
                {  $4.inhSigs      = $1
                ;  $4.inhVarSigs   = $2
                ;  $4.inhNodeUid   = $3
                ;  $$ = ( $4.synMaxUid, $4 )
                }

NeValueDefs     -- :: { VEnv }
                -- non-empty list of simultaneous value definitions
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                --           : inhVarSigs : usable variables
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synVarSigs : locally defined variables
                -- mirroring : -
                -- constrs   : variable identifiers shall be unique
              : ValueDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$.synVarSigs   = $1.synVarSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $$ = $1
                }
              | NeValueDefs "," ValueDef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $$.synVarSigs   = $1.synVarSigs ++ $3.synVarSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $$ = ( $1 `Map.union` $3 )::VEnv
                ;  where let dbls = doubles ( map ( sig . IdVar ) $$.synVarSigs )
                          in if null dbls then () else
                             error ("\nTXS0452: " ++
                                    "Double defined value identifiers in definition: "++
                                    (show dbls)++"\n")
                }

ValueDef        -- :: { VEnv }
                -- one value definition
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                --           : inhVarSigs : usable variables
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synVarSigs : locally defined variable
                -- mirroring : -
                -- constrs   : -
              : smallid OfSort "=" ValExpr
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $4.inhNodeUid   = $2.synMaxUid + 1
                ;  $$.synMaxUid    = $4.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $4.inhSigs      = $$.inhSigs
                ;  $4.inhSolvSort  = Just $2
                ;  $4.inhVarSigs   = $$.inhVarSigs
                ;  $$.synVarSigs   = [ VarId $1 $$.inhNodeUid $2 ]
                ;  $$ = Map.singleton (VarId $1 $$.inhNodeUid $2) $4
                }
              | smallid "=" ValExpr
                {  $3.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $3.inhSigs      = $$.inhSigs
                ;  $3.inhSolvSort  = Nothing
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $$.synVarSigs   = [ VarId $1 $$.inhNodeUid (sortOf ($3::VExpr)) ]
                ;  $$ = Map.singleton ( VarId $1 $$.inhNodeUid (sortOf ($3::VExpr)) ) $3
                }


-- ----------------------------------------------------------------------------------------- --
-- Constants with special syntax: 'Int', 'String', 'Regex ("<content>")'


Constant        -- :: { Const }
                -- predefined constants with special syntax: 'Bool', 'Int', 'String'
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhSolvSort: uniquely solved sort for check and unlazying
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                --           : synExpdSort: sort of Constant
                -- mirroring : -
                -- constrs   : sort of constant must be solved sort
              : True
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synExpdSort  = [ sortId_Bool ]
                ;  $$ = case $$.inhSolvSort of
                        { Nothing                   -> Cbool True
                        ; Just s | s == sortId_Bool -> Cbool True
                        ; Just _                    -> error "\nTXS ERROR 0909\n"
                        }
                ;  where case Map.lookup "Bool" (Sigs.sort $$.inhSigs) of
                            {   Just s | s == sortId_Bool    -> ()
                            ;   _                            -> error ("\nTXS0471: Bool constant but no sort 'Bool': True\n")
                            }
                }
              | False
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synExpdSort  = [ sortId_Bool ]
                ;  $$ = case $$.inhSolvSort of
                        { Nothing                   -> Cbool False
                        ; Just s | s == sortId_Bool -> Cbool False
                        ; Just _                    -> error "\nTXS ERROR 0910\n"
                        }
                ;  where case Map.lookup "Bool" (Sigs.sort $$.inhSigs) of
                            {   Just s | s == sortId_Bool    -> ()
                            ;   _                            -> error ("\nTXS0471: Bool constant but no sort 'Bool': False\n")
                            }
                }
              | integer
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synExpdSort  = [ sortId_Int ]
                ;  $$ = case $$.inhSolvSort of
                        { Nothing                   -> Cint $1
                        ; Just s | s == sortId_Int  -> Cint $1
                        ; Just _                    -> error "\nTXS ERROR 0911\n"
                        }
                ;  where case Map.lookup "Int" (Sigs.sort $$.inhSigs) of
                            {   Just s | s == sortId_Int     -> ()
                            ;   _                            -> error ("\nTXS0472: Integer constant but no sort 'Int': "++ show $1 ++"\n")
                            }
                }
              | string
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synExpdSort  = [ sortId_String ]
                ;  $$ = case $$.inhSolvSort of
                        { Nothing                       -> Cstring $1
                        ; Just s | s == sortId_String   -> Cstring $1
                        ; Just _                        -> error "\nTXS ERROR 0913\n"
                        }
                ;  where case Map.lookup "String" (Sigs.sort $$.inhSigs) of
                            {   Just s | s == sortId_String  -> ()
                            ;   _                            -> error ("\nTXS0476: String constant but no sort 'String': "++ show $1 ++"\n")
                            }
                }
              | REGEX "(" regexval ")"
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synExpdSort  = [ sortId_Regex ]
                ;  $$ = case $$.inhSolvSort of
                        { Nothing                       -> Cregex $3
                        ; Just s |  s == sortId_Regex   -> Cregex $3
                        ; Just _                        -> error "\nTXS ERROR 0915\n"
                        }
                ;  where case Map.lookup "Regex" (Sigs.sort $$.inhSigs) of
                            {   Just s | s == sortId_Regex   -> ()
                            ;   _                            -> error ("\nTXS0477: Regex constant but no sort 'Regex': REGEX("++ show $3 ++ ")\n")
                            }
                }


-- ----------------------------------------------------------------------------------------- --
-- Identifier lists -- lists of strings treated as quasi terminals, i.e., no Uid


Id              -- :: { String }
                -- identfier
              : capid
                {  $$ = $1
                }
              | smallid
                {  $$ = $1
                }

IdList          -- :: { [String] }
                -- a list of identfiers
              : {- empty -}
                {  $$ = []
                }
              | NeIdList
                {  $$ = $1
                }

NeIdList        -- :: { [String] }
                -- non-empty list of identfiers
              : Id
                {  $$ = [$1]
                }
              | NeIdList "," Id
                {  $$ = $1 ++ [$3]
                }

CapIdList       -- :: { [String] }
                -- a list of identfiers starting with cap letter
              : {- empty -}
                {  $$ = []
                }
              | NeCapIdList
                {  $$ = $1
                }

NeCapIdList     -- :: { [String] }
                -- non-empty list of identfiers starting with cap letter
              : capid
                {  $$ = [$1]
                }
              | NeCapIdList "," capid
                {  $$ = $1 ++ [$3]
                }

SharpCapIdList  -- :: { [String] }
                -- a '#'-separated list of identifiers starting with cap letter
              : {- empty -}
                {  $$ = []
                }
              | NeSharpCapIdList
                {  $$ = $1
                }

NeSharpCapIdList  -- :: { [String] }
                  -- a '#'-separated list of identifiers starting with cap letter
              : capid
                {  $$ = [$1]
                }
              | NeSharpCapIdList "#" capid
                {  $$ = $1 ++ [$3]
                }

NeBarCapIdList  -- :: { [String] }
                -- a '|'-separated list of identifiers starting with cap letter
              : capid
                {  $$ = [$1]
                }
              | NeBarCapIdList "|" capid
                {  $$ = $1 ++ [$3]
                }

-- SharpCapIdList1 -- :: { [String] }
--              -- a '#'-separated list of at least 2 identifiers starting with cap letter
--            : capid
--              {  $$ = [$1]
--              }
--            | SharpCapIdList1 "#" capid
--              {  $$ = $1 ++ [$3]
--              }

NeSmallIdList   -- :: { [String] }
                -- non-empty list of identfiers starting with small letter
              : smallid
                {  $$ = [$1]
                }
              | NeSmallIdList "," smallid
                {  $$ = $1 ++ [$3]
                }


-- ----------------------------------------------------------------------------------------- --
-- state automaton (symbolic transition system) items


StautItemList   -- :: { BExpr }
                -- top-level state automaton behaviour expression;
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts        = global sorts
                --           : inhCstrSigs : usable constructors = global constructors
                --           : inhFuncSigs : usable functions    = global functions
                --           : inhProcSigs : singleton with current process/staut procid
                --           : inhChanSigs : usable channels     = formal channels
                --           : inhVarSigs  : usable variables    = staut formal variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synExitSorts: exit sorts
                -- mirroring : synStateSigs -> inhStateSigs: staut defined states
                --           : synStVarSigs -> inhStVarSigs: staut state variables
                -- constrs   : all defined objects shall be unique
                --           : all used objects shall be defined
                --           : no overlap between state variable and parameter names
                --           : one initial state, and exactly one value for each state variable
              : StautItems
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $1.inhStateSigs = $1.synStateSigs
                ;  $1.inhStVarSigs = $1.synStVarSigs
                ;  $$.synExitSorts = $1.synExitSorts
                ;  $$ = case $1 of
                        { ( sts, vars, trs, [init], [venv] )
                            -> StAut init venv trs
                        ; _ -> error $ "\nTXS1010: " ++ "error in state atomaton def: " ++
                                       (show (Sigs.pro $$.inhSigs)) ++ "\n"
                        }
                ;  where
                     let ( sts, vars, trs, inits, venvs ) = $1
                      in if  not $ null $ doubles (map ( sig . IdStat ) sts)
                           then error $ "\nTXS1011: " ++ "Double defined states: " ++
                                        (show (Sigs.pro $$.inhSigs)) ++ "\n" else
                         if  not $ null $ doubles ( map ( sig . IdVar ) (vars ++ $$.inhVarSigs) )
                           then error $ "\nTXS1012: " ++ "Double defined state/parameter vars: "
                                        ++ (show (Sigs.pro $$.inhSigs)) ++ "\n" else
                         if  not $ Set.fromList vars == Set.fromList (Map.keys $ head venvs)
                           then error $ "\nTXS1015: " ++ "No (unique) initial values for " ++
                                        "all state vars: " ++ (show (Sigs.pro $$.inhSigs)) ++ "\n"
                           else ()
                }

StautItems      -- :: { ( [StatId], [VarId],  , [Trans]    , [StatId]   , [VEnv]          ) }
                --      ( states  , state vars, transitions, init states, init var values );
                -- state automaton behaviour items;
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts        = global sorts
                --           : inhCstrSigs : usable constructors = global constructors
                --           : inhFuncSigs : usable functions    = global functions
                --           : inhProcSigs : singleton with current process/staut procid
                --           : inhChanSigs : usable channels     = formal channels
                --           : inhVarSigs  : usable variables    = staut formal variables
                --           : inhStateSigs: usable states       = staut states
                --           : inhStVarSigs: usable variables    = staut state variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synStateSigs: defined states
                --           : synStVarSigs: defined state variables
                --           : synExitSorts: exit sorts
                -- mirroring : -
                -- constrs   : -
              : StautItem
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $1.inhStateSigs = $$.inhStateSigs
                ;  $1.inhStVarSigs = $$.inhStVarSigs
                ;  $$.synStateSigs = $1.synStateSigs
                ;  $$.synStVarSigs = $1.synStVarSigs
                ;  $$.synExitSorts = $1.synExitSorts
                ;  $$ = $1
                }
              | StautItems StautItem
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $2.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $2.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $2.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $1.inhStateSigs = $$.inhStateSigs
                ;  $2.inhStateSigs = $$.inhStateSigs
                ;  $1.inhStVarSigs = $$.inhStVarSigs
                ;  $2.inhStVarSigs = $$.inhStVarSigs
                ;  $$.synStateSigs = $1.synStateSigs ++ $2.synStateSigs
                ;  $$.synStVarSigs = $1.synStVarSigs ++ $2.synStVarSigs
                ;  $$.synExitSorts = $1.synExitSorts <<+>> $2.synExitSorts
                ;  $$ = $1 +++ $2
                }

StautItem       -- :: { ( [StatId], [VarId],  , [Trans]    , [StatId]  , [VEnv]          ) }
                --      ( states  , state vars, transitions, init state, init var values );
                -- state automaton behaviour item;
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts        = global sorts
                --           : inhCstrSigs : usable constructors = global constructors
                --           : inhFuncSigs : usable functions    = global functions
                --           : inhProcSigs : singleton with current process/staut procid
                --           : inhChanSigs : usable channels     = formal channels
                --           : inhVarSigs  : usable variables    = staut formal variables
                --           : inhStateSigs: usable states       = staut states
                --           : inhStVarSigs: usable variables    = staut state variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synStateSigs: defined states
                --           : synStVarSigs: defined state variables
                --           : synExitSorts: exit sorts
                -- mirroring : -
                -- constrs   : -
              : StateItem
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$.synStateSigs = $1.synStateSigs
                ;  $$.synStVarSigs = []
                ;  $$.synExitSorts = NoExit
                ;  $$ = ( $1, [], [], [], [] )
                }
              | VarItem
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $$.synStateSigs = []
                ;  $$.synStVarSigs = $1.synStVarSigs
                ;  $$.synExitSorts = NoExit
                ;  $$ = ( [], $1, [], [], [] )
                }
              | InitItem
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $1.inhStateSigs = $$.inhStateSigs
                ;  $1.inhStVarSigs = $$.inhStVarSigs
                ;  $$.synStateSigs = []
                ;  $$.synStVarSigs = []
                ;  $$.synExitSorts = NoExit
                ;  $$ = let (stid,venv) = $1
                         in ( [] , [], [], [stid] , [venv] )
                }
              | TransItem
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $1.inhStateSigs = $$.inhStateSigs
                ;  $1.inhStVarSigs = $$.inhStVarSigs
                ;  $$.synStateSigs = []
                ;  $$.synStVarSigs = []
                ;  $$.synExitSorts = $1.synExitSorts
                ;  $$ = ( [], [], $1, [], [] )
                }

StateItem       -- :: { [StatId] }
                -- states (locations) definition for state automaton behaviour;
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhProcSigs : singleton with current process/staut procid
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synStateSigs: defined states with global staut chans and vars
                -- mirroring : -
                -- constrs   : all defined states shall be unique
              : STATE NeIdList
                {  $$.synMaxUid    = $$.inhNodeUid + (length $2)
                ;  $$.synStateSigs = [ StatId nm uid pid
                                     | (nm,uid) <- zip $2 [$$.inhNodeUid ..]
                                     , pid <- Sigs.pro $$.inhSigs
                                     ]
                ;  $$ = $$.synStateSigs
                ;  where let dbls = doubles $2
                          in if (null dbls) && (length (Sigs.pro $$.inhSigs) == 1) then () else
                             error $ "\nTXS1031: " ++
                                     "Double defined states: " ++ (show dbls) ++ "\n"
                }

VarItem         -- :: { [VarId] }
                -- state variables definition for automaton behaviour;
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synStVarSigs: defined state variables
                -- mirroring : -
                -- constrs   : all defined objects shall be unique
                --           : all used objects shall be defined
              : VAR
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$.synStVarSigs = []
                ;  $$ = []
                }
              | VAR VarDeclList 
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs  = $$.inhSigs
                ;  $$.synStVarSigs = $2
                ;  $$ = $2
                ;  where let dbls = doubles ( map ( sig . IdVar ) $2 )
                          in if null dbls then () else
                             error ("\nTXS1041: " ++
                                    "Double defined state variables: " ++ (show dbls) ++ "\n")
                }

InitItem        -- :: { (StatId, VEnv) }
                -- initial state and variable assignment for state automaton behaviour;
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts     = global sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions = global functions
                --           : inhVarSigs  : usable variables = staut formal variables
                --           : inhStateSigs: usable states    = staut states
                --           : inhStVarSigs: usable variables = staut state variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : all defined objects shall be unique
                --           : all used objects shall be defined
              : INIT StateRef UpdateList
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $2.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $3.inhSigs      = $$.inhSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $2.inhStateSigs = $$.inhStateSigs
                ;  $3.inhStVarSigs = $$.inhStVarSigs
                ;  $$ = ( $2, $3 )
                }

TransItem       -- :: { [Trans] }
                -- transitions for state automaton behaviour;
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts     = global sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions = global functions
                --           : inhChanSigs : usable channels  = staut formal channels
                --           : inhVarSigs  : usable variables = staut formal variables
                --           : inhStateSigs: usable states    = staut states
                --           : inhStVarSigs: usable variables = staut state variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synExitSorts: exit sorts
                -- mirroring : -
                -- constrs   : all defined objects shall be unique
                --           : all used objects shall be defined
              : TRANS Transitions
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $2.inhChanSigs  = $$.inhChanSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $2.inhStateSigs = $$.inhStateSigs
                ;  $2.inhStVarSigs = $$.inhStVarSigs
                ;  $$.synExitSorts = $2.synExitSorts
                ;  $$ = $2
                }

Transitions     -- :: { [Trans] }
                -- transitions for state automaton behaviour;
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts     = global sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions = global functions
                --           : inhChanSigs : usable channels  = staut formal channels
                --           : inhVarSigs  : usable variables = staut formal variables
                --           : inhStateSigs: usable states    = staut states
                --           : inhStVarSigs: usable variables = staut state variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synExitSorts: exit sorts
                -- mirroring : -
                -- constrs   : all defined objects shall be unique
                --           : all used objects shall be defined
              : Transition
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $1.inhStateSigs = $$.inhStateSigs
                ;  $1.inhStVarSigs = $$.inhStVarSigs
                ;  $$.synExitSorts = $1.synExitSorts
                ;  $$ = [ $1 ]
                }
              | Transitions Transition
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $2.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $2.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $2.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $1.inhStateSigs = $$.inhStateSigs
                ;  $2.inhStateSigs = $$.inhStateSigs
                ;  $1.inhStVarSigs = $$.inhStVarSigs
                ;  $2.inhStVarSigs = $$.inhStVarSigs
                ;  $$.synExitSorts = $1.synExitSorts <<+>> $2.synExitSorts
                ;  $$ = $1 ++ [ $2 ]
                }
              | Transitions ";" Transition
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $1.inhChanSigs  = $$.inhChanSigs
                ;  $3.inhChanSigs  = $$.inhChanSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $1.inhStateSigs = $$.inhStateSigs
                ;  $3.inhStateSigs = $$.inhStateSigs
                ;  $1.inhStVarSigs = $$.inhStVarSigs
                ;  $3.inhStVarSigs = $$.inhStVarSigs
                ;  $$.synExitSorts = $1.synExitSorts <<+>> $3.synExitSorts
                ;  $$ = $1 ++ [ $3 ]
                }

Transition      -- :: { Trans }
                -- transition for state automaton behaviour;
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts     = global sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions = global functions
                --           : inhChanSigs : usable channels  = staut formal channels
                --           : inhVarSigs  : usable variables = staut formal variables
                --           : inhStateSigs: usable states    = staut states
                --           : inhStVarSigs: usable variables = staut state variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synExitSorts: exit sorts
                -- mirroring : -
                -- constrs   : all defined objects shall be unique
                --           : all used objects shall be defined
                --           : channel, state, and formal variables shall be disjoint
              : StateRef "->" PrefOfferList Constraints UpdateList "->" StateRef
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $4.inhNodeUid   = $3.synMaxUid + 1
                ;  $5.inhNodeUid   = $4.synMaxUid + 1
                ;  $7.inhNodeUid   = $5.synMaxUid + 1
                ;  $$.synMaxUid    = $7.synMaxUid
                ;  $3.inhSigs      = $$.inhSigs
                ;  $4.inhSigs      = $$.inhSigs
                ;  $5.inhSigs      = $$.inhSigs
                ;  $3.inhChanSigs  = $$.inhChanSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs ++ $$.inhStVarSigs
                ;  $4.inhVarSigs   = $$.inhVarSigs ++ $$.inhStVarSigs ++ $3.synVarSigs
                ;  $5.inhVarSigs   = $$.inhVarSigs ++ $$.inhStVarSigs ++ $3.synVarSigs
                ;  $1.inhStateSigs = $$.inhStateSigs
                ;  $7.inhStateSigs = $$.inhStateSigs
                ;  $5.inhStVarSigs = $$.inhStVarSigs
                ;  $$.synExitSorts = $3.synExitSorts
                ;  $$ = Trans $1 (ActOffer $3 (cstrAnd (Set.fromList $4))) $5 $7
                ;  where let dbls = doubles $ map ( sig . IdVar )
                                            $ $$.inhVarSigs ++ $$.inhStVarSigs ++ $3.synVarSigs
                          in if null dbls then () else
                             error $ "\nTXS1061: " ++ "State-, channel-, and formal-variables "
                                     ++ "overlap: " ++ (show dbls) ++ "\n"
                }
              | FROM StateRef VIA PrefOfferList Constraints UpdateList TO StateRef
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $4.inhNodeUid   = $2.synMaxUid + 1
                ;  $5.inhNodeUid   = $4.synMaxUid + 1
                ;  $6.inhNodeUid   = $5.synMaxUid + 1
                ;  $8.inhNodeUid   = $6.synMaxUid + 1
                ;  $$.synMaxUid    = $8.synMaxUid
                ;  $4.inhSigs      = $$.inhSigs
                ;  $5.inhSigs      = $$.inhSigs
                ;  $6.inhSigs      = $$.inhSigs
                ;  $4.inhChanSigs  = $$.inhChanSigs
                ;  $4.inhVarSigs   = $$.inhVarSigs ++ $$.inhStVarSigs
                ;  $5.inhVarSigs   = $$.inhVarSigs ++ $$.inhStVarSigs ++ $4.synVarSigs
                ;  $6.inhVarSigs   = $$.inhVarSigs ++ $$.inhStVarSigs ++ $4.synVarSigs
                ;  $2.inhStateSigs = $$.inhStateSigs
                ;  $8.inhStateSigs = $$.inhStateSigs
                ;  $6.inhStVarSigs = $$.inhStVarSigs
                ;  $$.synExitSorts = $4.synExitSorts
                ;  $$ = Trans $2 (ActOffer $4 (cstrAnd (Set.fromList $5))) $6 $8
                ;  where let dbls =doubles $ map ( sig . IdVar )
                                           $ $$.inhVarSigs ++ $$.inhStVarSigs ++ $4.synVarSigs
                          in if null dbls then () else
                             error $ "\nTXS1062: " ++ "State-, channel-, and formal-variables "
                                     ++ "overlap: " ++ (show dbls)++"\n"
                }
              | FROM StateRef TO StateRef VIA PrefOfferList Constraints UpdateList END
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $4.inhNodeUid   = $2.synMaxUid + 1
                ;  $6.inhNodeUid   = $4.synMaxUid + 1
                ;  $7.inhNodeUid   = $6.synMaxUid + 1
                ;  $8.inhNodeUid   = $7.synMaxUid + 1
                ;  $$.synMaxUid    = $8.synMaxUid
                ;  $6.inhSigs      = $$.inhSigs
                ;  $7.inhSigs      = $$.inhSigs
                ;  $8.inhSigs      = $$.inhSigs
                ;  $6.inhChanSigs  = $$.inhChanSigs
                ;  $6.inhVarSigs   = $$.inhVarSigs ++ $$.inhStVarSigs
                ;  $7.inhVarSigs   = $$.inhVarSigs ++ $$.inhStVarSigs ++ $6.synVarSigs
                ;  $8.inhVarSigs   = $$.inhVarSigs ++ $$.inhStVarSigs ++ $6.synVarSigs
                ;  $2.inhStateSigs = $$.inhStateSigs
                ;  $4.inhStateSigs = $$.inhStateSigs
                ;  $8.inhStVarSigs = $$.inhStVarSigs
                ;  $$.synExitSorts = $6.synExitSorts
                ;  $$ = Trans $2 (ActOffer $6 (cstrAnd (Set.fromList $7))) $8 $4
                ;  where let dbls = doubles $ map ( sig . IdVar )
                                            $ $$.inhVarSigs ++ $$.inhStVarSigs ++ $6.synVarSigs
                          in if null dbls then () else
                             error $ "\nTXS1063: " ++ "State-, channel-, and formal-variables "
                                     ++ "overlap: " ++ (show dbls) ++ "\n"
                }

Constraints     -- :: { [VExpr] }
                -- constraints of a transition;
                -- attrs inh : inhNodeUid : unique node identification
                --           : inhSortSigs: usable sorts
                --           : inhCstrSigs: usable constructors
                --           : inhFuncSigs: usable functions
                --           : inhVarSigs : usable variables in value expressions
                -- attrs syn : synMaxUid  : maximum uid in whole subtree
                -- mirroring : synExpdSorts -> inhSolvSorts
                -- constrs   : sort of value expressions shall be 'Bool'
              : {- empty -}
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = []::[VExpr]
                }
              | "[[" NeValExprs "]]"
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs  = $$.inhSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $2.inhSolvSorts = [ if sortId_Bool `elem` sids
                                         then Just sortId_Bool
                                         else Nothing
                                     | sids <- $2.synExpdSorts
                                     ]
                ;  $$ = $2::[VExpr]
                ;  where let notBools = [ vexp
                                        | (vexp,msid) <- zip $2 $2.inhSolvSorts
                                        , not (msid == Just sortId_Bool)
                                        ]
                          in if null notBools then () else
                             error  $ "\nTXS1101: " ++ "Sort of constraint must be 'Bool': " ++
                                      (show notBools) ++ "\n"
                }

UpdateList      -- :: { VEnv }
                -- top-level list of updates;
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhVarSigs  : usable variables in value expressions
                --           : inhStVarSigs: usable variables in assignments = state variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : exactly one unique update for each state variable
                --           : used objects shall be defined
              : {- empty -}
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = Map.fromList [ (vid, cstrVar vid) | vid <- $$.inhStVarSigs ]
                }
              | "{" "}"
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = Map.fromList [ (vid, cstrVar vid) | vid <- $$.inhStVarSigs ]
                }
              | Updates 
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $1.inhStVarSigs = $$.inhStVarSigs
                ;  $$ = let id = Map.fromList [ (vid, cstrVar vid)
                                              | vid <- $$.inhStVarSigs
                                              , vid `Map.notMember` $1
                                              ]
                         in $1 `Map.union` id
                }
              | "{" Updates "}"
                {  $2.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $2.synMaxUid
                ;  $2.inhSigs      = $$.inhSigs
                ;  $2.inhVarSigs   = $$.inhVarSigs
                ;  $2.inhStVarSigs = $$.inhStVarSigs
                ;  $$ = let id = Map.fromList [ (vid, cstrVar vid)
                                              | vid <- $$.inhStVarSigs
                                              , vid `Map.notMember` $2
                                              ]
                         in $2 `Map.union` id
                }

Updates         -- :: { VEnv }
                -- list of updates;
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhVarSigs  : usable variables in value expressions
                --           : inhStVarSigs: usable variables in assignments = state variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : updated state variables shall be unique
              : Update
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $1.synMaxUid
                ;  $1.inhSigs  = $$.inhSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $1.inhStVarSigs = $$.inhStVarSigs
                ;  $$ = $1
                }
              | Updates ";" Update
                {  $1.inhNodeUid   = $$.inhNodeUid + 1
                ;  $3.inhNodeUid   = $1.synMaxUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $1.inhSigs      = $$.inhSigs
                ;  $3.inhSigs      = $$.inhSigs
                ;  $1.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $1.inhStVarSigs = $$.inhStVarSigs
                ;  $3.inhStVarSigs = $$.inhStVarSigs
                ;  $$ = $1 `Map.union` $3
                ;  where let dbls = $1 `Map.intersection` $3
                          in if Map.null dbls then () else
                             error $ "\nTXS1121: " ++ "Double updated state variables : " ++
                                     (show dbls) ++ "\n"
                }

Update          -- :: { VEnv }
                -- single update of possibly multiple variables;
                -- attrs inh : inhNodeUid  : unique node identification
                --           : inhSortSigs : usable sorts
                --           : inhCstrSigs : usable constructors
                --           : inhFuncSigs : usable functions
                --           : inhVarSigs  : usable variables in value expressions
                --           : inhStVarSigs: usable variables in assignments = state variables
                -- attrs syn : synMaxUid   : maximum uid in whole subtree
                --           : synVarSigs  : locally updated state variables; only local use
                -- mirroring : -
                -- constrs   : updated state variables shall be unique
                --           : updated state variables shall be uniquely resolved
                --           : updated state variables shall have the same sort
                --           : updated state variables shall have the same sort as valexpr
              : NeSmallIdList ":=" ValExpr
                {  $3.inhNodeUid   = $$.inhNodeUid + 1
                ;  $$.synMaxUid    = $3.synMaxUid
                ;  $$.synVarSigs = [ case [ vid | vid <- $$.inhStVarSigs, VarId.name vid == nm ] of
                                     { [stvid] -> stvid
                                     ; _       -> error $
                                                    "\nTXS1141: State variable not (unique)" ++
                                                    " in Update: " ++ (show nm) ++ "\n"
                                     }
                                   | nm <- $1
                                   ]
                ;  $3.inhSigs      = $$.inhSigs
                ;  $3.inhVarSigs   = $$.inhVarSigs
                ;  $3.inhSolvSort  = case List.nub ( map varsort $$.synVarSigs ) of
                                     { [srt] -> Just srt
                                     ; srts  -> error $
                                                  "\nTXS1142: Sort of state variables not " ++
                                                  "unique in Update: " ++ (show srts) ++ "\n"
                                     }
                ;  $$ = Map.fromList [ (vid, $3) | vid <- $$.synVarSigs ]
                ;  where let dbls = doubles $1
                          in if null dbls then () else
                             error $ "\nTXS1133: " ++ "Double updated state variables : " ++
                                     (show dbls)++"\n"
                }

StateRef        -- :: { StatId }
                -- reference to state = used state id
                -- attrs inh : inhNodeUid:   unique node identification
                --           : inhStateSigs: usable states = staut states
                -- attrs syn : synMaxUid:    maximum uid in whole subtree
                -- mirroring : -
                -- constrs   : used state shall be defined and uniquely resolved
              : Id
                {  $$.synMaxUid    = $$.inhNodeUid
                ;  $$ = case filter ( ($1==) . StatId.name ) $$.inhStateSigs of
                        { []           -> error ("\nTXS1151: " ++
                                                 "State not defined: " ++ (show $1)++"\n")
                        ; [statid]     -> statid
                        ; (stid:stids) -> error ("\nTXS1152: " ++
                                                 "State not unique: " ++ (show $1)++"\n")
                        }
                }


-- ----------------------------------------------------------------------------------------- --
-- uninterpreted haskell postamble

{

-- ----------------------------------------------------------------------------------------- --
-- attribute functions


-- combine staut definition items
(+++) :: ([a],[b],[c],[d],[e]) -> ([a],[b],[c],[d],[e]) -> ([a],[b],[c],[d],[e])
(x1,y1,z1,u1,v1) +++ (x2,y2,z2,u2,v2) = ( x1++x2, y1++y2, z1++z2, u1++u2, v1++v2 )


-- combine exit sorts for choice, disable: max of exit sorts
(<<+>>) :: ExitSort -> ExitSort -> ExitSort
NoExit   <<+>> NoExit     =  NoExit
NoExit   <<+>> Exit exs   =  Exit exs
NoExit   <<+>> Hit        =  Hit
Exit exs <<+>> NoExit     =  Exit exs
Exit exs <<+>> Exit exs'  =  if  exs == exs'
                               then Exit exs
                               else error $ "\nTXS2222: Exit sorts do not match\n"
Exit exs <<+>> Hit        =  error $ "\nTXS2223: Exit sorts do not match\n"
Hit      <<+>> NoExit     =  Hit
Hit      <<+>> Exit exs   =  error $ "\nTXS2224: Exit sorts do not match\n"
Hit      <<+>> Hit        =  Hit


-- combine exit sorts for parallel: min of exit sorts
(<<->>) :: ExitSort -> ExitSort -> ExitSort
NoExit   <<->> NoExit     =  NoExit
NoExit   <<->> Exit exs   =  NoExit
NoExit   <<->> Hit        =  NoExit
Exit exs <<->> NoExit     =  NoExit
Exit exs <<->> Exit exs'  =  if  exs == exs'
                               then Exit exs
                               else error $ "\nTXS2225: Exit sorts do not match\n"
Exit exs <<->> Hit        =  error $ "\nTXS2226: Exit sorts do not match\n"
Hit      <<->> NoExit     =  NoExit
Hit      <<->> Exit exs   =  error $ "\nTXS2227: Exit sorts do not match\n"
Hit      <<->> Hit        =  Hit


-- ----------------------------------------------------------------------------------------- --
-- error handling

parseError :: [Token] -> a
parseError []      =  error $ "\nParse Error:  []"
parseError (t:tt)  =  error $ "\nParse Error:  " ++ (showToken t)

noerror = ()


showToken :: Token -> String
showToken t  =  case t of
                { Ttypedef          pos     ->  (showPos pos) ++ "  TYPEDEF"
                ; Tfuncdef          pos     ->  (showPos pos) ++ "  FUNCDEF"
                ; Tconstdef         pos     ->  (showPos pos) ++ "  CONSTDEF"
                ; Tprocdef          pos     ->  (showPos pos) ++ "  PROCDEF"
                ; Tstautdef         pos     ->  (showPos pos) ++ "  STAUTDEF"
                ; Tmodeldef         pos     ->  (showPos pos) ++ "  MODELDEF"
                ; Tcnectdef         pos     ->  (showPos pos) ++ "  CNECTDEF"
                ; Tenddef           pos     ->  (showPos pos) ++ "  ENDDEF"
                ; Tchan             pos     ->  (showPos pos) ++ "  CHAN"
                ; Tin               pos     ->  (showPos pos) ++ "  IN"
                ; Tout              pos     ->  (showPos pos) ++ "  OUT"
                ; Tsync             pos     ->  (showPos pos) ++ "  SYNC"
                ; Tclientsock       pos     ->  (showPos pos) ++ "  CLIENTSOCK"
                ; Tserversock       pos     ->  (showPos pos) ++ "  SERVERSOCK"
                ; Thost             pos     ->  (showPos pos) ++ "  HOST"
                ; Tport             pos     ->  (showPos pos) ++ "  PORT"
                ; Tencode           pos     ->  (showPos pos) ++ "  ENCODE"
                ; Tdecode           pos     ->  (showPos pos) ++ "  DECODE"
                ; Tstate            pos     ->  (showPos pos) ++ "  STATE"
                ; Tvar              pos     ->  (showPos pos) ++ "  VAR"
                ; Tinit             pos     ->  (showPos pos) ++ "  INIT"
                ; Ttrans            pos     ->  (showPos pos) ++ "  TRANS"
                ; Tfrom             pos     ->  (showPos pos) ++ "  FROM"
                ; Tvia              pos     ->  (showPos pos) ++ "  VIA"
                ; Tto               pos     ->  (showPos pos) ++ "  TO"
                ; Tvalue            pos     ->  (showPos pos) ++ "  VALUE"
                ; Tbehaviour        pos     ->  (showPos pos) ++ "  BEHAVIOUR"
                ; Tstop             pos     ->  (showPos pos) ++ "  STOP"
                ; Texit             pos     ->  (showPos pos) ++ "  EXIT"
                ; Taccept           pos     ->  (showPos pos) ++ "  ACCEPT"
                ; Thide             pos     ->  (showPos pos) ++ "  HIDE"
                ; Tlet              pos     ->  (showPos pos) ++ "  LET"
                ; Tni               pos     ->  (showPos pos) ++ "  NI"
                ; Tbegin            pos     ->  (showPos pos) ++ "  BEGIN"
                ; Tend              pos     ->  (showPos pos) ++ "  END"
                ; Tif               pos     ->  (showPos pos) ++ "  IF"
                ; Tthen             pos     ->  (showPos pos) ++ "  THEN"
                ; Telse             pos     ->  (showPos pos) ++ "  ELSE"
                ; Tfi               pos     ->  (showPos pos) ++ "  FI"
                ; Tistep            pos     ->  (showPos pos) ++ "  ISTEP"
                ; Terror            pos     ->  (showPos pos) ++ "  ERROR"
                ; Tregex            pos     ->  (showPos pos) ++ "  REGEX"
                ; Tarrow            pos     ->  (showPos pos) ++ "  '->'"
                ; Tbarrow           pos     ->  (showPos pos) ++ "  '<-'"
                ; Tchoice           pos     ->  (showPos pos) ++ "  '[]'"
                ; Taltchoice        pos     ->  (showPos pos) ++ "  '##'"
                ; Tsynchronization  pos     ->  (showPos pos) ++ "  '||'"
                ; Tinterleaving     pos     ->  (showPos pos) ++ "  '|||'"
                ; Tleftcommunicate  pos     ->  (showPos pos) ++ "  '|['"
                ; Trightcommunicate pos     ->  (showPos pos) ++ "  ']|'"
                ; Tprefix           pos     ->  (showPos pos) ++ "  '>->'"
                ; Tenable           pos     ->  (showPos pos) ++ "  '>>>'"
                ; Tdisable          pos     ->  (showPos pos) ++ "  '[>>'"
                ; Tinterrupt        pos     ->  (showPos pos) ++ "  '[><'"
                ; Topenpred         pos     ->  (showPos pos) ++ "  '[['"
                ; Tclosepred        pos     ->  (showPos pos) ++ "  ']]'"
                ; Topenlist         pos     ->  (showPos pos) ++ "  '['"
                ; Tcloselist        pos     ->  (showPos pos) ++ "  ']'"
                ; Topenbrace        pos     ->  (showPos pos) ++ "  '{'"
                ; Tclosebrace       pos     ->  (showPos pos) ++ "  '}'"
                ; Topenpar          pos     ->  (showPos pos) ++ "  '('"
                ; Tclosepar         pos     ->  (showPos pos) ++ "  ')'"
                ; Tsortof           pos     ->  (showPos pos) ++ "  '::'"
                ; Tisdef            pos     ->  (showPos pos) ++ "  '::='"
                ; Tassign           pos     ->  (showPos pos) ++ "  ':='"
                ; Tequal            pos     ->  (showPos pos) ++ "  '='"
                ; Tguard            pos     ->  (showPos pos) ++ "  '=>>'"
                ; Tbar              pos     ->  (showPos pos) ++ "  '|'"
                ; Tquestion         pos     ->  (showPos pos) ++ "  '?'"
                ; Texclam           pos     ->  (showPos pos) ++ "  '!'"
                ; Tsharp            pos     ->  (showPos pos) ++ "  '#'"
                ; Tsemicolon        pos     ->  (showPos pos) ++ "  ';'"
                ; Tcomma            pos     ->  (showPos pos) ++ "  ','"
                ; Tcapid            pos id  ->  (showPos pos) ++ "  " ++ (show id)
                ; Tsmallid          pos id  ->  (showPos pos) ++ "  " ++ (show id)
                ; Toperator         pos op  ->  (showPos pos) ++ "  " ++ (show op)
                ; Tinteger          pos n   ->  (showPos pos) ++ "  " ++ (show n)
                ; Tstring           pos s   ->  (showPos pos) ++ "  " ++ (show s)
                ; Tregexval         pos r   ->  (showPos pos) ++ "  " ++ (show r)
                ; Ctdefs            v       ->  "Ctdefs"
                ; Csigs             v       ->  "Csigs"
                ; Cchanenv          v       ->  "Cchanenv"
                ; Cvarenv           v       ->  "Cvarenv"
                ; Cunid             v       ->  "Cunid"
                }


showPos :: AlexPosn -> String
showPos (AlexPn a l c)  =  "( line = " ++ (show l) ++ ", column = " ++ (show c) ++ " )" 


-- ----------------------------------------------------------------------------------------- --
-- end uninterpreted haskell postamble

}
