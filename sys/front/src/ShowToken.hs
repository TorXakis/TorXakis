{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}
module ShowToken
( showToken
)
where
import           TxsAlex

showToken :: Token -> String
showToken t  =  case t of
                { Ttypedef          pos     ->  showPos pos ++ "  TYPEDEF"
                ; Tfuncdef          pos     ->  showPos pos ++ "  FUNCDEF"
                ; Tconstdef         pos     ->  showPos pos ++ "  CONSTDEF"
                ; Tprocdef          pos     ->  showPos pos ++ "  PROCDEF"
                ; Tchandef          pos     ->  showPos pos ++ "  CHANDEF"
                ; Tstautdef         pos     ->  showPos pos ++ "  STAUTDEF"
                ; Tmodeldef         pos     ->  showPos pos ++ "  MODELDEF"
                ; Tpurpdef          pos     ->  showPos pos ++ "  PURPDEF"
                ; Tmapperdef        pos     ->  showPos pos ++ "  MAPPERDEF"
                ; Tcnectdef         pos     ->  showPos pos ++ "  CNECTDEF"
                ; Tenddef           pos     ->  showPos pos ++ "  ENDDEF"
                ; Tgoal             pos     ->  showPos pos ++ "  GOAL"
                ; Tchan             pos     ->  showPos pos ++ "  CHAN"
                ; Tin               pos     ->  showPos pos ++ "  IN"
                ; Tout              pos     ->  showPos pos ++ "  OUT"
                ; Tsync             pos     ->  showPos pos ++ "  SYNC"
                ; Tclientsock       pos     ->  showPos pos ++ "  CLIENTSOCK"
                ; Tserversock       pos     ->  showPos pos ++ "  SERVERSOCK"
                ; Teworld           pos     ->  showPos pos ++ "  EWORLD"
                ; Thost             pos     ->  showPos pos ++ "  HOST"
                ; Tport             pos     ->  showPos pos ++ "  PORT"
                ; Tencode           pos     ->  showPos pos ++ "  ENCODE"
                ; Tdecode           pos     ->  showPos pos ++ "  DECODE"
                ; Tstate            pos     ->  showPos pos ++ "  STATE"
                ; Tvar              pos     ->  showPos pos ++ "  VAR"
                ; Tinit             pos     ->  showPos pos ++ "  INIT"
                ; Ttrans            pos     ->  showPos pos ++ "  TRANS"
                ; Tfrom             pos     ->  showPos pos ++ "  FROM"
                ; Tvia              pos     ->  showPos pos ++ "  VIA"
                ; Tto               pos     ->  showPos pos ++ "  TO"
                ; Tvalue            pos     ->  showPos pos ++ "  VALUE"
                ; Tbehaviour        pos     ->  showPos pos ++ "  BEHAVIOUR"
                ; Tstop             pos     ->  showPos pos ++ "  STOP"
                ; Texit             pos     ->  showPos pos ++ "  EXIT"
                ; Thit              pos     ->  showPos pos ++ "  HIT"
                ; Tmiss             pos     ->  showPos pos ++ "  MISS"
                ; Taccept           pos     ->  showPos pos ++ "  ACCEPT"
                ; Thide             pos     ->  showPos pos ++ "  HIDE"
                ; Tlet              pos     ->  showPos pos ++ "  LET"
                ; Tni               pos     ->  showPos pos ++ "  NI"
                ; Tbegin            pos     ->  showPos pos ++ "  BEGIN"
                ; Tend              pos     ->  showPos pos ++ "  END"
                ; Tif               pos     ->  showPos pos ++ "  IF"
                ; Tthen             pos     ->  showPos pos ++ "  THEN"
                ; Telse             pos     ->  showPos pos ++ "  ELSE"
                ; Tfi               pos     ->  showPos pos ++ "  FI"
                ; Tistep            pos     ->  showPos pos ++ "  ISTEP"
                ; Tqstep            pos     ->  showPos pos ++ "  QSTEP"
                ; Tregex            pos     ->  showPos pos ++ "  REGEX"
                ; Tany              pos     ->  showPos pos ++ "  ANY"
                ; Tarrow            pos     ->  showPos pos ++ "  '->'"
                ; Tbarrow           pos     ->  showPos pos ++ "  '<-'"
                ; Tchoice           pos     ->  showPos pos ++ "  '[]'"
                ; Taltchoice        pos     ->  showPos pos ++ "  '##'"
                ; Tsynchronization  pos     ->  showPos pos ++ "  '||'"
                ; Tinterleaving     pos     ->  showPos pos ++ "  '|||'"
                ; Tleftcommunicate  pos     ->  showPos pos ++ "  '|['"
                ; Trightcommunicate pos     ->  showPos pos ++ "  ']|'"
                ; Tprefix           pos     ->  showPos pos ++ "  '>->'"
                ; Tenable           pos     ->  showPos pos ++ "  '>>>'"
                ; Tdisable          pos     ->  showPos pos ++ "  '[>>'"
                ; Tinterrupt        pos     ->  showPos pos ++ "  '[><'"
                ; Topenpred         pos     ->  showPos pos ++ "  '[['"
                ; Tclosepred        pos     ->  showPos pos ++ "  ']]'"
                ; Topenlist         pos     ->  showPos pos ++ "  '['"
                ; Tcloselist        pos     ->  showPos pos ++ "  ']'"
                ; Topenbrace        pos     ->  showPos pos ++ "  '{'"
                ; Tclosebrace       pos     ->  showPos pos ++ "  '}'"
                ; Topenpar          pos     ->  showPos pos ++ "  '('"
                ; Tclosepar         pos     ->  showPos pos ++ "  ')'"
                ; Tsortof           pos     ->  showPos pos ++ "  '::'"
                ; Tisdef            pos     ->  showPos pos ++ "  '::='"
                ; Tassign           pos     ->  showPos pos ++ "  ':='"
                ; Tequal            pos     ->  showPos pos ++ "  '='"
                ; Tguard            pos     ->  showPos pos ++ "  '=>>'"
                ; Tbar              pos     ->  showPos pos ++ "  '|'"
                ; Tquestion         pos     ->  showPos pos ++ "  '?'"
                ; Texclam           pos     ->  showPos pos ++ "  '!'"
                ; Tsharp            pos     ->  showPos pos ++ "  '#'"
                ; Tsemicolon        pos     ->  showPos pos ++ "  ';'"
                ; Tcomma            pos     ->  showPos pos ++ "  ','"
                ; Tcapid            pos cid ->  showPos pos ++ "  " ++ show cid
                ; Tsmallid          pos sid ->  showPos pos ++ "  " ++ show sid
                ; Toperator         pos op  ->  showPos pos ++ "  " ++ show op
                ; Tbool             pos b   ->  showPos pos ++ "  " ++ show b
                ; Tinteger          pos n   ->  showPos pos ++ "  " ++ show n
                ; Tstring           pos s   ->  showPos pos ++ "  " ++ show s
                ; Tregexval         pos r   ->  showPos pos ++ "  " ++ show r
                ; Ctdefs            _       ->  "Ctdefs"
                ; Csigs             _       ->  "Csigs"
                ; Cchanenv          _       ->  "Cchanenv"
                ; Cvarenv           _       ->  "Cvarenv"
                ; Cunid             _       ->  "Cunid"
                }

showPos :: AlexPosn -> String
showPos (AlexPn _ l c)  =  "( line = " ++ show l ++ ", column = " ++ show c ++ " )"
