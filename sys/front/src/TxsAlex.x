-- TorXakis - Model Based Testing
-- Copyright (c) 2015-2017 TNO and Radboud University
-- See LICENSE at root directory of this repository.


-- ----------------------------------------------------------------------------------------- --

{

module TxsAlex

( Token(..)                  -- exporting
, AlexPosn(..)               -- Alex type for Position
, txsLexer                   -- txsLexer :: String -> [Token]
)

where

import Data.Text (Text)
import qualified Data.Text as T
  
import TxsDefs
import Sigs
}

-- ----------------------------------------------------------------------------------------- --

%wrapper "posn"

$digit        = [0-9]                               -- digits
$oct          = [0-7]                               -- oct
$hex          = [0-9A-Fa-f]                         -- hex
$alphaCap     = [A-Z]                               -- large alphabetic characters
$alphaSmall   = [a-z]                               -- small alphabetic characters
$alpha        = [a-zA-Z]                            -- alphabetic characters
$special      = [\=\+\-\*\/\\\^\<\>\|\@\&\%]        -- special characters for operators
$space        = [\ ]
$graphic      = $printable # $white                 -- printable characters without white spaces
$stringable   = [.\n] # [\" \\]                     -- chars occurring in sort String 
                                                    -- Haskell's Prelude read for strings is assumed: \ and " have special meaning
$regexable    = [ $graphic $space ] # \'            -- chars occurring in sort Regex
 
tokens :-                                           -- Each right-hand side has type
                                                    -- :: AlexPosn -> String -> Token


    $white+                                                      ;   -- white space
    "--".*                                                       ;   -- comment
    "{-" ( [.\n] # \- 
         | \-+ [.\n]#\}
         )* 
    \-+ \}                                                       ;   -- comment
         
    TYPEDEF                   { tok ( \p _s -> Ttypedef p ) }
    FUNCDEF                   { tok ( \p _s -> Tfuncdef p ) }
    CONSTDEF                  { tok ( \p _s -> Tconstdef p ) }
    PROCDEF                   { tok ( \p _s -> Tprocdef p ) }
    CHANDEF                   { tok ( \p _s -> Tchandef p ) }
    STAUTDEF                  { tok ( \p _s -> Tstautdef p ) }
    MODELDEF                  { tok ( \p _s -> Tmodeldef p ) }
    PURPDEF                   { tok ( \p _s -> Tpurpdef p ) }
    MAPPERDEF                 { tok ( \p _s -> Tmapperdef p ) }
    CNECTDEF                  { tok ( \p _s -> Tcnectdef p ) }
    ENDDEF                    { tok ( \p _s -> Tenddef p ) }
    GOAL                      { tok ( \p _s -> Tgoal p ) }
    CHAN                      { tok ( \p _s -> Tchan p ) }
    IN                        { tok ( \p _s -> Tin p ) }
    OUT                       { tok ( \p _s -> Tout p ) }
    SYNC                      { tok ( \p _s -> Tsync p ) }
    CLIENTSOCK                { tok ( \p _s -> Tclientsock p ) }
    SERVERSOCK                { tok ( \p _s -> Tserversock p ) }
    HOST                      { tok ( \p _s -> Thost p ) }
    PORT                      { tok ( \p _s -> Tport p ) }
    ENCODE                    { tok ( \p _s -> Tencode p ) }
    DECODE                    { tok ( \p _s -> Tdecode p ) }
    STATE                     { tok ( \p _s -> Tstate p ) }
    VAR                       { tok ( \p _s -> Tvar p ) }
    INIT                      { tok ( \p _s -> Tinit p ) }
    TRANS                     { tok ( \p _s -> Ttrans p ) }
    FROM                      { tok ( \p _s -> Tfrom p ) }
    VIA                       { tok ( \p _s -> Tvia p ) }
    TO                        { tok ( \p _s -> Tto p ) }
    VALUE                     { tok ( \p _s -> Tvalue p ) }
    BEHAVIOUR                 { tok ( \p _s -> Tbehaviour p ) }
    STOP                      { tok ( \p _s -> Tstop p ) }
    EXIT                      { tok ( \p _s -> Texit p ) }
    HIT                       { tok ( \p _s -> Thit p ) }
    MISS                      { tok ( \p _s -> Tmiss p ) }
    ACCEPT                    { tok ( \p _s -> Taccept p ) }
    HIDE                      { tok ( \p _s -> Thide p ) }
    LET                       { tok ( \p _s -> Tlet p ) }
    NI                        { tok ( \p _s -> Tni p ) }
    BEGIN                     { tok ( \p _s -> Tbegin p ) }
    END                       { tok ( \p _s -> Tend p ) }
    IF                        { tok ( \p _s -> Tif p ) }
    THEN                      { tok ( \p _s -> Tthen p ) }
    ELSE                      { tok ( \p _s -> Telse p ) }
    FI                        { tok ( \p _s -> Tfi p ) }
    ISTEP                     { tok ( \p _s -> Tistep p ) }
    QSTEP                     { tok ( \p _s -> Tqstep p ) }
    ERROR                     { tok ( \p _s -> Terror p ) }
    REGEX                     { tok ( \p _s -> Tregex p ) }
    ANY                       { tok ( \p _s -> Tany p ) }
    True                      { tok ( \p _s -> Tbool p True) }
    False                     { tok ( \p _s -> Tbool p False) }
 
    \-\>                      { tok ( \p _s -> Tarrow p ) }
    \<\-                      { tok ( \p _s -> Tbarrow p ) }
    \[\]                      { tok ( \p _s -> Tchoice p ) }
    \#\#                      { tok ( \p _s -> Taltchoice p ) }
    \|\|                      { tok ( \p _s -> Tsynchronization p ) }
    \|\|\|                    { tok ( \p _s -> Tinterleaving p ) }
    \|\[                      { tok ( \p _s -> Tleftcommunicate p ) }
    \]\|                      { tok ( \p _s -> Trightcommunicate p ) }
    \>\-\>                    { tok ( \p _s -> Tprefix p ) }
    \>\>\>                    { tok ( \p _s -> Tenable p ) }
    \[\>\>                    { tok ( \p _s -> Tdisable p ) }
    \[\>\<                    { tok ( \p _s -> Tinterrupt p ) }
    \[\[                      { tok ( \p _s -> Topenpred p ) }
    \]\]                      { tok ( \p _s -> Tclosepred p ) }
    \[                        { tok ( \p _s -> Topenlist p ) }
    \]                        { tok ( \p _s -> Tcloselist p ) }
    \{                        { tok ( \p _s -> Topenbrace p ) }
    \}                        { tok ( \p _s -> Tclosebrace p ) }
    \(                        { tok ( \p _s -> Topenpar p ) }
    \)                        { tok ( \p _s -> Tclosepar p ) }
    \:\:                      { tok ( \p _s -> Tsortof p ) }
    \:\:\=                    { tok ( \p _s -> Tisdef p ) }
    \:\=                      { tok ( \p _s -> Tassign p ) }
    \=                        { tok ( \p _s -> Tequal p ) }
    \=\>\>                    { tok ( \p _s -> Tguard p ) }
    \|                        { tok ( \p _s -> Tbar p ) }
    \?                        { tok ( \p _s -> Tquestion p ) }
    \!                        { tok ( \p _s -> Texclam p ) }
    \#                        { tok ( \p _s -> Tsharp p ) }
    \;                        { tok ( \p _s -> Tsemicolon p ) }
    \,                        { tok ( \p _s -> Tcomma p ) }
 
    $alphaCap[$alpha $digit \_]*          { tokT ( \p s -> Tcapid p s ) }
    $alphaSmall[$alpha $digit \_]*        { tokT ( \p s -> Tsmallid p s ) }
    $special+                             { tokT ( \p s -> Toperator p s ) }
    $digit+                               { tok ( \p s -> Tinteger p (read s) ) }
    \"( [$stringable]
       | \\[abfnrtv\"\&\'\\]                -- Table B.1 Single-character escape codes http://book.realworldhaskell.org/read/characters-strings-and-escaping-rules.html
                                            -- Note: 0 is part of numeric escapes - dec

       | \\NUL                              -- Table B.2.
       | \\SOH
       | \\STX
       | \\ETX
       | \\EOT
       | \\ENQ
       | \\ACK
       | \\BEL
       | \\BS
       | \\HT
       | \\LF
       | \\VT
       | \\FF
       | \\CR
       | \\SO
       | \\SI
       | \\DLE
       | \\DC1
       | \\DC2
       | \\DC3
       | \\DC4
       | \\NAK
       | \\SYN
       | \\ETB
       | \\CAN
       | \\EM
       | \\SUB
       | \\ESC
       | \\FS
       | \\GS
       | \\RS
       | \\US
       | \\SP
       | \\DEL

                                            -- Table B.3. is not supported
                                            
       | \\ $digit+                         -- Numeric escapes - dec : includes 0 from Table B.1
       | \\[oO] $oct+                       -- Numeric escapes - oct
       | \\[xX] $hex+                       -- Numeric escapes - hex
      )*
    \"                                    { tok ( \p s -> Tstring p (T.pack (read s)) ) }
    \'[$regexable]*\'                     { tok ( \p s -> Tregexval p (T.pack (init (tail s))) ) }

-- ----------------------------------------------------------------------------------------- --

{
-- Some action helpers:
tok f p s = f p s
tokT f p s = f p (T.pack s)

-- The token type:
data  Token  =  Ttypedef          AlexPosn
              | Tfuncdef          AlexPosn
              | Tconstdef         AlexPosn
              | Tprocdef          AlexPosn
              | Tchandef          AlexPosn
              | Tstautdef         AlexPosn
              | Tmodeldef         AlexPosn
              | Tpurpdef          AlexPosn
              | Tmapperdef        AlexPosn
              | Tcnectdef         AlexPosn
              | Tenddef           AlexPosn
              | Tgoal             AlexPosn
              | Tchan             AlexPosn
              | Tin               AlexPosn
              | Tout              AlexPosn
              | Tsync             AlexPosn
              | Tclientsock       AlexPosn
              | Tserversock       AlexPosn
              | Thost             AlexPosn
              | Tport             AlexPosn
              | Tencode           AlexPosn
              | Tdecode           AlexPosn
              | Tstate            AlexPosn
              | Tvar              AlexPosn
              | Tinit             AlexPosn
              | Ttrans            AlexPosn
              | Tfrom             AlexPosn
              | Tvia              AlexPosn
              | Tto               AlexPosn
              | Tvalue            AlexPosn
              | Tbehaviour        AlexPosn
              | Tstop             AlexPosn
              | Texit             AlexPosn
              | Thit              AlexPosn
              | Tmiss             AlexPosn
              | Taccept           AlexPosn
              | Thide             AlexPosn
              | Tlet              AlexPosn
              | Tni               AlexPosn
              | Tbegin            AlexPosn
              | Tend              AlexPosn
              | Tif               AlexPosn
              | Tthen             AlexPosn
              | Telse             AlexPosn
              | Tfi               AlexPosn
              | Tistep            AlexPosn
              | Tqstep            AlexPosn
              | Terror            AlexPosn
              | Tregex            AlexPosn
              | Tany              AlexPosn
              | Tarrow            AlexPosn
              | Tbarrow           AlexPosn
              | Tchoice           AlexPosn
              | Taltchoice        AlexPosn
              | Tsynchronization  AlexPosn
              | Tinterleaving     AlexPosn
              | Tleftcommunicate  AlexPosn
              | Trightcommunicate AlexPosn
              | Tprefix           AlexPosn
              | Tenable           AlexPosn
              | Tdisable          AlexPosn
              | Tinterrupt        AlexPosn
              | Topenpred         AlexPosn
              | Tclosepred        AlexPosn
              | Topenlist         AlexPosn
              | Tcloselist        AlexPosn
              | Topenbrace        AlexPosn
              | Tclosebrace       AlexPosn
              | Topenpar          AlexPosn
              | Tclosepar         AlexPosn
              | Tsortof           AlexPosn
              | Tisdef            AlexPosn
              | Tassign           AlexPosn
              | Tequal            AlexPosn
              | Tguard            AlexPosn
              | Tbar              AlexPosn
              | Tquestion         AlexPosn
              | Texclam           AlexPosn
              | Tsharp            AlexPosn
              | Tsemicolon        AlexPosn
              | Tcomma            AlexPosn
              | Tcapid            AlexPosn  Text
              | Tsmallid          AlexPosn  Text
              | Toperator         AlexPosn  Text
              | Tbool             AlexPosn  Bool
              | Tinteger          AlexPosn  Integer
              | Tstring           AlexPosn  Text
              | Tregexval         AlexPosn  Text
              | Ctdefs            TxsDefs
              | Csigs             (Sigs VarId)
              | Cchanenv          [ChanId]
              | Cvarenv           [VarId]
              | Cunid             Int

txsLexer :: String -> [Token]
txsLexer = alexScanTokens
}

