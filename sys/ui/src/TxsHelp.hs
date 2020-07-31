{-
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
-}


-- ----------------------------------------------------------------------------------------- --

module TxsHelp

-- ----------------------------------------------------------------------------------------- --
--
--   Help Text for TorXakis
--
-- ----------------------------------------------------------------------------------------- --
-- export

( helptxt     --  helptxt :: String
)

where

-- ----------------------------------------------------------------------------------------- --
-- torxakis help text

helptxt :: String

helptxt  =
  "\n"++
  "--------------------------------\n"++
  "TorXakis :: Model-Based Testing \n"++
  "--------------------------------\n"++
  "\n"++
  "quit, q                               : stop TorXakis completely\n"++
  "exit, x                               : exit the current command run of TorXakis \n"++
  "help, h, ?                            : show help\n"++
  "info, i                               : show info on TorXakis\n"++
  "param [<parameter>]                   : show value of <parameter>/[all parameters]\n"++
  "param <parameter> <value>             : set <parameter> to <value>\n"++
  "echo <text>                           : echo <text>\n"++
  "# <text>                              : comment       : <text> is ignored\n"++
  "seed <n>                              : set random seed to <n>\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"++
  "delay <n>                             : delay TorXakis for <n> seconds\n"++
  "time                                  : give the current time\n"++
  "timer <name>                          : set or read timer <name>\n"++
  "run <file>                            : run the torxakis script from <file>\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"++
  "var <variable-declarations>           : declare variables\n"++
  "val <value-definitions>               : define values\n"++
  "eval <value-expression>               : evaluate the (closed) <value-expression>\n"++
  "solve <value-expression>              : solve the (open, boolean) <value-expression>\n"++
  "unisolve <value-expression>           : solve (uniquely) the (open, boolean) <value-expression>\n"++
  "                                        'unsat': 0, 'sat': 1, 'unknown': >1 or unknown solution\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"++
  "tester <mod> [<purp>] [<map>] <cnect> : start testing with model <mod> and connection <cnect>\n"++
  "                                        using possible test purpose <purp> and/or mapper <map>\n"++
  "simulator <mod> <cnect>               : start simulating with model <mod> and connection <cnect>\n"++
--                                  mapper <map>\n"++
  "stepper <mod>                         : start stepping with model <mod>\n"++
  "stop                                  : stop testing, simulation, or stepping\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"++
  "test <action>                         : make a test step identified by (visible) input <action>\n"++
  "test                                  : make a test step by observing output/quiescence\n"++
  "test <n>                              : make <n> random test steps\n"++
  "sim [<n>]                             : make <n>/[unbounded] random simulation steps\n"++
  "step <action>                         : make a step identified by <action>\n"++
  "step [<n>]                            : make <n>/[one] random steps\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"++
  "show <object>                         : show <object>       :\n"++
  "                                        tdefs, state, model, purp, mapper, cnect, var, or val\n"++
  "state                                 : show current state number\n"++
  "btree [<state>]                       : show <state>/[current] behaviour tree\n"++
  "goto [<state>]                        : goto <state>/[current] state number in the model\n"++
  "back [<n>]                            : go back <n>/[one] visible steps in the model state\n"++
  "path                                  : show the visible path from the initial state\n"++
  "trace [proc|purp]                     : show the current trace [in PROCDEF|PURPDEF] format]\n"++
  "menu [in|out|purp] [<state>]          : give the [in|out] menu of actions of [current] <state>\n"++
  "\n"++
  "--------------------------------\n"++
  "ncomp                                 : test purpose generation via `N-Complete'-algorithm\n"++
  "lpe                                   : lpe transformation (Linear Process Equation)\n"++
  "lpeop <op> <lpe> <out>                : apply lpe operation <op> to model <lpe> and produce output named <out>\n"++
  "                                           stop       -> do nothing\n"++
  "                                           show       -> print the lpe to console\n"++
  "                                           export     -> generate torxakis file (do not change identifiers)\n"++
  "                                           export*    -> generate compilable torxakis file\n"++
  "                                           mcrl2      -> translate to mCRL2 specification file\n"++
  "                                           clean      -> remove duplicate/unreachable summands\n"++
  "                                           cstelm     -> remove parameters that never change value\n"++
  "                                           parelm     -> remove behavior-independent parameters\n"++
  "                                           istepelm   -> remove ISTEP actions (preserve weak bisimulation)\n"++
  "                                           datareset  -> reset parameters based on control flow graphs\n"++
  "                                           parreset   -> reset parameters based on summand reachability\n"++
  "                                           isdet      -> determine if the lpe is deterministic\n"++
  "                                           det        -> make the lpe deterministic\n"++
  "                                           uguard     -> add guards for underspecified summands\n"++
  "                                           angelic    -> make the lpe input-enabled (angelic completion)\n"++
  --"                                           confelm    -> statically prioritize invisible steps\n"++
  "                                           A->B       -> do two lpe operations in succession\n"++
  "                                           loop       -> repeat lpe operations until a fixpoint is reached\n"++
  "                                           loop*x     -> loop lpe operations x times or until a fixpoint is reached\n"++
  " merge <mod1> <mod2> <out>            : put two models in parallel and save it as a new model\n"++
  "--------------------------------\n"++
  "systart <name> <command>              : start external system <command> with internal <name>\n"++
  "systop  <name>                        : stop external command with internal <name>\n"++
  "<command> '$<' <file>                 : read command arguments from <file>\n"++
  "<command> args '$>' <file>            : write standard output of <command> to <file>\n"++
  "<command> args '$>>' <file>           : append standard output of <command> to <file>\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"


-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --
