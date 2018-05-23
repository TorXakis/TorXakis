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
  "quit, q                          : stop TorXakis completely\n"++
  "exit, x                          : exit the current command run of TorXakis \n"++
  "help, h, ?                       : show help\n"++
  "info, i                          : show info on TorXakis\n"++
  "param [<parameter>]              : show value of <parameter>/[all parameters]\n"++
  "param <parameter> <value>        : set <parameter> to <value>\n"++
  "echo <text>                      : echo <text>\n"++
  "# <text>                         : comment       : <text> is ignored\n"++
  "seed <n>                         : set random seed to <n>\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"++
  "delay <n>                        : delay TorXakis for <n> seconds\n"++
  "time                             : give the current time\n"++
  "timer <name>                     : set or read timer <name>\n"++
  "run <file>                       : run the torxakis script from <file>\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"++
  "var [<variable-declarations>]    : show/[declare] variables\n"++
  "val [<value-definitions>]        : show/[define] values\n"++
  "eval <value-expression>          : evaluate the (closed) <value-expression>\n"++
  "solve <mode> <value-expression>  : solve the (open, boolean) <value-expression>\n"++
  "                                   mode is 'sat(isfaction)'; 'uni(que)'; 'ran(dom)'\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"++
  "tester <mod> [<map>] <cnect>     : start testing with model <mod>, connection <cnect>,\n"++
  "                                   and optionally mapper <map>\n"++
  "test act <action>                : do input test step on sut identified by <action>\n"++
  "test obs                         : observe output test step from sut\n"++
  "test offer <offer>               : do input test step on sut filtered by <offer>\n"++
  "test run [<n>]                   : do <n>/[unbounded] random test steps on sut\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"++
  "simulator <mod> [<map>] <cnect>  : start simulating with model <mod>, connection <cnect>\n"++
  "                                   and optionally mapper <map>\n"++
  "sim run [<n>]                    : do <n>/[unbounded] random simulation steps\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"++
  "stepper <mod>                    : start stepping with model <mod>\n"++
  "step act <action>                : make a step identified by <action>\n"++
  "step offer <offer>               : make a random step filtered by <offer>\n"++
  "step run [<n>]                   : make <n>/[one] random steps\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"++
  "manualor <cnect>                 : start manual mode with connection <cnect>\n"++
  "man act <action>                 : do input to world identified by <action>\n"++
  "man obs                          : observe output from world\n"++
  "man offer <offer>                : do input to world filtered by <offer>\n"++
  "man run [<n>]                    : do <n>/[unbounded] random steps on world\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"++
  "stop                             : stop tester, simulator, stepper, or manualor\n"++
  "menu [in|out]                    : give the [in|out] menu of offers of current state\n"++
  "trace [format]                   : show the current trace [in format]\n"++
  "graph                            : show the current transition graph\n"++
  "show <object>                    : show <object>: model, purp, mapper, cnect, procdef,\n"++
  "                                   tdefs, statenr, state\n"++
  "goto [<state>]                   : go to <state> number in the model\n"++
  "init                             : go to initial in the model\n"++
  "back [<n>]                       : go back <n>/[one] steps in the model\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"++
  "lpe <proc inst>                  : lpe transformation of <process instantiation>\n"++
  "ncomp <arg>                      : test purpose generation via `N-Complete'-algorithm\n"++
  "--------------------------------\n"++
  "<command> '$<' <file>            : read command arguments from <file>\n"++
  "<command> args '$>' <file>       : write standard output of <command> to <file>\n"++
  "<command> args '$>>' <file>      : append standard output of <command> to <file>\n"++
  "\n"++
  "--------------------------------\n"++
  "\n"


-- ----------------------------------------------------------------------------------------- --
--
-- ----------------------------------------------------------------------------------------- --

