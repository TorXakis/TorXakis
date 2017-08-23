@REM TorXakis - Model Based Testing
@REM Copyright (c) 2015-2017 TNO and Radboud University
@REM See LICENSE at root directory of this repository.

@echo off

set PORTNR=%1

set RESTVAR=
shift
:loop1
if "%1"=="" goto after_loop
set RESTVAR=%RESTVAR% %1
shift
goto loop1

:after_loop
start /min torxakisServer.bat %PORTNR%
txsui %PORTNR% %RESTVAR%