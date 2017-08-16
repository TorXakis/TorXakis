REM TorXakis - Model Based Testing
REM Copyright (c) 2015-2017 TNO and Radboud University
REM See LICENSE at root directory of this repository.

@echo off

REM
REM move to torxakis directory
REM

rem cd %~dp0%
rem cd ..\test\datcheck

java -cp . DatChecker %1


REM
REM return to call location
REM

