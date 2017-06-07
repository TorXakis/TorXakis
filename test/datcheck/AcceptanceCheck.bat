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

