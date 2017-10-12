@REM TorXakis - Model Based Testing
@REM Copyright (c) 2015-2017 TNO and Radboud University
@REM See LICENSE at root directory of this repository.
@ECHO off

IF [%1]==[] GOTO InvalidNumberArguments		REM TARGET_DIR

set TARGET_DIR=%1

javac %~dp0/Copyright.java
java -cp %~dp0 Copyright %TARGET_DIR% 1
GOTO END

:InvalidNumberArguments
ECHO Usage:	FixCopyrightNotices TargetDir

:END