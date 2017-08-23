@REM TorXakis - Model Based Testing
@REM Copyright (c) 2015-2017 TNO and Radboud University
@REM See LICENSE at root directory of this repository.

@echo off

rem retrieve date and time NOTE: Locale dependent!!!
for /f "tokens=1,2,3,4 delims=-/ " %%a in ("%date%") do set wday=%%a&set month=%%c&set day=%%b&set year=%%d
for /f "tokens=1,2,3,4 delims=:," %%a in ("%time%") do set hours=%%a&set minutes=%%b&set seconds=%%c&set msec=%%d
set DateTime=%year%%month%%day%_%hours: =0%%minutes%%seconds%

cd datcheck

echo Start Development Acceptance Test
echo %DATE%%TIME%
call AcceptanceTest.bat > %DateTime%_DatOutput.datr 2>&1
echo End Development Acceptance Test
echo %DATE%%TIME%

echo Start Development Acceptance Check
echo %DATE%%TIME%
call AcceptanceCheck.bat %DateTime%_DatOutput.datr > %DateTime%_DatReport.datr
echo End Development Acceptance Check
echo %DATE%%TIME%

type %DateTime%_DatReport.datr

cd ..