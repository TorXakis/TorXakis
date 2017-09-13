@REM TorXakis - Model Based Testing
@REM Copyright (c) 2015-2017 TNO and Radboud University
@REM See LICENSE at root directory of this repository.

@ECHO OFF
SET TORXAKISPORT=torxakisPort.bat
SET PORT=9876

SET ASSIGN=%1\examps\LuckyPeople\sut
SET SOL=%1\examps\LuckyPeople\spec
SET TEST=%1\test\examps\LuckyPeople

echo ------- Start LuckyPeople Examples Purpose Test
start /min java -cp %ASSIGN% LuckyPeople
call %TORXAKISPORT% %PORT% %SOL%\LuckyPeople.txs < %TEST%\LuckyPeopleExamples_Purpose.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y .txs.%PORT%.err.log Examples_Purpose.txs.%PORT%.err.log
echo ------- End LuckyPeople Test Examples Purpose Test

echo ------- Start LuckyPeople Random Test
start /min java -cp %ASSIGN% LuckyPeople
call %TORXAKISPORT% %PORT% %SOL%\LuckyPeople.txs < %TEST%\LuckyPeopleRandom_Tester.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y .txs.%PORT%.err.log Random.txs.%PORT%.err.log
echo ------- End LuckyPeople Random Test

echo ------- Start LuckyPeople By Gender Test
start /min java -cp %ASSIGN% LuckyPeople
call %TORXAKISPORT% %PORT% %SOL%\LuckyPeople.txs < %TEST%\LuckyPeopleByGender_Purpose.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y .txs.%PORT%.err.log ByGender.txs.%PORT%.err.log
echo ------- End LuckyPeople By Gender Test
