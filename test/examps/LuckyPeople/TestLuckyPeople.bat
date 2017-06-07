@ECHO OFF
REM where torxakis.bat
SET TORXAKISPORT=torxakisPort.bat
SET PORT=9876

SET ASSIGN=%1\examps\LuckyPeople\sut
SET SOL=%1\examps\LuckyPeople\spec
SET TEST=%1\test\examps\LuckyPeople

echo ------- Start LuckyPeople Test 1
start /min java -cp %ASSIGN% LuckyPeople
call %TORXAKISPORT% %PORT% %SOL%\LuckyPeople.txs < %TEST%\LuckyPeopleExamples.txscmd
TIMEOUT /T 10 /NOBREAK
move /Y .txs.%PORT%.err.log Examples.txs.%PORT%.err.log
echo ------- End LuckyPeople Test 1
echo ------- Start LuckyPeople Test 2
start /min java -cp %ASSIGN% LuckyPeople
call %TORXAKISPORT% %PORT% %SOL%\LuckyPeople.txs < %TEST%\LuckyPeopleRandom.txscmd
TIMEOUT /T 10 /NOBREAK
move /Y .txs.%PORT%.err.log Random.txs.%PORT%.err.log
echo ------- End LuckyPeople Test 2
