@ECHO OFF
SET TORXAKIS=torxakis.bat
SET REL=examps\stimulusresponse
SET EXAMPS=%1\%REL%
SET TEST=%1\test\%REL%

echo ------- Start StimulusResponse Test 1
start /min java -cp %EXAMPS% StimulusResponse
call %TORXAKIS% %EXAMPS%\StimulusResponse.txs < %TEST%\StimulusResponse.txscmd
TIMEOUT /T 10 /NOBREAK
move /Y testTrace testTrace.Spec.StimulusResponse.Sut.StimulusResponse
echo ------- End StimulusResponse Test 1

echo ------- Start StimulusResponse Test 2
start /min java -cp %EXAMPS% StimulusNoResponse
call %TORXAKIS% %EXAMPS%\StimulusResponse.txs < %TEST%\StimulusResponse.txscmd
TIMEOUT /T 10 /NOBREAK
move /Y testTrace testTrace.Spec.StimulusResponse.Sut.StimulusNoResponse
echo ------- End StimulusResponse Test 2

echo ------- Start StimulusResponse Test 3
start /min java -cp %EXAMPS% StimulusResponseLoop
call %TORXAKIS% %EXAMPS%\StimulusResponseLoop.txs < %TEST%\StimulusResponse.txscmd
TIMEOUT /T 10 /NOBREAK
move /Y testTrace testTrace.Spec.StimulusResponseLoop.Sut.StimulusResponseLoop
echo ------- End StimulusResponse Test 3

echo ------- Start StimulusResponse Test 4
start /min java -cp %EXAMPS% StimulusResponse
call %TORXAKIS% %EXAMPS%\StimulusResponseLoop.txs < %TEST%\StimulusResponse.txscmd
TIMEOUT /T 10 /NOBREAK
move /Y testTrace testTrace.Spec.StimulusResponseLoop.Sut.StimulusResponse
echo ------- End StimulusResponse Test 4

echo ------- Start StimulusResponse Test 5
start /min java -cp %EXAMPS% StimulusResponseLoop
call %TORXAKIS% %EXAMPS%\StimulusResponse.txs < %TEST%\StimulusResponse.txscmd
TIMEOUT /T 10 /NOBREAK
move /Y testTrace testTrace.Spec.StimulusResponse.Sut.StimulusResponseLoop
echo ------- End StimulusResponse Test 5
