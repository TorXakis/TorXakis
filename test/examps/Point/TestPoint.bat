REM TorXakis - Model Based Testing
REM Copyright (c) 2015-2017 TNO and Radboud University
REM See LICENSE at root directory of this repository.

@ECHO OFF
SET TORXAKIS=torxakis.bat
SET TORXAKISPORT=torxakisport.bat
SET REL=examps\Point
SET EXAMPS=%1\%REL%
SET TEST=%1\test\%REL%

echo TORXAKIS %TORXAKIS%
echo REL %REL%
echo EXAMPS %EXAMPS%
echo TEST %TEST%


echo ------- Start Point Stepper Test
call %TORXAKIS% %EXAMPS%\Point.txs < %TEST%\Point_Stepper.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Point.Sut.Point_Stepper.log
echo ------- End Point Stepper Test

REM echo ------- Start Point Simulator Test
REM start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\Point.txs < %TEST%\Point_Simulator.txscmd
REM call %TORXAKIS% %EXAMPS%\Point.txs < %TEST%\Point_Tester.txscmd
REM TIMEOUT /T 8 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.Point.Sut.Point_Simulator.log
REM echo ------- End Point Simulator Test

echo ------- Start Point_Purpose Stepper Test
call %TORXAKIS% %EXAMPS%\Point.txs < %TEST%\Point_Purpose_Stepper.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.Point.Sut.Point_Purpose_Stepper.log
echo ------- End Point_Purpose Stepper Test

REM echo ------- Start Point Simulator Test
REM start "" /min /b call %TORXAKISPORT% 9877 %EXAMPS%\Point.txs < %TEST%\Point_Purpose_Simulator.txscmd
REM call %TORXAKIS% %EXAMPS%\Point.txs < %TEST%\Point_Purpose_Tester.txscmd
REM TIMEOUT /T 8 /NOBREAK
REM move /Y testTrace.log testTrace.Spec.Point.Sut.Point_Purpose_Simulator.log
REM echo ------- End Point Simulator Test
