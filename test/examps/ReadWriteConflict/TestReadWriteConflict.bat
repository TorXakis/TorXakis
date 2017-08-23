@REM TorXakis - Model Based Testing
@REM Copyright (c) 2015-2017 TNO and Radboud University
@REM See LICENSE at root directory of this repository.

@ECHO OFF
SET TORXAKIS=torxakis.bat
SET TORXAKISPORT=torxakisport.bat
SET REL=examps\ReadWriteConflict
SET EXAMPS=%1\%REL%
SET TEST=%1\test\%REL%

echo TORXAKIS %TORXAKIS%
echo REL %REL%
echo EXAMPS %EXAMPS%
echo TEST %TEST%


echo ------- Start ReadWrite Stepper Test
call %TORXAKIS% %EXAMPS%\ReadWrite.txs < %TEST%\ReadWrite_Stepper.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.ReadWrite.Sut.ReadWrite_Stepper.log
echo ------- End ReadWrite Stepper Test

echo ------- Start ReadWrite7 Stepper Test
call %TORXAKIS% %EXAMPS%\ReadWrite.txs < %TEST%\ReadWrite7_Stepper.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.ReadWrite.Sut.ReadWrite7_Stepper.log
echo ------- End ReadWrite7 Stepper Test

echo ------- Start ReadWriteAdvanced Stepper Test
call %TORXAKIS% %EXAMPS%\ReadWriteAdvanced.txs < %TEST%\ReadWrite_Stepper.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.ReadWriteAdvanced.Sut.ReadWrite_Stepper.log
echo ------- End ReadWriteAdvanced Stepper Test