Param(
  [string]$testName,
  [string]$specsPath,
  [string]$sutPath
)

$outputFilePath = "..\test\examps\$testName\Test$testName.bat"

"@ECHO OFF
SET TORXAKIS=torxakis.bat
SET REL=examps\$testName
SET EXAMPS=%1\%REL%\$specsPath
SET EXAMPSUTS=%1\%REL%\$sutPath
SET TEST=%1\test\%REL%

echo TORXAKIS %TORXAKIS%
echo REL %REL%
echo EXAMPS %EXAMPS%
echo TEST %TEST%
" > $outputFilePath


$testTemplate = 
"
echo ------- Start {0} Test
start /min java -cp %EXAMPSUTS% {1}
call %TORXAKIS% %EXAMPS%\{0}.txs < %TEST%\{1}.txscmd
TIMEOUT /T 4 /NOBREAK
move /Y testTrace.log testTrace.Spec.{0}.Sut.{1}.log
echo ------- End {0} Test
";

$files = ls $testName\$specsPath | foreach {$_.BaseName};

$files | foreach {
 $testTemplate -f $_, $testName >> $outputFilePath
};

