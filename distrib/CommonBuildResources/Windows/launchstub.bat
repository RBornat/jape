SETLOCAL ENABLEEXTENSIONS
SET mypath=%~dp0
SET mypath=%mypath:~0,-1%
echo %mypath%
cd /D %mypath%
%mypath%\jre\bin\java -m uk.org.jape/uk.org.jape.Jape


