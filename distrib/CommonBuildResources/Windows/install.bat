SETLOCAL ENABLEEXTENSIONS
SET scriptpath=%~dp0
SET scriptpath=%scriptpath:~0,-1%
mkdir %LOCALAPPDATA%\Jape
rmdir /S /Q %LOCALAPPDATA%\Jape\Jape.app
powershell Expand-Archive -Force -Path Jape.app.zip -DestinationPath %LOCALAPPDATA%\Jape
wscript extractToOriginalLocation.vbs /x /e:VBScript
rmdir %LOCALAPPDATA%\Jape\Jape.app\examples
@echo off
echo Set oWS = WScript.CreateObject("WScript.Shell") > CreateShortcut.vbs
echo sLinkFile = "%HOME%\"Jape examples"\Jape.lnk" >> CreateShortcut.vbs
echo Set oLink = oWS.CreateShortcut(sLinkFile) >> CreateShortcut.vbs
echo oLink.TargetPath = "%LOCALAPPDATA%\Jape\Jape.app\launchstub.bat" >> CreateShortcut.vbs
echo oLink.Save >> CreateShortcut.vbs
cscript CreateShortcut.vbs
