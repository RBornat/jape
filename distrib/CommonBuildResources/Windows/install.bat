SETLOCAL ENABLEEXTENSIONS
SET scriptpath=%~dp0
SET scriptpath=%scriptpath:~0,-1%
mkdir "%LOCALAPPDATA%"\Jape
rmdir /S /Q "%LOCALAPPDATA%"\Jape\Jape.app
set destpath=%LOCALAPPDATA%\Jape
set destpath=%destpath: =' '%
powershell Expand-Archive -Force -Path Jape.app.zip -DestinationPath %destpath%
echo Dim oWS, sLinkFile, oLink > CreateShortcut.vbs
echo Set oWS = WScript.CreateObject("WScript.Shell") >> CreateShortcut.vbs
echo sLinkFile = "Jape.lnk" >> CreateShortcut.vbs
echo Set oLink = oWS.CreateShortcut(sLinkFile) >> CreateShortcut.vbs
echo oLink.TargetPath = "%LOCALAPPDATA%\Jape\Jape.app\launchstub.bat" >> CreateShortcut.vbs
echo oLink.IconLocation = "%LOCALAPPDATA%\Jape\Jape.app\Jape.ico" >> CreateShortcut.vbs
echo oLink.WindowStyle = 7 >> CreateShortcut.vbs &REM minimised
echo oLink.Save >> CreateShortcut.vbs
wscript CreateShortcut.vbs
dir
wscript /e:VBScript extractToOriginalLocation.vbs /x
rmdir "%LOCALAPPDATA%"\Jape\Jape.app\examples
