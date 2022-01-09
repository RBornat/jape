Option Explicit

' adapted from https://stackoverflow.com/questions/13534699/iexpress-extraction-path

Dim objShell, objWMI
Dim objCmd, intMyPid, intMyParentPid, objMyParent, intmyGrandParentPid, objmyGrandParent, strGrandDir
Dim strLocalAppData, strCopyCommand

Set objShell = CreateObject("WScript.Shell")
Set objWMI = GetObject("winmgmts:root\cimv2")

Set objCmd = objShell.Exec("cmd.exe")
intMyPid = objWMI.Get("Win32_Process.Handle='" & objCmd.ProcessID & "'").ParentProcessId
objCmd.Terminate

intMyParentPid = objWMI.Get("Win32_Process.Handle='" & intMyPid & "'").ParentProcessId
Set objMyParent = objWMI.Get("Win32_Process.Handle='" & intMyParentPid & "'")

intmyGrandParentPid = objWMI.Get("Win32_Process.Handle='" & intMyParentPid & "'").ParentProcessId
Set objmyGrandParent = objWMI.Get("Win32_Process.Handle='" & intmyGrandParentPid & "'")

WScript.Interactive = False ' whether Echo works or not
Wscript.Echo "Parent is " & objMyParent.ExecutablePath & "; GrandParent is " & objmyGrandParent.ExecutablePath

strGrandDir = """" & Left(objmyGrandParent.ExecutablePath, InStrRev(objmyGrandParent.ExecutablePath, "JapeInstall", -1, vbTextCompare)-1) & """"
WScript.Echo "GrandDir is " & strGrandDir

strLocalAppData = """" & objShell.ExpandEnvironmentStrings("%LOCALAPPDATA%") & """"
strCopyCommand = "xcopy /y /s /e /i " & strLocalAppData & "\Jape\Jape.app\examples " & strGrandDir & "\examples"
WScript.Echo "command is " & strCopyCommand, 0, True
objShell.Run strCopyCommand, 0, True

objShell.Run "xcopy /y /s /e /i Jape.lnk " & strGrandDir

WScript.Echo "how did that go?"
