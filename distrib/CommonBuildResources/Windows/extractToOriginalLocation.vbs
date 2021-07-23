Option Explicit

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

WScript.Interactive = true
Wscript.Echo "Parent is " & objMyParent.ExecutablePath & "; GrandParent is " & objmyGrandParent.ExecutablePath

strGrandDir = """" & Left(objmyGrandParent.ExecutablePath, Len(objmyGrandParent.ExecutablePath)-Len("\JapeInstall.exe")) & """"
WScript.Echo "GrandDir is " & strGrandDir

strLocalAppData = objShell.ExpandEnvironmentStrings("%LOCALAPPDATA%")
strCopyCommand = "xcopy /y /s /e /i " & strLocalAppData & "\Jape\Jape.app\examples " & strGrandDir & "\examples"
WScript.Echo "command is " & strCopyCommand, 0, True
objShell.Run strCopyCommand, 0, True

WScript.Echo "how did that go?"
