#
# $Id$
#
#
#       This include must come first
#
#
include ../../CommonBuildResources/Makefile-common
#
#
#
#
OS=Windows
#
# Override the special targets (See Makefile-common for explanation)
#
#       ENGINETARGET=makejape.bat 
#               because we make a windows batch file on Unix
#               then execute it on Windows
#               rather than trying to get a Windows make that works consistently
#               with GNU-make
#               -- the details of the batch file make are in ocamlengine/Makefile
#       
ENGINETARGET=makejape.bat
BUILTENGINE=jape.exe
DEPLOYEDENGINE=jape.exe

#
#       Make the appropriate installation script file (with installjape.class)
#       then run it as a command. This two-phase approach is complicated, but
#       works reliably on lots of different variants of windows.
#
AFTERINSTALL= -classwindows installjape -cmdwindows '"%INSTALL%installjape.cmd" "%INSTALL%"'

#
#       Send the icon, the README, and the specialised code in the jar
#
INSTALLRESOURCES=$(BUILDRESOURCES)/README-WINDOWS.txt           \
                 $(BUILDRESOURCES)/japeicon.ico                 \
                 $(BUILDRESOURCES)/InstallerKit/SHORTCUT.EXE    \
                 $(BUILDRESOURCES)/installjape.class 

APP="Windows Jape"






