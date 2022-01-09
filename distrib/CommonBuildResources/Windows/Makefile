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
APPEXTRAS=launchstub.bat Jape.ico
#
# Override the default targets (See Makefile-common for explanation)
#
ENGINETARGET=jape.exe


wrap: app; cp -pR ../../examples Jape.app; \
	      powershell Compress-Archive -Force -Path Jape.app -DestinationPath Jape.app.zip; \
	      iexpress /N JapeInstall.SED; \
	      mv JapeInstall.exe JapeInstall_`$(BUILDRESOURCES)/version.sh`.exe

run:    ; Jape.app/launchstub.bat&
