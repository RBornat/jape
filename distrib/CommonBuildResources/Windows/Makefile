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
# Override the default targets (See Makefile-common for explanation)
#
ENGINETARGET=jape.exe

#
#       Send the icon, the README, and the specialised code in the jar
#
INSTALLRESOURCES=$(BUILDRESOURCES)/Windows/README-WINDOWS.txt           \
                 -autoicon $(BUILDRESOURCES)/japeicon.ico


wrap:   ; cp launchstub.bat Jape.app; \
          cp -pR ../../iconset Jape.app; \
		  cp -pR ../../examples Jape.app; \
		  powershell Compress-Archive -Force -Path Jape.app -DestinationPath Jape.app.zip; \
		  iexpress /N JapeInstall.SED

run:    ; Jape.app/launchstub.bat&
