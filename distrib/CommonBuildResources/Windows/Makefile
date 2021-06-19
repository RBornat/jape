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
          cp -pR ../../iconset Jape.app

run:    ; Jape.app/launchstub.bat&
