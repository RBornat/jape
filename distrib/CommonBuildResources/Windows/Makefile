#
# $Id$
#
#
# 
#
components::  ; (cd ../../CommonBuildResources/; make -f Makefile-java installjape.jar)
#
#
#
include ../../CommonBuildResources/Makefile-common
#
#
#
OS=Windows
#
# Override the special targets
#
ENGINETARGET=makejape.bat
BUILTENGINE=jape.exe
DEPLOYEDENGINE=jape.exe
README=README-WINDOWS 
INSTALLME=installjape.class
DEPLOYMENTKIT=$(README) japeicon.ico
                

jarfile:  ;\
            ln -s ../../examples .;\
            java bootstrap.install $(OS)jape.jar\
                   -classwindows installjape installjape.class \
                   -cmdwindows   'SHORTCUT.EXE -f -t jape.cmd -n Jape -d . -i japeicon.ico' \
                   -app          "Windows Jape" \
                   -splash       japesplash.png \
                   SHORTCUT.EXE  $(DEPLOYEDENGINE) japeserver.jar $(DEPLOYMENTKIT) examples



