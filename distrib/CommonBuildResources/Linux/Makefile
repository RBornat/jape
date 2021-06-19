#
#       This include must come first
#
include ../../CommonBuildResources/Makefile-common
OS=Linux

run:        ; Jape.app/launchstub&

wrap:       ; cp launchstub Jape.app; \
              cp -pR ../../examples Jape.app; \
              cp -pR ../../iconset Jape.app; \
              cp post_install.sh Jape.app; \
              ./makeself.sh Jape.app InstallJape.sh "Jape installer" ./post_install.sh
