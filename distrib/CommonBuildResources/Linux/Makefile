#
#       This include must come first
#
include ../../CommonBuildResources/Makefile-common
OS=Linux

run:        ; Jape.app/launchstub&

wrap:       ; cp launchstub Jape.app; \
              cp AAA_README_INSTALL_JAPE Jape.app; \
              cp -pR ../../examples Jape.app; \
              cp -pR ../../iconset Jape.app; \
              cp post_install.sh Jape.app; \
              ./makeself.sh --tar-quietly Jape.app InstallJape.sh "Jape installer" ./post_install.sh
