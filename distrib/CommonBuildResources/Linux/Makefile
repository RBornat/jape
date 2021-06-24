#
#       This include must come first
#
include ../../CommonBuildResources/Makefile-common
OS=Linux

run:        ; Jape.app/launchstub&

wrap:       ; chmod +x launchstub installJape.sh; \
              cp launchstub Jape.app; \
              cp README_HOW_TO_RUN_JAPE Jape.app; \
              cp -pR ../../examples Jape.app; \
              cp -pR ../../iconset Jape.app; \
              tar -zcf LinuxJape.tgz installJape.sh Jape.app
              
