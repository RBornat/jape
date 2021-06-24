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
              mkdir -p LinuxJape; rm -fr LinuxJape/*; \
              cp -pR installJape.sh Jape.app LinuxJape; \
              tar -zcf LinuxJape.tgz LinuxJape; \
              rm -fr LinuxJape
              
