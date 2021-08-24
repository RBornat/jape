#
#       This include must come first
#
include ../../CommonBuildResources/Makefile-common
OS=Linux

run:        ; Jape.app/launchstub&

wrap:       ; chmod +x launchstub installJape.sh; \
              cp launchstub Jape.app; \
              cp README_RUN.html Jape.app; \
              cp -pR ../../examples Jape.app; \
              cp -pR ../../iconset Jape.app; \
              mkdir -p LinuxJape; rm -fr LinuxJape/*; \
              cp -pR installJape.sh README_INSTALL.html Jape.app LinuxJape; \
              mv LinuxJape/Jape.app LinuxJape/.data; \
              tar -zcf LinuxJape.tgz LinuxJape; \
              rm -fr LinuxJape
              
