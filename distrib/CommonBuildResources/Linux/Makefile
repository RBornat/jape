#
#       This include must come first
#
include ../../CommonBuildResources/Makefile-common
OS=Linux
APPEXTRAS=launchstub

run:        ; Jape.app/launchstub&

wrap:       ; chmod +x launchstub installJape.sh; \
              cp launchstub Jape.app; \
              cp README_RUN.html Jape.app; \
              cp -pR ../../examples Jape.app; \
              mkdir -p LinuxJape; rm -fr LinuxJape/*; \
              cp -pR installJape.sh README_INSTALL.html Jape.app LinuxJape; \
              mv LinuxJape/Jape.app LinuxJape/.data; \
              tar -zcf LinuxJape_$(../version.sh).tgz LinuxJape; \
              rm -fr LinuxJape
              
