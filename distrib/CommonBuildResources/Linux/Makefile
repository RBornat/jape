#
#       This include must come first
#
include ../../CommonBuildResources/Makefile-common

OS=Linux
APPEXTRAS=launchstub

run:        ; Jape.app/launchstub&

wrap: app README_RUN.html README_INSTALL.html ; \
              chmod +x launchstub installJape.sh; \
              mkdir -p LinuxJape; rm -fr LinuxJape/*; \
              cp -pR installJape.sh README_INSTALL.html Jape.app LinuxJape; \
              cp README_RUN.html LinuxJape/Jape.app; \
              cp -pR ../../examples LinuxJape/Jape.app; \
              mv LinuxJape/Jape.app LinuxJape/.data; \
              tar -zcf LinuxJape_`../version.sh`.tgz LinuxJape

# at present 21.08 is baked into this build -- here and in uk.org.jape.yaml. Hmm.
flatpak: app flatpak/README_INSTALL_FLATPAKJAPE.html ; \
            flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo; \
            flatpak install flathub org.freedesktop.Platform//21.08 org.freedesktop.Sdk//21.08; \
            mkdir tmp; rm -fr tmp/*; \
            cd tmp; mkdir LinuxJape; \
            cp -pR ../flatpak/installJape.sh ../Jape.app LinuxJape; cp ../flatpak/launchstub LinuxJape/Jape.app; \
            cp ../flatpak/uk.org.jape.yaml .; \
            flatpak-builder --repo=japerepo build-dir uk.org.jape.yaml --force-clean; \
            flatpak build-bundle japerepo ../jape.flatpak uk.org.jape; \
            cd ..; rm -rf tmp; \
            mkdir -p FlatpakJape; rm -fr FlatpakJape/*; \
            cp -pR flatpak/README_INSTALL_FLATPAKJAPE.html flatpak/prepare_flatpak.sh ../../examples jape.flatpak FlatpakJape; \
            tar -zcf FlatpakJape_`../version.sh`.tgz FlatpakJape

