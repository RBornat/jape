#!/usr/bin/env bash

# install dependencies
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak install flathub org.freedesktop.Platform//21.08 org.freedesktop.Sdk//21.08 
# org.freedesktop.Sdk.Extension.openjdk11//21.08 

# prepare workspace
export VERSION=`../../version.sh`
mkdir tmp
cp ../LinuxJape_$VERSION.tgz tmp/LinuxJape.tgz
cd tmp
tar zxvf LinuxJape.tgz
cp ../customInstallJape.sh LinuxJape/installJape.sh
cp ../uk.org.jape.yaml .

flatpak-builder --repo=japerepo build-dir uk.org.jape.yaml --force-clean
flatpak build-bundle japerepo ../jape_$VERSION.flatpak uk.org.jape
cd ..
rm -rf tmp

echo -e "\nresult bundle: $(pwd)/jape_$VERSION.flatpak"
