#!/usr/bin/env bash

# claims to be, in bash, the way to find source directory.
# from https://stackoverflow.com/questions/59895/how-can-i-get-the-source-directory-of-a-bash-script-from-within-the-script-itsel/60157372#60157372
full_path_to_script="$(realpath "$0")"
scriptdir="$(dirname "$full_path_to_script")"
appdir="/app/Jape.app"
mkdir -p $appdir; rm -fr $appdir/*
cd $scriptdir
mv .data Jape.app
mv Jape.app $(dirname $appdir)
cp -pR $appdir/examples .
cp $appdir/README_RUN.html .
rm -fr runJape.sh;
cat <<ENDSCRIPT>uk.org.jape.desktop
[Desktop Entry]
Version=1.0
Name=Jape
Comment=Jape proof editor for Linux
Exec=/app/Jape.app/launchstub
Icon=uk.org.jape
Terminal=false
Type=Application
Categories=Development
StartupWMClass=uk-org-jape-Jape
ENDSCRIPT
mkdir -p /app/share/icons/hicolor/128x128/apps
mkdir -p /app/share/applications
cp $appdir/iconset/icon_128x128.png /app/share/icons/hicolor/128x128/apps/uk.org.jape.png
cp uk.org.jape.desktop /app/share/applications/uk.org.jape.desktop
rm installJape.sh README_INSTALL.html
