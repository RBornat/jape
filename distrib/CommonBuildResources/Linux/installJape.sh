#!/bin/bash
# 
# If you can see this text, you have opened the installation control file.
#
# Please close this file, and open READ_ME_HOW_TO_INSTALL_JAPE.
#
# -----------------
















































# claims to be, in bash, the way to find source directory.
# from https://stackoverflow.com/questions/59895/how-can-i-get-the-source-directory-of-a-bash-script-from-within-the-script-itsel/60157372#60157372
full_path_to_script="$(realpath "$0")"
scriptdir="$(dirname "$full_path_to_script")"
appdir="$HOME/.local/share/Jape.app"
mkdir -p $appdir; rm -fr $appdir/*
mv Jape.app $(dirname $appdir)
cd $scriptdir
cp -pR $appdir/examples .
cp $appdir/README_HOW_TO_RUN_JAPE .
rm -fr Jape.sh; ln -s $appdir/launchstub Jape.sh
cat <<ENDSCRIPT>Jape.desktop
[Desktop Entry]
Version=1.0
Name=Jape
Comment=Jape proof editor for Linux
Exec=$appdir/launchstub
Icon=$appdir/Pics/japeicon.png
Terminal=false
Type=Application
Categories=Development
StartupWMClass=uk-org-jape-Jape
ENDSCRIPT
cat <<ENDSCRIPT>Japestart.desktop
[Desktop Entry]
Version=1.0
Name=Jape
Comment=Jape proof editor for Linux
Exec=$USERPWD/Jape
Icon=$appdir/Pics/japeicon.png
Terminal=false
Type=Application
Categories=Development
StartupWMClass=uk-org-jape-Jape
ENDSCRIPT
mv Jape.desktop Japestart.desktop ~/.local/share/applications
rm installJape.sh README_HOW_TO_INSTALL_JAPE

