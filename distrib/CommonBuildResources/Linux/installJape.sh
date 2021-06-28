#!/bin/bash
# 
# This is the install script for Jape: a bunch of commands that move everything to its
# correct place.
# 
# To install Jape you must 'launch' this script. In some Linuxes (but not usually in Ubuntu)
# you can double-click the file. If double-clicking shows you this text, then there are two
# things you can try.
#
# 1. If you are on Ubuntu, and you'd like to tell it to launch shell scripts that you 
#    double-click, then paste the following command into a terminal window and press return:
#
#       gsettings set org.gnome.nautilus.preferences executable-text-activation 'launch'
#
#    and then try double-clicking this file again (it should launch, and install Jape).
# 
# 2. Otherwise, open a terminal window, change (cd) to the directory containing this script and
#    Jape.app, then type or paste the following command and press return:
#
#       bash installJape.sh
#
#    (it should launch this script and then install Jape).
#
# -----------------
# 
# Once Jape is installed, open the file README_HOW_TO_RUN_JAPE and follow the instructions.
# 
# You can close this window now.
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

