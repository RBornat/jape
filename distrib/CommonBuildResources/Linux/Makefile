#
# $Id$
#

include ../../CommonBuildResources/Makefile-common
OS=Linux

wrap:		; cp launchstub Jape.app; \
		  cp -pR ../../examples Jape.app; \
		  cp post_install.sh Jape.app; \
		  ./makeself.sh Jape.app InstallJape.sh "Jape installer" ./post_install.sh


