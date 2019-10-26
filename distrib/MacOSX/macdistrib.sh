#!/bin/bash
# usage: macdistrib (no argument)
# Run from MacOSX directory
# builds jape_<version> distribution from build/Jape.app and examples
VERSION=`../CommonBuildResources/version.sh`
DIR=jape_$VERSION
DOC=jape_manuals_$VERSION
#
MANUALS=../../dev/doc/manuals
HERE=`pwd`
SYNCMANUALSTO=$HERE/$DIR.dir/manuals

echo Making $DIR distribution

make release


rm -fr $DIR.dir $DIR.dmg $DOC
mkdir $DIR.dir 

# rsync -l because Jape.app includes a symbolic link (recommended by jarbundler.jar docs)
rsync -rlvt build/Jape.app $DIR.dir
rsync -rvt  ../examples README.html $DIR.dir
rsync -rvt  licences $DIR.dir

# make and sync the manuals
(cd $MANUALS; make SYNCTO=$SYNCMANUALSTO syncmanuals)

sh makedmg $DIR
rm -fr $DIR.dir



