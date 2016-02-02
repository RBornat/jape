#!/bin/bash
# usage: macdistrib (no argument)
# Run from MacOSX directory
# builds jape_<version> distribution from build/Jape.app and examples
VERSION=`../CommonBuildResources/version.sh`
DIR=jape_$VERSION
MANUALS=../../dev/doc/manuals

echo Making $DIR distribution

make release

# make the manuals

(cd $MANUALS/roll_your_own; latexmk -pdf roll_your_own; latexmk -c)
(cd $MANUALS/natural_deduction; latexmk -pdf natural_deduction_manual; latexmk -c)
(cd $MANUALS/forward_reasoning; latexmk -pdf forward_reasoning; latexmk -c)
(cd $MANUALS/disproof_howto; latexmk -pdf disproof_howto; latexmk -c)

rm -fr $DIR.dir $DIR.dmg
mkdir $DIR.dir

# rsync -l because Jape.app includes a symbolic link (recommended by jarbundler.jar docs)

rsync -rlvt build/Jape.app $DIR.dir
rsync -rvt  ../examples README.html $DIR.dir
rsync -rvt  ../CommonBuildResources/License.txt $DIR.dir
cp ../../License.txt $DIR.dir/GPLv2.0.txt
rsync -rvt  $MANUALS/roll_your_own/roll_your_own.pdf \
            $MANUALS/natural_deduction/natural_deduction_manual.pdf \
            $MANUALS/forward_reasoning/forward_reasoning.pdf \
            $MANUALS/disproof_howto/disproof_howto.pdf \
            $DIR.dir

sh makedmg $DIR








