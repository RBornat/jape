#!/bin/env bash
#
#       Build the build directories for all the Oxford-hosted Japes
#
#       Expects to be run from the top-level jape directory
#
JAPES="Linux Windows"
for jape in $JAPES
do
    mkdir ${jape}Jape ${jape}Jape/build
    cp CommonBuildResources/${jape}/Makefile ${jape}Jape/build
done


