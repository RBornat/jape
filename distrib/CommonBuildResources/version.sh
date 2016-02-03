#!/bin/bash
# gets version number from git

export MAJOR=`git describe --tags --always --abbrev=0`; \
export MINOR=`git rev-list $MAJOR.. --count`; \
if [ $MINOR = "0" ]; then echo $MAJOR; fi; \
if [ $MINOR != "0" ]; then echo $MINOR commits after $MAJOR; fi