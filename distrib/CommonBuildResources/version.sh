#!/bin/bash
# gets version number from git

export MAJOR=`git describe --tags --always --abbrev=0`; \
export MINOR=`git rev-list $MAJOR.. --count`; \
echo $MAJOR.$MINOR