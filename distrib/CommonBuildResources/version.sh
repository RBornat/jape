#!/bin/bash
# gets version number from git

export MAJOR=`git describe --tags --always --abbrev=0`; \
export MINOR=`git rev-list $MAJOR.. --count`; \
export CHANGE=`git status -s | egrep -c "^ [MARCD]"`; 
if [ $MINOR = "0"  ] && [ $CHANGE = "0"  ]; then echo $MAJOR; fi; \
if [ $MINOR != "0" ] && [ $CHANGE = "0"  ]; then echo $MAJOR[+"$MINOR"c]; fi
if [ $MINOR = "0"  ] && [ $CHANGE != "0" ]; then echo $MAJOR[+"$CHANGE"f]; fi
if [ $MINOR != "0" ] && [ $CHANGE != "0" ]; then echo $MAJOR[+"$MINOR"c][+"$CHANGE"f]; fi
