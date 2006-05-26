#
# $Id$
#
SERVERSOURCE=java_GUI
ENGINESOURCE=../camlengine
XCODETARGET=Jape

all:         ; $(MAKE) engine GUIclean GUI symlink

run:         ; open Development/Jape.app

engine:      ; cd $(ENGINESOURCE); make jape_engine

GUI:         ; cd $(SERVERSOURCE); xcodebuild -target $(XCODETARGET) -configuration Development SYMROOT=.. OBJROOT=..

symlink:     ; cd Development; rm -f jape_engine; ln -s ../../camlengine/jape_engine

clean:       ; $(MAKE) engineclean GUIclean

engineclean: ; cd $(ENGINESOURCE); $(MAKE) clean

GUIclean:    ; cd $(SERVERSOURCE); xcodebuild -target $(XCODETARGET) -configuration Development clean SYMROOT=.. OBJROOT=..
