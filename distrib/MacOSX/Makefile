#
# $Id$
#
SERVERSOURCE=../java_GUI
ENGINESOURCE=../../camlengine
TARGETDIR=$(SERVERSOURCE)/build
XCODETARGET=Jape

all:         ; $(MAKE) engine GUIclean GUI symlink

run:         ; cd $(TARGETDIR); open Jape.app

engine:      ; cd $(ENGINESOURCE); make jape

GUI:         ; cd $(SERVERSOURCE); xcodebuild -target $(XCODETARGET) 

symlink:     ; cd $(TARGETDIR); rm -f jape_engine; ln -s ../../../camlengine/jape jape_engine

clean:       ; $(MAKE) engineclean japeserverclean

engineclean: ; cd $(ENGINESOURCE); make clean

GUIclean:    ; cd $(SERVERSOURCE); xcodebuild -target $(XCODETARGET) clean
