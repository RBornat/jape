#
# $Id$
#
SERVERSOURCE=../java_japeserver
ENGINESOURCE=../../camlengine
BIN=.
#

all:    ; $(MAKE) engine GUI

engine:   ; cd $(ENGINESOURCE); make jape

GUI: ; cd $(SERVERSOURCE); pbxbuild -target japeserver build

clean:  ; rm -f $(BIN)/japeserver $(BIN)/japelogo.gif $(BIN)/jape;  \
          $(MAKE) engineclean japeserverclean

engineclean: ; cd $(ENGINESOURCE); make clean

GUIclean: ; cd $(SERVERSOURCE); pbxbuild -target japeserver clean
