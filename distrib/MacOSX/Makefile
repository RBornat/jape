#
# $Id$
#
SERVERSOURCE=../java_japeserver
ENGINESOURCE=../../camlengine
#

all:         ; $(MAKE) engine GUIclean GUI

engine:      ; cd $(ENGINESOURCE); make jape

GUI:         ; cd $(SERVERSOURCE); pbxbuild -target japeserver build

clean:       ; $(MAKE) engineclean japeserverclean

engineclean: ; cd $(ENGINESOURCE); make clean

GUIclean:    ; cd $(SERVERSOURCE); pbxbuild -target japeserver clean
