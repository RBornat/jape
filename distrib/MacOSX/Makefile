#
# $Id$
#
GUISOURCE=../java_GUI
ENGINESOURCE=../camlengine

local:	 ; 	$(MAKE) engineclean engine GUIclean GUI

release:	 ; 	$(MAKE) engineclean OSXuniversal GUIclean GUI
				
OSXuniversal: ; cd $(ENGINESOURCE); $(MAKE) OSXuniversal

run:         ; open Jape.app

engine:      ; cd $(ENGINESOURCE); make jape_engine

GUI:         ; 	(cd $(GUISOURCE); ant OSXapp); \
				rm -fr Jape.app; mv $(GUISOURCE)/build/Jape.app .

clean:       ; $(MAKE) engineclean GUIclean; rm -fr Jape.app

engineclean: ; cd $(ENGINESOURCE); $(MAKE) clean

GUIclean:    ; cd $(GUISOURCE); ant clean
