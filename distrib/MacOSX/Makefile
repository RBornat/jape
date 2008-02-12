#
# $Id$
#
GUISOURCE=../java_GUI
ENGINESOURCE=../camlengine

local:	 ; 	$(MAKE) engineclean engine GUIclean GUI

release:	 ; 	$(MAKE) engineclean OSXuniversal GUIclean GUI
				
OSXuniversal: ; cd $(ENGINESOURCE); $(MAKE) OSXuniversal OS=OSX

run:         ; open build/Jape.app

engine:      ; cd $(ENGINESOURCE); make jape_engine OS=OSX

GUI:         ; 	(cd $(GUISOURCE); ant OSX.jar); ant Jape.app

clean:       ; $(MAKE) engineclean GUIclean

engineclean: ; cd $(ENGINESOURCE); $(MAKE) clean OS=OSX

GUIclean:    ; ant clean; cd $(GUISOURCE); ant clean
