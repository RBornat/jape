#
#       Copyright (C) 2007-20 Richard Bornat and Bernard Sufrin
#    
#       richard@bornat.me.uk
#
#       This file is part of the Jape source distribution.
#       
#       Jape is free software; you can redistribute it and/or modify
#       it under the terms of the GNU General Public License as published by
#       the Free Software Foundation; either version 2 of the License, or
#       (at your option) any later version.
#       
#       Jape is distributed in the hope that it will be useful,
#       but WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#       GNU General Public License for more details.
#       
#       You should have received a copy of the GNU General Public License
#       along with jape; if not, write to the Free Software
#       Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#       (or look at http://www.gnu.org).
#
GUISOURCE=../java_GUI
ENGINESOURCE=../camlengine
JARBUNDLER=./JarBundler

local:          ; $(MAKE) engine GUI bundler app
release:        ; $(MAKE) clean engine GUI bundler app                          
run:            ; open build/Jape.app

engine:         ; cd $(ENGINESOURCE); make jape_engine OS=OSX

GUI:            ; export JAPEVERSION=`../CommonBuildResources/version.sh`; \
                  (cd $(GUISOURCE); echo `pwd`; ant -Dguiversion='"'$$JAPEVERSION'"' OSX.jar)

app:            ; export JAPEVERSION=`../CommonBuildResources/version.sh`; \
                  ant Jape.app -Dappversion='"'$$JAPEVERSION'"' -Djarbundler=$(JARBUNDLER)

clean:          ; $(MAKE) engineclean GUIclean bundlerclean appclean

engineclean:    ; cd $(ENGINESOURCE); $(MAKE) clean OS=OSX
GUIclean:       ; cd $(GUISOURCE); ant clean
guiclean:       ; ant clean; cd $(GUISOURCE); ant clean
bundlerclean:   ; cd $(JARBUNDLER); ant clean
appclean:       ; ant clean

bundler:        ; cd $(JARBUNDLER); ant jar






