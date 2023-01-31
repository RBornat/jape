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
GUIDIR=../java_GUI
ENGINEDIR=../camlengine
JREBUNDLER=./JREBundler

%.html: %.md; pandoc $< -o $@

local:          ; $(MAKE) engine GUI bundler jre app
release:        ; $(MAKE) clean engine GUI bundler jre app README.html README_BUILD_MACOSX_JAPE.html
run:            ; open build/Jape.app
JARrun:         ; open build/JarJape/Jape.app

engine:         ; cd $(ENGINEDIR); make jape_engine OS=OSX

                # make app doesn't build the GUI or the jre bundle.
app:            ; export JAPEVERSION=`../CommonBuildResources/version.sh`; \
                  ant Jape.app -Dappversion='"'$$JAPEVERSION'"' -Djrebundler=$(JREBUNDLER)

JARapp:         ; export JAPEVERSION=`../CommonBuildResources/version.sh`; \
                  ant JARJape.app -Dappversion='"'$$JAPEVERSION'"'

                # jre doesn't build the GUI
jre:            ; cd $(GUIDIR); ant -Dtarget="OSX" -Dos="OSX" jre

                # GUI gets stuff ready for jre 
GUI:            ; export JAPEVERSION=`../CommonBuildResources/version.sh`; \
                  cd $(GUIDIR); ant -Dguiversion='"'$$JAPEVERSION'"' -Dtarget="OSX" uk.org.jape.jar

clean:          ; $(MAKE) engineclean GUIclean bundlerclean appclean htmlclean

engineclean:    ; cd $(ENGINEDIR); $(MAKE) clean OS=OSX
GUIclean:       ; cd $(GUIDIR); ant -Dtarget="OSX" -Dos="OSX" clean
guiclean:       ; ant clean; cd $(GUIDIR); ant clean
bundlerclean:   ; cd $(JREBUNDLER); ant clean
appclean:       ; ant clean
htmlclean:      ; rm *.html

bundler:        ; cd $(JREBUNDLER); ant jar






