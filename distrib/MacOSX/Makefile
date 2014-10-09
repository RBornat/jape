#
# $Id$
#
#       Copyright (C) 2007-9 Richard Bornat
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

local:   ;      $(MAKE) engineclean engine GUIclean GUI6
release: ;      $(MAKE) engineclean engine GUIclean GUI6                              
run:     ;      open build/Jape.app

local7:   ;      $(MAKE) engineclean engine GUIclean GUI7
release7: ;      $(MAKE) engineclean engine GUIclean GUI7                              
run7:     ;      open build1.7/Jape.app

engine:      ; cd $(ENGINESOURCE); make jape_engine OS=OSX

GUI:         ;  export JAVA_HOME=`/usr/libexec/java_home -v 1.6` ; (cd $(GUISOURCE); echo `pwd`; ant OSX.jar); ant Jape.app

GUI7:        ;  export JAVA_HOME=`/usr/libexec/java_home -v 1.7` ; (cd $(GUISOURCE); echo `pwd`; ant OSX.jar); ant Jape-1.7.app
GUI6:        ;  export JAVA_HOME=`/usr/libexec/java_home -v 1.6` ; (cd $(GUISOURCE); echo `pwd`; ant OSX.jar); ant Jape.app

clean:       ; $(MAKE) engineclean GUIclean

engineclean: ; cd $(ENGINESOURCE); $(MAKE) clean OS=OSX

GUIclean:    ; ant clean; cd $(GUISOURCE); ant clean
guiclean:    ; ant clean; cd $(GUISOURCE); ant clean






