#
#       $Id$
#
#       Copyright (C) 2003-8 Richard Bornat & Bernard Sufrin
#    
#       richard@bornat.me.uk
#       sufrin@comlab.ox.ac.uk
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

BUILDDIR=../_buildengine$(OS)

# you must have ocamlbuild nowadays

jape_engine:: version;\
              mkdir -p $(BUILDDIR); \
              ocamlbuild -build-dir $(BUILDDIR) -lib unix -no-links main.native; \
              rm -f $(BUILDDIR)/jape_engine; ln -s main.native $(BUILDDIR)/jape_engine 

# windows ocaml 4.01 cannot do this (reports ''inconsistent assumptions''); but 4.02 and 4.00 work fine
jape.exe::     version ;\
               mkdir -p $(BUILDDIR); \
               ocamlbuild.native -classic-display -build-dir $(BUILDDIR) \
                      -lib unix -no-links \
                      main.native; \
               cp $(BUILDDIR)/main.native $(BUILDDIR)/jape.exe

# use git for version
version: ;              echo let version=\"`../CommonBuildResources/version.sh`\" >version.ml

# for Windows on the assumption that a Unix build already happened -- not used any more
#windowsversion:         version.ml ; echo "Re-using existing version.ml"

clean:;                 rm -fr $(BUILDDIR)  version.ml 
cleanall:;              rm -fr ../_buildengine* version.ml 



