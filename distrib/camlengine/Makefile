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

BUILDDIR=_build$(OS)

# for folks who don't have ocamlbuild, do it the old way

jape_engine:: ;\
	if [[ `which ocamlbuild` == "" ]]; then \
		make -f LegacyMakefile jape_engine; \
	else \
		ocamlbuild -build-dir $(BUILDDIR) -lib unix -no-links main.native; \
		rm -f $(BUILDDIR)/jape_engine; ln -s main.native $(BUILDDIR)/jape_engine ;\
	fi

# can't do ln -s on windoze, cos symbolic links only half work on Cygwin
jape.exe::	;\
	ocamlbuild.native -classic-display -build-dir $(BUILDDIR) -lib unix -no-links \
		-ocamlopt ocamlopt.opt -ocamlc ocamlc.opt main.native; \
	cp $(BUILDDIR)/main.native $(BUILDDIR)/jape.exe
	
#############################################################################
#
# For universal engines on OS X -- only on an Intel box.
# Requirements: ppc library in oPPCLIB, compiler in oPPCCOMPILER
#     (set that up by loading the ppc OCaml, copying the relevant files, then
#      loading the Intel OCaml. It works!)
#

oPPCCOMPILER=/usr/local/bin/ocamloptppc
oPPCLIB=/usr/local/lib/ocamlppc

OSXuniversal:: ;\
	make clean; \
	ocamlbuild -build-dir _build$(OS)/i386 -lib unix -no-links main.native; \
	ocamlbuild -build-dir _build$(OS)/ppc -lib unix -ocamlopt '$(oPPCCOMPILER) -ccopt "-arch ppc" -nostdlib -I $(oPPCLIB)' -no-links main.native; \
	lipo _build$(OS)/i386/main.native _build$(OS)/ppc/main.native -create -output _build$(OS)/jape_engine; \
	lipo -info _build$(OS)/jape_engine
	
#
#############################################################################

clean:;                 rm -fr _build$(OS) *.native *.cmi *.cmo *.cmx *.o jape_engine
cleanall:;				rm -fr _build* _log *.native

        
