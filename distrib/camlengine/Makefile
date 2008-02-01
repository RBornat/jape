#
#       $Id$
#
#       This file is part of the jape proof engine, which is part of jape.
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

jape_engine:: ;\
	ocamlbuild -lib unix -no-links main.native; \
	cp _build/main.native jape_engine

jape_exe::	;\
	ocamlbuild -classic-display -build-dir _windows -lib unix -no-links main.native; \
	cp _build/main.native jape.exe
	
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
	ocamlbuild -build-dir _buildi386 -lib unix -no-links main.native; \
	ocamlbuild -build-dir _buildppc -lib unix -ocamlopt '$(oPPCCOMPILER) -ccopt "-arch ppc" -nostdlib -I $(oPPCLIB)' -no-links main.native; \
	lipo _buildi386/main.native _buildppc/main.native -create -output jape_engine; \
	lipo -info jape_engine
	
#
#############################################################################

clean:;                 rm -fr _build* _log *.native jape_engine jape.exe
        
