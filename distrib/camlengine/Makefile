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

#CAMLDEBUG=-g
CAMLDEBUG = 
oCOMPILE = ocamlopt
oLOAD = ocamlopt

%.cmi : %.mli;          ocamlc $(CAMLDEBUG) -c $<

%.cmo : %.ml;           ocamlc $(CAMLDEBUG) -c $<

%.cmx : %.ml;           $(oCOMPILE) -c $<

%.sig : %.ml %.cmo;     ocamlc $(CAMLDEBUG) -c -i $< >$*.sig
        

CMO = sml.cmo moresys.cmo fix.cmo cache.cmo miscellaneous.cmo reason.cmo answer.cmo prestring.cmo listfuns.cmo version.cmo optionfuns.cmo mappingfuns.cmo stringfuns.cmo uTF.cmo invisibles.cmo minwaste.cmo box.cmo displayfont.cmo text.cmo idclass.cmo searchtree.cmo symbol.cmo idclassfuns.cmo termtype.cmo termstring.cmo termstore.cmo termfuns.cmo predicate.cmo match.cmo binding.cmo termparse.cmo seqtype.cmo sequent.cmo name.cmo treelayout.cmo tactic.cmo provisotype.cmo proviso.cmo paraparam.cmo panelkind.cmo menu.cmo displayclass.cmo japeserver.cmo alert.cmo doubleclick.cmo forcedef.cmo usefile.cmo proofstage.cmo paragraph.cmo rewinf.cmo cxttype.cmo cxtstring.cmo cxtfuns.cmo cxtprovisos.cmo cxtexterior.cmo facts.cmo substmapfuns.cmo rew.cmo rewrite.cmo provisofuns.cmo thing.cmo unify.cmo treeformat.cmo prooftree.cmo proofstore.cmo proofstate.cmo applyrule.cmo absprooftree.cmo hit.cmo draw.cmo seqdraw.cmo termfold.cmo treedraw.cmo boxdraw.cmo displaystate.cmo displaystyle.cmo selection.cmo disproof.cmo interaction.cmo japeenv.cmo env.cmo button.cmo oracle.cmo tacticfuns.cmo runproof.cmo paragraphfuns.cmo dialogue.cmo

CMX = $(CMO:.cmo=.cmx)

# Obsolete: we'll use compiled code
# jape_engine:     $(CMO) main.cmo;   ocamlc $(SOURCES) $(CAMLDEBUG) -custom -o jape_engine unix.cma $(CMO) main.cmo
#
jape_engine:     $(CMX) main.cmx; $(oLOAD) $(SOURCES) -o jape_engine unix.cmxa $(CMX) main.cmx

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

#############################################################################
#
#  For the windows build
#
#  We don't actually make this target, because we're running on a Unix. 
#  We do a make -n on it to make a batch file that can be run on Windows. 

#       This constructs an interpreted version 
#       Obsoleted by the native-code version below.
#
#jape.exe: $(CMO) main.cmo;\
#          ocamlc $(SOURCES) $(CAMLDEBUG) -custom -o jape.exe unix.cma $(CMO) main.cmo

#
#
#       This constructs a compiled native-code version
#        * ocamlopt needs to be installed properly
#          and supplemented with an assembler (ml) and linker (link)
#        * These can be gotten by downloading and installing masm32, then
#          copying masm32/bin/{ml.exe,ml.err,link.exe} into the ocaml bin directory
#       (BS/RB 22 Mar 2005)    
#
jape.exe: $(CMX) main.cmx; ocamlopt $(SOURCES) -o jape.exe unix.cmxa $(CMX) main.cmx

makejape.bat:: ;\
   echo cd tmp                                                 >  ../makejape.bat;\
   make -n jape.exe | egrep -v "Entering|Leaving|up to date"   >> ../makejape.bat;\
   echo cd ..                                                  >> ../makejape.bat;\
   echo NOW RUN makejape.bat ON WINDOZE AND TYPE CR WHEN THROUGH;\
   read ANSWER             
#
#
#
#############################################################################

clean:;                 rm -fr *.cmi *.cmo *.cmx *.o _build* _log *.native
        
depend: .depend;        ocamldep *.mli *.ml >.depend
        
include .depend




