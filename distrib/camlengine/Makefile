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
CAMLDEBUG= 

%.cmi : %.mli;          ocamlc $(CAMLDEBUG) -c $<

%.cmo : %.ml;           ocamlc $(CAMLDEBUG) -c $<

%.cmx : %.ml;           ocamlopt -c $<

%.sig : %.ml %.cmo;     ocamlc $(CAMLDEBUG) -c -i $< >$*.sig
        

CMO = sml.cmo moresys.cmo fix.cmo cache.cmo miscellaneous.cmo reason.cmo answer.cmo prestring.cmo listfuns.cmo version.cmo optionfuns.cmo mappingfuns.cmo stringfuns.cmo uTF.cmo invisibles.cmo minwaste.cmo box.cmo displayfont.cmo text.cmo idclass.cmo searchtree.cmo symbol.cmo idclassfuns.cmo termtype.cmo termstring.cmo termstore.cmo termfuns.cmo predicate.cmo match.cmo binding.cmo termparse.cmo seqtype.cmo sequent.cmo name.cmo treelayout.cmo tactic.cmo provisotype.cmo proviso.cmo paraparam.cmo panelkind.cmo menu.cmo displayclass.cmo japeserver.cmo alert.cmo doubleclick.cmo forcedef.cmo usefile.cmo proofstage.cmo paragraph.cmo rewinf.cmo cxttype.cmo cxtstring.cmo cxtfuns.cmo cxtprovisos.cmo cxtexterior.cmo facts.cmo substmapfuns.cmo rew.cmo rewrite.cmo provisofuns.cmo thing.cmo unify.cmo treeformat.cmo prooftree.cmo proofstore.cmo proofstate.cmo applyrule.cmo absprooftree.cmo hit.cmo draw.cmo seqdraw.cmo termfold.cmo treedraw.cmo boxdraw.cmo displaystate.cmo displaystyle.cmo selection.cmo disproof.cmo interaction.cmo japeenv.cmo env.cmo button.cmo oracle.cmo tacticfuns.cmo runproof.cmo paragraphfuns.cmo dialogue.cmo

CMX = $(CMO:.cmo=.cmx)

jape_engine:     $(CMO) main.cmo;   ocamlc   $(SOURCES) $(CAMLDEBUG) -custom -o jape_engine unix.cma $(CMO) main.cmo
jape_engine.opt: $(CMX) main.cmx;   ocamlopt $(SOURCES) -o jape_engine.opt unix.cmxa $(CMX) main.cmx; cp jape_engine.opt jape_engine

#############################################################################
#
#  For the windows build at Oxford
#
#  We don't actually make this target, because we're running on a Unix. 
#  We do a make -n on it to make a batch file that can be run on Windows. 
#
jape.exe: $(CMO) main.cmo;\
          ocamlc $(SOURCES) $(CAMLDEBUG) -custom -o jape.exe unix.cma $(CMO) main.cmo

makejape.bat:: ;\
               rsync -vt /users/sufrin/WINDOWS-C-RESOURCES/* .;\
               echo cd tmp                                      >  ../makejape.bat;\
               make -n jape.exe | egrep -v "Entering|Leaving|up to date"   >> ../makejape.bat;\
               echo cd ..                                       >> ../makejape.bat;\
               echo NOW RUN makejape.bat ON WINDOZE AND TYPE CR WHEN THROUGH;\
               read ANSWER             
#
#
#
#############################################################################

clean:;                 rm -f *.cmi *.cmo *.cmx *.o
        
depend: .depend;        ocamldep *.mli *.ml >.depend
        
include .depend
