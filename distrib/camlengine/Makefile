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

#
# Home of the engine bytecodes relative to the windows bytecode build file
#      (in WIN syntax, with backquote-insanity) 
#
WIN32CMORELATIVE= -I ..\\\\..\\\\camlengine\\\\

%.cmi : %.mli;          ocamlc $(CAMLDEBUG) -c $<

%.cmo : %.ml;           ocamlc $(CAMLDEBUG) -c $<

%.cmx : %.ml;           ocamlopt -c $<

%.sig : %.ml %.cmo;     ocamlc $(CAMLDEBUG) -c -i $< >$*.sig
        

CMO = sml.cmo moresys.cmo fix.cmo cache.cmo miscellaneous.cmo reason.cmo answer.cmo prestring.cmo listfuns.cmo version.cmo minwaste.cmo optionfuns.cmo mappingfuns.cmo stringfuns.cmo box.cmo displayfont.cmo text.cmo idclass.cmo searchtree.cmo symbol.cmo idclassfuns.cmo termtype.cmo termstring.cmo termstore.cmo termfuns.cmo predicate.cmo match.cmo binding.cmo termparse.cmo sequent.cmo name.cmo treelayout.cmo tactic.cmo proviso.cmo paraparam.cmo panelkind.cmo menu.cmo displayclass.cmo japeserver.cmo alert.cmo doubleclick.cmo forcedef.cmo usefile.cmo proofstage.cmo paragraph.cmo rewinf.cmo context.cmo facts.cmo substmapfuns.cmo rew.cmo rewrite.cmo provisofuns.cmo thing.cmo unify.cmo treeformat.cmo prooftree.cmo proofstore.cmo proofstate.cmo applyrule.cmo absprooftree.cmo hit.cmo draw.cmo seqdraw.cmo treedraw.cmo boxdraw.cmo displaystate.cmo displaystyle.cmo selection.cmo disproof.cmo interaction.cmo japeenv.cmo env.cmo button.cmo oracle.cmo tacticfuns.cmo runproof.cmo paragraphfuns.cmo dialogue.cmo
CMX = $(CMO:.cmo=.cmx)

jape:     $(CMO) main.cmo;   ocamlc $(CAMLDEBUG) -o jape unix.cma $(CMO) main.cmo
jape.opt: $(CMX) main.cmx;   ocamlopt -o jape.opt unix.cmxa $(CMX) main.cmx

#
#       This so we can do most of the work on a Unix machine with a single command
#
win32link.bat:  $(CMO) main.cmo ;\
                echo ocamlc -custom $(WIN32CMORELATIVE) $(CAMLDEBUG) -o win32jape.exe unix.cma $(CMO) main.cmo > win32link.bat


clean:;                 rm -f *.cmi *.cmo *.cmx *.o
        
depend: .depend;        ocamldep *.mli *.ml >.depend
        
include .depend

#
#       Superannuated 
#
%.tml : ../engine/%.sig.sml ../engine/%.sml ;\
        (camltran ../engine/$*.sig.sml; camltran ../engine/$*.sml) >$*.testml ;\
        mv $*.testml $*.tml

%.tml : ../engine/%.sig.sml ../X/python/interface/%.sml ;\
        (camltran ../engine/$*.sig.sml; camltran ../X/python/interface/$*.sml) >$*.testml ;\
        mv $*.testml $*.tml

%.tml : ../engine/%.sml ;\
        camltran ../engine/$*.sml >$*.testml ;\
        mv $*.testml $*.tml

%.tml : ../engine/%.sig.sml ;\
        camltran ../engine/$*.sig.sml >$*.testml ;\
        mv $*.testml $*.tml


