%.cmi : %.mli ;
	ocamlc -g -c $<

%.cmi %.cmo : %.ml ;
	ocamlc -g -c $<

%.sig : %.ml %.cmo ;
	ocamlc -g -c -i $< >$*.sig
	
%.tml : ../engine/%.sig.sml ../engine/%.sml ;
	(camltran ../engine/$*.sig.sml; camltran ../engine/$*.sml) >$*.testml
	mv $*.testml $*.tml

%.tml : ../engine/%.sig.sml ../X/python/interface/%.sml ;
	(camltran ../engine/$*.sig.sml; camltran ../X/python/interface/$*.sml) >$*.testml
	mv $*.testml $*.tml

%.tml : ../engine/%.sml ;
	camltran ../engine/$*.sml >$*.testml
	mv $*.testml $*.tml

%.tml : ../engine/%.sig.sml ;
	camltran ../engine/$*.sig.sml >$*.testml
	mv $*.testml $*.tml

CAMLCODE = sml.cmo moresys.cmo version.cmo fix.cmo cache.cmo miscellaneous.cmo reason.cmo streamio.cmo answer.cmo prestring.cmo listfuns.cmo minwaste.cmo optionfuns.cmo mappingfuns.cmo stringfuns.cmo box.cmo displayfont.cmo text.cmo idclass.cmo searchtree.cmo symbol.cmo idclassfuns.cmo term.cmo predicate.cmo match.cmo binding.cmo termparse.cmo sequent.cmo name.cmo treelayout.cmo tactic.cmo proviso.cmo paraparam.cmo panelkind.cmo menu.cmo displayclass.cmo japeserver.cmo alert.cmo doubleclick.cmo forcedef.cmo usefile.cmo proofstage.cmo paragraph.cmo rewinf.cmo context.cmo facts.cmo facts.cmo substmapfuns.cmo rewrite.cmo provisofuns.cmo thing.cmo unify.cmo treeformat.cmo prooftree.cmo proofstore.cmo proofstate.cmo applyrule.cmo absprooftree.cmo hit.cmo draw.cmo seqdraw.cmo treedraw.cmo boxdraw.cmo displaystate.cmo displaystyle.cmo selection.cmo disproof.cmo interaction.cmo japeenv.cmo env.cmo button.cmo tacticfuns.cmo runproof.cmo paragraphfuns.cmo dialogue.cmo

jape: $(CAMLCODE) main.cmo
	ocamlc -g -o jape unix.cma $(CAMLCODE) main.cmo

depend: .depend
	ocamldep *.mli *.ml >.depend
	
include .depend