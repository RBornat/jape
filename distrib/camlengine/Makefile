%.cmi %.cmo : %.ml ;
	ocamlc -c $<

%.sig : %.ml ;
	ocamlc -c -i $< >$*.sig
	
%.tml : ../engine/%.sig.sml ../engine/%.sml ;
	(camltran ../engine/$*.sig.sml; camltran ../engine/$*.sml) >$*.testml
	mv $*.testml $*.tml

%.tml : ../engine/%.sig.sml ../X/python/interface/%.sml ;
	(camltran ../engine/$*.sig.sml; camltran ../X/python/interface/%.sml) >$*.testml
	mv $*.testml $*.tml

%.tml : ../engine/%.sml ;
	camltran ../engine/$*.sml >$*.testml
	mv $*.testml $*.tml

%.tml : ../engine/%.sig.sml ;
	camltran ../engine/$*.sig.sml >$*.testml
	mv $*.testml $*.tml

camlcode: SML.cmo version.cmo miscellaneous.cmo reason.cmo extend.cmo streamio.cmo answer.cmo prestring.cmo listfuns.cmo optionfuns.cmo mappingfuns.cmo stringfuns.cmo box.cmo displayfont.cmo text.cmo idclass.cmo searchtree.cmo symbol.cmo idclassfuns.cmo term.cmo predicate.cmo match.cmo binding.cmo termparse.cmo sequent.cmo name.cmo treelayout.cmo tactic.cmo proviso.cmo paraparam.cmo panelkind.cmo menu.cmo displayclass.cmo alert.cmo japeserver.cmo doubleclick.cmo forcedef.cmo proofstage.cmo facts.cmo rewinf.cmo context.cmo facts.cmo substmapfuns.cmo rewrite.cmo provisofuns.cmo thing.cmo unify.cmo treeformat.cmo prooftree.cmo proofstore.cmo proofstate.cmo applyrule.cmo absprooftree.cmo hit.cmo draw.cmo screendraw.cmi seqdraw.cmo treedraw.cmo boxdraw.cmo displaystate.cmo displaystyle.cmo selection.cmo disproof.cmo interaction.cmo japeenv.cmo env.cmo tacticfuns.cmo runproof.cmo paragraphfuns.cmo dialogue.cmo

camlsource: version.tml miscellaneous.tml reason.tml extend.tml streamio.tml answer.tml prestring.tml listfuns.tml optionfuns.tml mappingfuns.tml stringfuns.tml box.tml displayfont.tml text.tml idclass.tml searchtree.tml symbol.tml idclassfuns.tml term.tml predicate.tml match.tml binding.tml termparse.tml sequent.tml name.tml treelayout.tml tactic.tml proviso.tml paraparam.tml panelkind.tml menu.tml displayclass.tml alert.tml japeserver.tml button.tml doubleclick.tml forcedef.tml proofstage.tml facts.tml rewinf.tml context.tml facts.tml substmapfuns.tml rewrite.tml provisofuns.tml thing.tml unify.tml treeformat.tml prooftree.tml proofstore.tml proofstate.tml applyrule.tml absprooftree.tml hit.tml draw.tml seqdraw.tml treedraw.tml boxdraw.tml displaystate.tml selection.tml disproof.tml interaction.tml japeenv.tml env.tml tacticfuns.tml runproof.tml paragraphfuns.tml dialogue.tml

# CAMLSOURCE = SML.ml version.ml miscellaneous.ml reason.ml extend.ml streamio.ml answer.ml prestring.ml listfuns.ml optionfuns.ml mappingfuns.ml stringfuns.ml box.ml displayfont.ml text.ml idclass.ml searchtree.ml symbol.ml idclassfuns.ml term.ml predicate.ml match.ml binding.ml termparse.ml sequent.ml name.ml treelayout.ml tactic.ml proviso.ml paraparam.ml panelkind.ml menu.ml displayclass.ml japeserver.ml button.ml alert.ml doubleclick.ml forcedef.ml proofstage.ml facts.ml rewinf.ml context.ml facts.ml substmapfuns.ml rewrite.ml provisofuns.ml thing.ml unify.ml treeformat.ml prooftree.ml proofstore.ml proofstate.ml applyrule.ml absprooftree.ml hit.ml draw.ml seqdraw.ml treedraw.ml boxdraw.ml displaystate.ml selection.ml disproof.ml interaction.ml japeenv.ml env.ml tacticfuns.ml runproof.ml paragraphfuns.ml dialogue.ml

depend: .depend
	ocamldep *.mli *.ml >.depend
	
include .depend