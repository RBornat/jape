
%.cmi %.cmo : %.ml ;
	ocamlc $<

%.ml : ../engine/%.sig.sml ../engine/%.sml ;
	(camltran ../engine/$*.sig.sml; camltran ../engine/$*.sml) >$*.testml
	mv $*.testml $*.ml

%.ml : ../engine/%.sml ;
	camltran ../engine/$*.sml >$*.testml
	mv $*.testml $*.ml

camlcode: version.cmo miscellaneous.cmo

camlsource: version.ml miscellaneous.ml reason.ml extend.ml streamio.ml answer.ml prestring.ml hashstring.ml simplecache.ml listfuns.ml optionfuns.ml mappingfuns.ml stringfuns.ml box.ml displayfont.ml text.ml idclass.ml searchtree.ml symboltype.ml symbol.ml idclassfuns.ml term.ml predicate.ml match.ml binding.ml termparse.ml sequent.ml name.ml hashname.ml treelayout.ml tactic.ml proviso.ml paraparam.ml panelkind.ml menu.ml displayclass.ml alert.ml doubleclick.ml forcedef.ml proofstage.ml facts.ml rewinf.ml context.ml facts.ml substmapfuns.ml rewrite.ml provisofuns.ml thing.ml unify.ml treeformat.ml prooftree.ml proofstore.ml proofstate.ml applyrule.ml absprooftree.ml hit.ml draw.ml seqdraw.ml treedraw.ml boxdraw.ml displaystate.ml treedisplaystyle.ml boxdisplaystyle.ml selection.ml disproof.ml interaction.ml japeenv.ml env.ml tacticfuns.ml runproof.ml paragraphfuns.ml dialogue.ml
