/* $Id$ */

CONJECTUREPANEL Conjectures IS
  THEOREMS "Otway-Rees" ARE
  		Sä(A,S)ÍKas, Së({Na+Nc}Kas+{Nb+Nc}Kbs) Ê SäAï(Na+Nc)
  AND	Sä(B,S)ÍKbs, Së({Na+Nc}Kas+{Nb+Nc}Kbs) Ê SäBï(Nb+Nc)
  AND	Aä(A,S)ÍKas, Ëk.AäSö(A,B)Ík, Aä#Na, Aë{Na+(A,B)ÍKab+BïNc}Kas Ê Aä(A,B)ÍKab
  AND	Bä(B,S)ÍKbs, Ëk.BäSö(A,B)Ík, Bä#Nb, Bë({Na+(A,B)ÍKab+BïNc}Kas + {Nb+(A,B)ÍKab+AïNc}Kbs) Ê Bä(A,B)ÍKab
  AND	Aä(A,S)ÍKas, Ëx.AäSöBïx, Aä#Na, Aä#Nc, Aë{Na+(A,B)ÍKab+ BïNc}Kas Ê AäBäNc
  AND	Bä(B,S)ÍKbs, Ëx.BäSöAïx, Aä#Na, Bä#Nb, Bë({Na+(A,B)ÍKab+BïNc}Kas + {Nb+(A,B)ÍKab+AïNc}Kbs) Ê BäAïNc
  END
  THEOREMS "Needham-Schroeder" ARE
  		Aä(A,S)ÍKas, Ëk.AäSö(A,B)Ík, Aë{Na+(A,B)ÍKab+#((A,B)ÍKab)+{(A,B)ÍKab}Kbs}Kas Ê Aä(A,B)ÍKab
  AND	Aä(A,S)ÍKas, Ëk.AäSö#((A,B)Ík), Aë{Na+(A,B)ÍKab+#((A,B)ÍKab)+{(A,B)ÍKab}Kbs}Kas Ê Aä#((A,B)ÍKab)
  AND	BÍ(B,S)ÍKbs, Bä#((A,B)ÍKab), Bë{(A,B)ÍKab}Kbs Ê Bä(A,B)ÍKab
  AND	Aä(A,S)ÍKas, Ëk.AäSö(A,B)Ík, Aë{Na+(A,B)ÍKab+#((A,B)ÍKab)+{(A,B)ÍKab}Kbs}Kas, Aë{Nb+(A,B)ÍKab}Kab Ê AäBä(A,B)ÍKab
  AND	BÍ(B,S)ÍKbs, Bä#((A,B)ÍKab), Bë{(A,B)ÍKab}Kbs, Bë{Nb+(A,B)ÍKab}Kab Ê BäAä(A,B)ÍKab
  END
  /* THEOREMS Kerberos ARE
  		Aë{Ts+(A,B)ÍKab+{Ts,(A,B)ÍKab}Kbs}Kas
  		Bë({Ts,(A,B)ÍKab}Kbs+{Ta,(A,B)ÍKab}Kab from A)
  		Aë{Ta,(A,B)ÍKab} from B
  END */
END
