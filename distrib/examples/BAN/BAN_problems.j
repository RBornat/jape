/* $Id$ */ 
 
TACTIC TheoremForwardOrBackward(thm) IS
  WHEN (LETHYP _X cut (WITHSELECTIONS thm)) thm
  
CONJECTUREPANEL Conjectures IS 
  PREFIXBUTTON Apply IS apply TheoremForwardOrBackward

  THEOREM "Otway-Rees: Së({Na,Nc}Kas,{Nb,Nc}Kbs) Ê SäAï(Na,Nc)" IS 
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Sä(A,B)ÍKab,
  		AäËk.Sö(A,B)Ík, BäËk.Sö(A,B)Ík, AäËx.SöBïx, BäËx.SöAïx, 
  		Aä#Na, Bä#Nb, Aä#Nc,
  		Së({Na,Nc}Kas,{Nb,Nc}Kbs)
  		Ê SäAï(Na,Nc) 
  THEOREM "Otway-Rees: Së({Na,Nc}Kas,{Nb,Nc}Kbs) Ê SäBï(Nb,Nc)" IS 
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Sä(A,B)ÍKab,
  		AäËk.Sö(A,B)Ík, BäËk.Sö(A,B)Ík, AäËx.SöBïx, BäËx.SöAïx, 
  		Aä#Na, Bä#Nb, Aä#Nc,
 		Së({Na,Nc}Kas,{Nb,Nc}Kbs) 
 		Ê SäBï(Nb,Nc) 
  THEOREM "Otway-Rees: Aë{Na,(A,B)ÍKab,BïNc}Kas Ê Aä(A,B)ÍKab" IS 
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Sä(A,B)ÍKab,
  		AäËk.Sö(A,B)Ík, BäËk.Sö(A,B)Ík, AäËx.SöBïx, BäËx.SöAïx, 
  		Aä#Na, Bä#Nb, Aä#Nc,
  		Aë{Na,(A,B)ÍKab,BïNc}Kas 
  		Ê Aä(A,B)ÍKab
  THEOREM "Otway-Rees: Bë({Na,(A,B)ÍKab,BïNc}Kas , {Nb,(A,B)ÍKab,AïNc}Kbs) Ê Bä(A,B)ÍKab" IS 
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Sä(A,B)ÍKab,
  		AäËk.Sö(A,B)Ík, BäËk.Sö(A,B)Ík, AäËx.SöBïx, BäËx.SöAïx, 
  		Aä#Na, Bä#Nb, Aä#Nc,
  		Bë({Na,(A,B)ÍKab,BïNc}Kas , {Nb,(A,B)ÍKab,AïNc}Kbs) 
  		Ê Bä(A,B)ÍKab 
  THEOREM "Otway-Rees: Aë{Na,(A,B)ÍKab, BïNc}Kas Ê AäBäNc" IS 
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Sä(A,B)ÍKab,
  		Aä(Ëk.Sö(A,B)Ík), Bä(Ëk.Sö(A,B)Ík), AäËx.SöBïx, BäËx.SöAïx, 
  		Aä#Na, Bä#Nb, Aä#Nc,
		Aë{Na,(A,B)ÍKab, BïNc}Kas 
		Ê AäBäNc 
  THEOREM "Otway-Rees: Bë({Na,(A,B)ÍKab,BïNc}Kas , {Nb,(A,B)ÍKab,AïNc}Kbs) Ê BäAïNc" IS 
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Sä(A,B)ÍKab,
  		Aä(Ëk.Sö(A,B)Ík), Bä(Ëk.Sö(A,B)Ík), AäËx.SöBïx, BäËx.SöAïx, 
  		Aä#Na, Bä#Nb, Aä#Nc,
		Bë({Na,(A,B)ÍKab,BïNc}Kas , {Nb,(A,B)ÍKab,AïNc}Kbs) 
  		Ê BäAïNc 
 
  THEOREM "Needham-Schroeder: Aë{Na,(A,B)ÍKab,#((A,B)ÍKab),{(A,B)ÍKab}Kbs}Kas Ê Aä#((A,B)ÍKab)" IS 
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Sä(A,B)ÍKab,
		Aä(Ëk.Sö(A,B)Ík), Bä(Ëk.Sö(A,B)Ík), Aä(Ëk.Sö#((A,B)Ík)), 
		Aä#Na, Bä#Nb, Sä#((A,B)ÍKab), Bä(Ëk.#((A,B)Ík)), 
  		Aë{Na,(A,B)ÍKab,#((A,B)ÍKab),{(A,B)ÍKab}Kbs}Kas 
  		Ê Aä#((A,B)ÍKab) 
  THEOREM "Needham-Schroeder: Aë{Na,(A,B)ÍKab,#((A,B)ÍKab),{(A,B)ÍKab}Kbs}Kas Ê Aä(A,B)ÍKab" IS 
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Sä(A,B)ÍKab,
		Aä(Ëk.Sö(A,B)Ík), Bä(Ëk.Sö(A,B)Ík), Aä(Ëk.Sö#((A,B)Ík)), 
		Aä#Na, Bä#Nb, Sä#((A,B)ÍKab), Bä(Ëk.#((A,B)Ík)), 
		Aë{Na,(A,B)ÍKab,#((A,B)ÍKab),{(A,B)ÍKab}Kbs}Kas 
		Ê Aä(A,B)ÍKab 
  THEOREM "Needham-Schroeder: Bë{(A,B)ÍKab}Kbs Ê Bä(A,B)ÍKab" IS 
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Sä(A,B)ÍKab,
		Aä(Ëk.Sö(A,B)Ík), Bä(Ëk.Sö(A,B)Ík), Aä(Ëk.Sö#((A,B)Ík)), 
		Aä#Na, Bä#Nb, Sä#((A,B)ÍKab), Bä(Ëk.#((A,B)Ík)), 
		Bë{(A,B)ÍKab}Kbs 
		Ê Bä(A,B)ÍKab 
  THEOREM "Needham-Schroeder: Aë{Nb,(A,B)ÍKab}Kab Ê AäBä(A,B)ÍKab" IS 
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Sä(A,B)ÍKab,
		Aä(Ëk.Sö(A,B)Ík), Bä(Ëk.Sö(A,B)Ík), Aä(Ëk.Sö#((A,B)Ík)), 
		Aä#Na, Bä#Nb, Sä#((A,B)ÍKab), Bä(Ëk.#((A,B)Ík)), 
  		Aë{Na,(A,B)ÍKab,#((A,B)ÍKab),{(A,B)ÍKab}Kbs}Kas, 
		Aë{Nb,(A,B)ÍKab}Kab 
		Ê AäBä(A,B)ÍKab 
  THEOREM "Needham-Schroeder: Bë{Nb,(A,B)ÍKab}Kab Ê BäAä(A,B)ÍKab" IS 
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Sä(A,B)ÍKab,
		Aä(Ëk.Sö(A,B)Ík), Bä(Ëk.Sö(A,B)Ík), Aä(Ëk.Sö#((A,B)Ík)), 
		Aä#Na, Bä#Nb, Sä#((A,B)ÍKab), Bä(Ëk.#((A,B)Ík)), 
		Bë{(A,B)ÍKab}Kbs, 
		Bë{Nb,(A,B)ÍKab}Kab 
		Ê BäAä(A,B)ÍKab 
 
  THEOREM "Kerberos: Aë{Ts,(A,B)ÍKab,{Ts,(A,B)ÍKab}Kbs}Kas Ê Aä(A,B)ÍKab" IS
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Sä(A,B)ÍKab,
  		Aä(Ëk.Sö(A,B)Ík), Bä(Ëk.Sö(A,B)Ík), Aä#Ts, Bä#Ts, Aä#Ta, Bä#Ta,
  		Aë{Ts,(A,B)ÍKab,{Ts,(A,B)ÍKab}Kbs}Kas
  		Ê Aä(A,B)ÍKab
  THEOREM "Kerberos: Bë({Ts,(A,B)ÍKab}Kbs,{Ta,(A,B)ÍKab}Kab) Ê BäAä(A,B)ÍKab" IS
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Sä(A,B)ÍKab,
  		Aä(Ëk.Sö(A,B)Ík), Bä(Ëk.Sö(A,B)Ík), Aä#Ts, Bä#Ts, Aä#Ta, Bä#Ta,
  		Bë({Ts,(A,B)ÍKab}Kbs,{Ta,(A,B)ÍKab}Kab)
  		Ê BäAä(A,B)ÍKab
  THEOREM "Kerberos: Aë{Ta,(A,B)ÍKab}Kab Ê AäBä(A,B)ÍKab" IS
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Sä(A,B)ÍKab,
  		Aä(Ëk.Sö(A,B)Ík), Bä(Ëk.Sö(A,B)Ík), Aä#Ts, Bä#Ts, Aä#Ta, Bä#Ta,
  		Aë{Ts,(A,B)ÍKab,{Ts,(A,B)ÍKab}Kbs}Kas,
  		Aë{Ta,(A,B)ÍKab}Kab
  		Ê AäBä(A,B)ÍKab

  THEOREM "Frog: Së{Ta,(A,B)ÍKab}Kas Ê SäAä(A,B)ÍKab" IS
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Aä(A,B)ÍKab,
  		Bä(Ëk.Aö(A,B)Ík), Bä(Ëk.Sö(Aä(A,B)Ík)), Sä#Ta, Bä#Ts,
  		Së{Ta,(A,B)ÍKab}Kas
  		Ê SäAä(A,B)ÍKab
  THEOREM "Frog: Bë{Ts,Aä(A,B)ÍKab}Kbs Ê Bä(A,B)ÍKab" IS
  		Aä(A,S)ÍKas, Sä(A,S)ÍKas, Bä(B,S)ÍKbs, Sä(B,S)ÍKbs, Aä(A,B)ÍKab,
  		Bä(Ëk.Aö(A,B)Ík), Bä(Ëk.Sö(Aä(A,B)Ík)), Sä#Ta, Bä#Ts,
  		Bë{Ts,Aä(A,B)ÍKab}Kbs 
  		Ê Bä(A,B)ÍKab
END 
