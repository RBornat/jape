/* $Id$ */ 
 
TACTIC TheoremForwardOrBackward(thm) IS
  WHEN (LETHYP _X cut (WITHSELECTIONS thm)) thm
  
CONJECTUREPANEL Conjectures IS 
  BUTTON Apply IS apply TheoremForwardOrBackward COMMAND

  THEOREM "Otway-Rees: S<|({Na,Nc}Kas,{Nb,Nc}Kbs) æ S|éA|~(Na,Nc)" IS 
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, S|é(A,B)êKab,
  		A|éèk.S|Û(A,B)êk, B|éèk.S|Û(A,B)êk, A|éèx.S|ÛB|~x, B|éèx.S|ÛA|~x, 
  		A|é#Na, B|é#Nb, A|é#Nc,
  		S<|({Na,Nc}Kas,{Nb,Nc}Kbs)
  		æ S|éA|~(Na,Nc) 
  THEOREM "Otway-Rees: S<|({Na,Nc}Kas,{Nb,Nc}Kbs) æ S|éB|~(Nb,Nc)" IS 
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, S|é(A,B)êKab,
  		A|éèk.S|Û(A,B)êk, B|éèk.S|Û(A,B)êk, A|éèx.S|ÛB|~x, B|éèx.S|ÛA|~x, 
  		A|é#Na, B|é#Nb, A|é#Nc,
 		S<|({Na,Nc}Kas,{Nb,Nc}Kbs) 
 		æ S|éB|~(Nb,Nc) 
  THEOREM "Otway-Rees: A<|{Na,(A,B)êKab,B|~Nc}Kas æ A|é(A,B)êKab" IS 
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, S|é(A,B)êKab,
  		A|éèk.S|Û(A,B)êk, B|éèk.S|Û(A,B)êk, A|éèx.S|ÛB|~x, B|éèx.S|ÛA|~x, 
  		A|é#Na, B|é#Nb, A|é#Nc,
  		A<|{Na,(A,B)êKab,B|~Nc}Kas 
  		æ A|é(A,B)êKab
  THEOREM "Otway-Rees: B<|({Na,(A,B)êKab,B|~Nc}Kas , {Nb,(A,B)êKab,A|~Nc}Kbs) æ B|é(A,B)êKab" IS 
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, S|é(A,B)êKab,
  		A|éèk.S|Û(A,B)êk, B|éèk.S|Û(A,B)êk, A|éèx.S|ÛB|~x, B|éèx.S|ÛA|~x, 
  		A|é#Na, B|é#Nb, A|é#Nc,
  		B<|({Na,(A,B)êKab,B|~Nc}Kas , {Nb,(A,B)êKab,A|~Nc}Kbs) 
  		æ B|é(A,B)êKab 
  THEOREM "Otway-Rees: A<|{Na,(A,B)êKab, B|~Nc}Kas æ A|éB|éNc" IS 
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, S|é(A,B)êKab,
  		A|é(èk.S|Û(A,B)êk), B|é(èk.S|Û(A,B)êk), A|éèx.S|ÛB|~x, B|éèx.S|ÛA|~x, 
  		A|é#Na, B|é#Nb, A|é#Nc,
		A<|{Na,(A,B)êKab, B|~Nc}Kas 
		æ A|éB|éNc 
  THEOREM "Otway-Rees: B<|({Na,(A,B)êKab,B|~Nc}Kas , {Nb,(A,B)êKab,A|~Nc}Kbs) æ B|éA|~Nc" IS 
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, S|é(A,B)êKab,
  		A|é(èk.S|Û(A,B)êk), B|é(èk.S|Û(A,B)êk), A|éèx.S|ÛB|~x, B|éèx.S|ÛA|~x, 
  		A|é#Na, B|é#Nb, A|é#Nc,
		B<|({Na,(A,B)êKab,B|~Nc}Kas , {Nb,(A,B)êKab,A|~Nc}Kbs) 
  		æ B|éA|~Nc 
 
  THEOREM "Needham-Schroeder: A<|{Na,(A,B)êKab,#((A,B)êKab),{(A,B)êKab}Kbs}Kas æ A|é#((A,B)êKab)" IS 
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, S|é(A,B)êKab,
		A|é(èk.S|Û(A,B)êk), B|é(èk.S|Û(A,B)êk), A|é(èk.S|Û#((A,B)êk)), 
		A|é#Na, B|é#Nb, S|é#((A,B)êKab), B|é(èk.#((A,B)êk)), 
  		A<|{Na,(A,B)êKab,#((A,B)êKab),{(A,B)êKab}Kbs}Kas 
  		æ A|é#((A,B)êKab) 
  THEOREM "Needham-Schroeder: A<|{Na,(A,B)êKab,#((A,B)êKab),{(A,B)êKab}Kbs}Kas æ A|é(A,B)êKab" IS 
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, S|é(A,B)êKab,
		A|é(èk.S|Û(A,B)êk), B|é(èk.S|Û(A,B)êk), A|é(èk.S|Û#((A,B)êk)), 
		A|é#Na, B|é#Nb, S|é#((A,B)êKab), B|é(èk.#((A,B)êk)), 
		A<|{Na,(A,B)êKab,#((A,B)êKab),{(A,B)êKab}Kbs}Kas 
		æ A|é(A,B)êKab 
  THEOREM "Needham-Schroeder: B<|{(A,B)êKab}Kbs æ B|é(A,B)êKab" IS 
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, S|é(A,B)êKab,
		A|é(èk.S|Û(A,B)êk), B|é(èk.S|Û(A,B)êk), A|é(èk.S|Û#((A,B)êk)), 
		A|é#Na, B|é#Nb, S|é#((A,B)êKab), B|é(èk.#((A,B)êk)), 
		B<|{(A,B)êKab}Kbs 
		æ B|é(A,B)êKab 
  THEOREM "Needham-Schroeder: A<|{Nb,(A,B)êKab}Kab æ A|éB|é(A,B)êKab" IS 
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, S|é(A,B)êKab,
		A|é(èk.S|Û(A,B)êk), B|é(èk.S|Û(A,B)êk), A|é(èk.S|Û#((A,B)êk)), 
		A|é#Na, B|é#Nb, S|é#((A,B)êKab), B|é(èk.#((A,B)êk)), 
  		A<|{Na,(A,B)êKab,#((A,B)êKab),{(A,B)êKab}Kbs}Kas, 
		A<|{Nb,(A,B)êKab}Kab 
		æ A|éB|é(A,B)êKab 
  THEOREM "Needham-Schroeder: B<|{Nb,(A,B)êKab}Kab æ B|éA|é(A,B)êKab" IS 
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, S|é(A,B)êKab,
		A|é(èk.S|Û(A,B)êk), B|é(èk.S|Û(A,B)êk), A|é(èk.S|Û#((A,B)êk)), 
		A|é#Na, B|é#Nb, S|é#((A,B)êKab), B|é(èk.#((A,B)êk)), 
		B<|{(A,B)êKab}Kbs, 
		B<|{Nb,(A,B)êKab}Kab 
		æ B|éA|é(A,B)êKab 
 
  THEOREM "Kerberos: A<|{Ts,(A,B)êKab,{Ts,(A,B)êKab}Kbs}Kas æ A|é(A,B)êKab" IS
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, S|é(A,B)êKab,
  		A|é(èk.S|Û(A,B)êk), B|é(èk.S|Û(A,B)êk), A|é#Ts, B|é#Ts, A|é#Ta, B|é#Ta,
  		A<|{Ts,(A,B)êKab,{Ts,(A,B)êKab}Kbs}Kas
  		æ A|é(A,B)êKab
  THEOREM "Kerberos: B<|({Ts,(A,B)êKab}Kbs,{Ta,(A,B)êKab}Kab) æ B|éA|é(A,B)êKab" IS
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, S|é(A,B)êKab,
  		A|é(èk.S|Û(A,B)êk), B|é(èk.S|Û(A,B)êk), A|é#Ts, B|é#Ts, A|é#Ta, B|é#Ta,
  		B<|({Ts,(A,B)êKab}Kbs,{Ta,(A,B)êKab}Kab)
  		æ B|éA|é(A,B)êKab
  THEOREM "Kerberos: A<|{Ta,(A,B)êKab}Kab æ A|éB|é(A,B)êKab" IS
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, S|é(A,B)êKab,
  		A|é(èk.S|Û(A,B)êk), B|é(èk.S|Û(A,B)êk), A|é#Ts, B|é#Ts, A|é#Ta, B|é#Ta,
  		A<|{Ts,(A,B)êKab,{Ts,(A,B)êKab}Kbs}Kas,
  		A<|{Ta,(A,B)êKab}Kab
  		æ A|éB|é(A,B)êKab

  THEOREM "Frog: S<|{Ta,(A,B)êKab}Kas æ S|éA|é(A,B)êKab" IS
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, A|é(A,B)êKab,
  		B|é(èk.A|Û(A,B)êk), B|é(èk.S|Û(A|é(A,B)êk)), S|é#Ta, B|é#Ts,
  		S<|{Ta,(A,B)êKab}Kas
  		æ S|éA|é(A,B)êKab
  THEOREM "Frog: B<|{Ts,A|é(A,B)êKab}Kbs æ B|é(A,B)êKab" IS
  		A|é(A,S)êKas, S|é(A,S)êKas, B|é(B,S)êKbs, S|é(B,S)êKbs, A|é(A,B)êKab,
  		B|é(èk.A|Û(A,B)êk), B|é(èk.S|Û(A|é(A,B)êk)), S|é#Ta, B|é#Ts,
  		B<|{Ts,A|é(A,B)êKab}Kbs 
  		æ B|é(A,B)êKab
END 
