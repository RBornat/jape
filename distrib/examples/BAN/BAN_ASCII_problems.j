/* $Id$ */ 
 
TACTIC TheoremForwardOrBackward(thm) IS
  WHEN (LETHYP _X cut (WITHSELECTIONS thm)) thm
  
CONJECTUREPANEL Conjectures IS 
  BUTTON Apply IS apply TheoremForwardOrBackward COMMAND

  THEOREM "Otway-Rees: S<|({Na,Nc}Kas,{Nb,Nc}Kbs) æ S|=-A|~(Na,Nc)" IS 
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, S|=-(A,B)êKab,
  		A|=-èk.S|Û(A,B)êk, B|=-èk.S|Û(A,B)êk, A|=-èx.S|ÛB|~x, B|=-èx.S|ÛA|~x, 
  		A|=-#Na, B|=-#Nb, A|=-#Nc,
  		S<|({Na,Nc}Kas,{Nb,Nc}Kbs)
  		æ S|=-A|~(Na,Nc) 
  THEOREM "Otway-Rees: S<|({Na,Nc}Kas,{Nb,Nc}Kbs) æ S|=-B|~(Nb,Nc)" IS 
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, S|=-(A,B)êKab,
  		A|=-èk.S|Û(A,B)êk, B|=-èk.S|Û(A,B)êk, A|=-èx.S|ÛB|~x, B|=-èx.S|ÛA|~x, 
  		A|=-#Na, B|=-#Nb, A|=-#Nc,
 		S<|({Na,Nc}Kas,{Nb,Nc}Kbs) 
 		æ S|=-B|~(Nb,Nc) 
  THEOREM "Otway-Rees: A<|{Na,(A,B)êKab,B|~Nc}Kas æ A|=-(A,B)êKab" IS 
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, S|=-(A,B)êKab,
  		A|=-èk.S|Û(A,B)êk, B|=-èk.S|Û(A,B)êk, A|=-èx.S|ÛB|~x, B|=-èx.S|ÛA|~x, 
  		A|=-#Na, B|=-#Nb, A|=-#Nc,
  		A<|{Na,(A,B)êKab,B|~Nc}Kas 
  		æ A|=-(A,B)êKab
  THEOREM "Otway-Rees: B<|({Na,(A,B)êKab,B|~Nc}Kas , {Nb,(A,B)êKab,A|~Nc}Kbs) æ B|=-(A,B)êKab" IS 
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, S|=-(A,B)êKab,
  		A|=-èk.S|Û(A,B)êk, B|=-èk.S|Û(A,B)êk, A|=-èx.S|ÛB|~x, B|=-èx.S|ÛA|~x, 
  		A|=-#Na, B|=-#Nb, A|=-#Nc,
  		B<|({Na,(A,B)êKab,B|~Nc}Kas , {Nb,(A,B)êKab,A|~Nc}Kbs) 
  		æ B|=-(A,B)êKab 
  THEOREM "Otway-Rees: A<|{Na,(A,B)êKab, B|~Nc}Kas æ A|=-B|=-Nc" IS 
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, S|=-(A,B)êKab,
  		A|=-(èk.S|Û(A,B)êk), B|=-(èk.S|Û(A,B)êk), A|=-èx.S|ÛB|~x, B|=-èx.S|ÛA|~x, 
  		A|=-#Na, B|=-#Nb, A|=-#Nc,
		A<|{Na,(A,B)êKab, B|~Nc}Kas 
		æ A|=-B|=-Nc 
  THEOREM "Otway-Rees: B<|({Na,(A,B)êKab,B|~Nc}Kas , {Nb,(A,B)êKab,A|~Nc}Kbs) æ B|=-A|~Nc" IS 
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, S|=-(A,B)êKab,
  		A|=-(èk.S|Û(A,B)êk), B|=-(èk.S|Û(A,B)êk), A|=-èx.S|ÛB|~x, B|=-èx.S|ÛA|~x, 
  		A|=-#Na, B|=-#Nb, A|=-#Nc,
		B<|({Na,(A,B)êKab,B|~Nc}Kas , {Nb,(A,B)êKab,A|~Nc}Kbs) 
  		æ B|=-A|~Nc 
 
  THEOREM "Needham-Schroeder: A<|{Na,(A,B)êKab,#((A,B)êKab),{(A,B)êKab}Kbs}Kas æ A|=-#((A,B)êKab)" IS 
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, S|=-(A,B)êKab,
		A|=-(èk.S|Û(A,B)êk), B|=-(èk.S|Û(A,B)êk), A|=-(èk.S|Û#((A,B)êk)), 
		A|=-#Na, B|=-#Nb, S|=-#((A,B)êKab), B|=-(èk.#((A,B)êk)), 
  		A<|{Na,(A,B)êKab,#((A,B)êKab),{(A,B)êKab}Kbs}Kas 
  		æ A|=-#((A,B)êKab) 
  THEOREM "Needham-Schroeder: A<|{Na,(A,B)êKab,#((A,B)êKab),{(A,B)êKab}Kbs}Kas æ A|=-(A,B)êKab" IS 
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, S|=-(A,B)êKab,
		A|=-(èk.S|Û(A,B)êk), B|=-(èk.S|Û(A,B)êk), A|=-(èk.S|Û#((A,B)êk)), 
		A|=-#Na, B|=-#Nb, S|=-#((A,B)êKab), B|=-(èk.#((A,B)êk)), 
		A<|{Na,(A,B)êKab,#((A,B)êKab),{(A,B)êKab}Kbs}Kas 
		æ A|=-(A,B)êKab 
  THEOREM "Needham-Schroeder: B<|{(A,B)êKab}Kbs æ B|=-(A,B)êKab" IS 
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, S|=-(A,B)êKab,
		A|=-(èk.S|Û(A,B)êk), B|=-(èk.S|Û(A,B)êk), A|=-(èk.S|Û#((A,B)êk)), 
		A|=-#Na, B|=-#Nb, S|=-#((A,B)êKab), B|=-(èk.#((A,B)êk)), 
		B<|{(A,B)êKab}Kbs 
		æ B|=-(A,B)êKab 
  THEOREM "Needham-Schroeder: A<|{Nb,(A,B)êKab}Kab æ A|=-B|=-(A,B)êKab" IS 
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, S|=-(A,B)êKab,
		A|=-(èk.S|Û(A,B)êk), B|=-(èk.S|Û(A,B)êk), A|=-(èk.S|Û#((A,B)êk)), 
		A|=-#Na, B|=-#Nb, S|=-#((A,B)êKab), B|=-(èk.#((A,B)êk)), 
  		A<|{Na,(A,B)êKab,#((A,B)êKab),{(A,B)êKab}Kbs}Kas, 
		A<|{Nb,(A,B)êKab}Kab 
		æ A|=-B|=-(A,B)êKab 
  THEOREM "Needham-Schroeder: B<|{Nb,(A,B)êKab}Kab æ B|=-A|=-(A,B)êKab" IS 
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, S|=-(A,B)êKab,
		A|=-(èk.S|Û(A,B)êk), B|=-(èk.S|Û(A,B)êk), A|=-(èk.S|Û#((A,B)êk)), 
		A|=-#Na, B|=-#Nb, S|=-#((A,B)êKab), B|=-(èk.#((A,B)êk)), 
		B<|{(A,B)êKab}Kbs, 
		B<|{Nb,(A,B)êKab}Kab 
		æ B|=-A|=-(A,B)êKab 
 
  THEOREM "Kerberos: A<|{Ts,(A,B)êKab,{Ts,(A,B)êKab}Kbs}Kas æ A|=-(A,B)êKab" IS
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, S|=-(A,B)êKab,
  		A|=-(èk.S|Û(A,B)êk), B|=-(èk.S|Û(A,B)êk), A|=-#Ts, B|=-#Ts, A|=-#Ta, B|=-#Ta,
  		A<|{Ts,(A,B)êKab,{Ts,(A,B)êKab}Kbs}Kas
  		æ A|=-(A,B)êKab
  THEOREM "Kerberos: B<|({Ts,(A,B)êKab}Kbs,{Ta,(A,B)êKab}Kab) æ B|=-A|=-(A,B)êKab" IS
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, S|=-(A,B)êKab,
  		A|=-(èk.S|Û(A,B)êk), B|=-(èk.S|Û(A,B)êk), A|=-#Ts, B|=-#Ts, A|=-#Ta, B|=-#Ta,
  		B<|({Ts,(A,B)êKab}Kbs,{Ta,(A,B)êKab}Kab)
  		æ B|=-A|=-(A,B)êKab
  THEOREM "Kerberos: A<|{Ta,(A,B)êKab}Kab æ A|=-B|=-(A,B)êKab" IS
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, S|=-(A,B)êKab,
  		A|=-(èk.S|Û(A,B)êk), B|=-(èk.S|Û(A,B)êk), A|=-#Ts, B|=-#Ts, A|=-#Ta, B|=-#Ta,
  		A<|{Ts,(A,B)êKab,{Ts,(A,B)êKab}Kbs}Kas,
  		A<|{Ta,(A,B)êKab}Kab
  		æ A|=-B|=-(A,B)êKab

  THEOREM "Frog: S<|{Ta,(A,B)êKab}Kas æ S|=-A|=-(A,B)êKab" IS
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, A|=-(A,B)êKab,
  		B|=-(èk.A|Û(A,B)êk), B|=-(èk.S|Û(A|=-(A,B)êk)), S|=-#Ta, B|=-#Ts,
  		S<|{Ta,(A,B)êKab}Kas
  		æ S|=-A|=-(A,B)êKab
  THEOREM "Frog: B<|{Ts,A|=-(A,B)êKab}Kbs æ B|=-(A,B)êKab" IS
  		A|=-(A,S)êKas, S|=-(A,S)êKas, B|=-(B,S)êKbs, S|=-(B,S)êKbs, A|=-(A,B)êKab,
  		B|=-(èk.A|Û(A,B)êk), B|=-(èk.S|Û(A|=-(A,B)êk)), S|=-#Ta, B|=-#Ts,
  		B<|{Ts,A|=-(A,B)êKab}Kbs 
  		æ B|=-(A,B)êKab
END 
