/* $Id$ */ 
 
TACTIC TheoremForwardOrBackward(thm) IS
  WHEN (LETHYP _X cut (WITHSELECTIONS thm)) thm
  
CONJECTUREPANEL Conjectures IS 
  BUTTON Apply IS apply TheoremForwardOrBackward COMMAND

  THEOREM "Otway-Rees: S◁({Na,Nc}Kas,{Nb,Nc}Kbs) ⊢ S⫢A⇝(Na,Nc)" IS 
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, S⫢(A,B)↔Kab,
        A⫢∀k.Sö(A,B)↔k, B⫢∀k.Sö(A,B)↔k, A⫢∀x.SöB⇝x, B⫢∀x.SöA⇝x, 
        A⫢#Na, B⫢#Nb, A⫢#Nc,
        S◁({Na,Nc}Kas,{Nb,Nc}Kbs)
        ⊢ S⫢A⇝(Na,Nc) 
  THEOREM "Otway-Rees: S◁({Na,Nc}Kas,{Nb,Nc}Kbs) ⊢ S⫢B⇝(Nb,Nc)" IS 
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, S⫢(A,B)↔Kab,
        A⫢∀k.Sö(A,B)↔k, B⫢∀k.Sö(A,B)↔k, A⫢∀x.SöB⇝x, B⫢∀x.SöA⇝x, 
        A⫢#Na, B⫢#Nb, A⫢#Nc,
        S◁({Na,Nc}Kas,{Nb,Nc}Kbs) 
        ⊢ S⫢B⇝(Nb,Nc) 
  THEOREM "Otway-Rees: A◁{Na,(A,B)↔Kab,B⇝Nc}Kas ⊢ A⫢(A,B)↔Kab" IS 
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, S⫢(A,B)↔Kab,
        A⫢∀k.Sö(A,B)↔k, B⫢∀k.Sö(A,B)↔k, A⫢∀x.SöB⇝x, B⫢∀x.SöA⇝x, 
        A⫢#Na, B⫢#Nb, A⫢#Nc,
        A◁{Na,(A,B)↔Kab,B⇝Nc}Kas 
        ⊢ A⫢(A,B)↔Kab
  THEOREM "Otway-Rees: B◁({Na,(A,B)↔Kab,B⇝Nc}Kas , {Nb,(A,B)↔Kab,A⇝Nc}Kbs) ⊢ B⫢(A,B)↔Kab" IS 
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, S⫢(A,B)↔Kab,
        A⫢∀k.Sö(A,B)↔k, B⫢∀k.Sö(A,B)↔k, A⫢∀x.SöB⇝x, B⫢∀x.SöA⇝x, 
        A⫢#Na, B⫢#Nb, A⫢#Nc,
        B◁({Na,(A,B)↔Kab,B⇝Nc}Kas , {Nb,(A,B)↔Kab,A⇝Nc}Kbs) 
        ⊢ B⫢(A,B)↔Kab 
  THEOREM "Otway-Rees: A◁{Na,(A,B)↔Kab, B⇝Nc}Kas ⊢ A⫢B⫢Nc" IS 
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, S⫢(A,B)↔Kab,
        A⫢(∀k.Sö(A,B)↔k), B⫢(∀k.Sö(A,B)↔k), A⫢∀x.SöB⇝x, B⫢∀x.SöA⇝x, 
        A⫢#Na, B⫢#Nb, A⫢#Nc,
        A◁{Na,(A,B)↔Kab, B⇝Nc}Kas 
        ⊢ A⫢B⫢Nc 
  THEOREM "Otway-Rees: B◁({Na,(A,B)↔Kab,B⇝Nc}Kas , {Nb,(A,B)↔Kab,A⇝Nc}Kbs) ⊢ B⫢A⇝Nc" IS 
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, S⫢(A,B)↔Kab,
        A⫢(∀k.Sö(A,B)↔k), B⫢(∀k.Sö(A,B)↔k), A⫢∀x.SöB⇝x, B⫢∀x.SöA⇝x, 
        A⫢#Na, B⫢#Nb, A⫢#Nc,
        B◁({Na,(A,B)↔Kab,B⇝Nc}Kas , {Nb,(A,B)↔Kab,A⇝Nc}Kbs) 
        ⊢ B⫢A⇝Nc 
 
  THEOREM "Needham-Schroeder: A◁{Na,(A,B)↔Kab,#((A,B)↔Kab),{(A,B)↔Kab}Kbs}Kas ⊢ A⫢#((A,B)↔Kab)" IS 
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, S⫢(A,B)↔Kab,
        A⫢(∀k.Sö(A,B)↔k), B⫢(∀k.Sö(A,B)↔k), A⫢(∀k.Sö#((A,B)↔k)), 
        A⫢#Na, B⫢#Nb, S⫢#((A,B)↔Kab), B⫢(∀k.#((A,B)↔k)), 
        A◁{Na,(A,B)↔Kab,#((A,B)↔Kab),{(A,B)↔Kab}Kbs}Kas 
        ⊢ A⫢#((A,B)↔Kab) 
  THEOREM "Needham-Schroeder: A◁{Na,(A,B)↔Kab,#((A,B)↔Kab),{(A,B)↔Kab}Kbs}Kas ⊢ A⫢(A,B)↔Kab" IS 
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, S⫢(A,B)↔Kab,
        A⫢(∀k.Sö(A,B)↔k), B⫢(∀k.Sö(A,B)↔k), A⫢(∀k.Sö#((A,B)↔k)), 
        A⫢#Na, B⫢#Nb, S⫢#((A,B)↔Kab), B⫢(∀k.#((A,B)↔k)), 
        A◁{Na,(A,B)↔Kab,#((A,B)↔Kab),{(A,B)↔Kab}Kbs}Kas 
        ⊢ A⫢(A,B)↔Kab 
  THEOREM "Needham-Schroeder: B◁{(A,B)↔Kab}Kbs ⊢ B⫢(A,B)↔Kab" IS 
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, S⫢(A,B)↔Kab,
        A⫢(∀k.Sö(A,B)↔k), B⫢(∀k.Sö(A,B)↔k), A⫢(∀k.Sö#((A,B)↔k)), 
        A⫢#Na, B⫢#Nb, S⫢#((A,B)↔Kab), B⫢(∀k.#((A,B)↔k)), 
        B◁{(A,B)↔Kab}Kbs 
        ⊢ B⫢(A,B)↔Kab 
  THEOREM "Needham-Schroeder: A◁{Nb,(A,B)↔Kab}Kab ⊢ A⫢B⫢(A,B)↔Kab" IS 
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, S⫢(A,B)↔Kab,
        A⫢(∀k.Sö(A,B)↔k), B⫢(∀k.Sö(A,B)↔k), A⫢(∀k.Sö#((A,B)↔k)), 
        A⫢#Na, B⫢#Nb, S⫢#((A,B)↔Kab), B⫢(∀k.#((A,B)↔k)), 
        A◁{Na,(A,B)↔Kab,#((A,B)↔Kab),{(A,B)↔Kab}Kbs}Kas, 
        A◁{Nb,(A,B)↔Kab}Kab 
        ⊢ A⫢B⫢(A,B)↔Kab 
  THEOREM "Needham-Schroeder: B◁{Nb,(A,B)↔Kab}Kab ⊢ B⫢A⫢(A,B)↔Kab" IS 
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, S⫢(A,B)↔Kab,
        A⫢(∀k.Sö(A,B)↔k), B⫢(∀k.Sö(A,B)↔k), A⫢(∀k.Sö#((A,B)↔k)), 
        A⫢#Na, B⫢#Nb, S⫢#((A,B)↔Kab), B⫢(∀k.#((A,B)↔k)), 
        B◁{(A,B)↔Kab}Kbs, 
        B◁{Nb,(A,B)↔Kab}Kab 
        ⊢ B⫢A⫢(A,B)↔Kab 
 
  THEOREM "Kerberos: A◁{Ts,(A,B)↔Kab,{Ts,(A,B)↔Kab}Kbs}Kas ⊢ A⫢(A,B)↔Kab" IS
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, S⫢(A,B)↔Kab,
        A⫢(∀k.Sö(A,B)↔k), B⫢(∀k.Sö(A,B)↔k), A⫢#Ts, B⫢#Ts, A⫢#Ta, B⫢#Ta,
        A◁{Ts,(A,B)↔Kab,{Ts,(A,B)↔Kab}Kbs}Kas
        ⊢ A⫢(A,B)↔Kab
  THEOREM "Kerberos: B◁({Ts,(A,B)↔Kab}Kbs,{Ta,(A,B)↔Kab}Kab) ⊢ B⫢A⫢(A,B)↔Kab" IS
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, S⫢(A,B)↔Kab,
        A⫢(∀k.Sö(A,B)↔k), B⫢(∀k.Sö(A,B)↔k), A⫢#Ts, B⫢#Ts, A⫢#Ta, B⫢#Ta,
        B◁({Ts,(A,B)↔Kab}Kbs,{Ta,(A,B)↔Kab}Kab)
        ⊢ B⫢A⫢(A,B)↔Kab
  THEOREM "Kerberos: A◁{Ta,(A,B)↔Kab}Kab ⊢ A⫢B⫢(A,B)↔Kab" IS
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, S⫢(A,B)↔Kab,
        A⫢(∀k.Sö(A,B)↔k), B⫢(∀k.Sö(A,B)↔k), A⫢#Ts, B⫢#Ts, A⫢#Ta, B⫢#Ta,
        A◁{Ts,(A,B)↔Kab,{Ts,(A,B)↔Kab}Kbs}Kas,
        A◁{Ta,(A,B)↔Kab}Kab
        ⊢ A⫢B⫢(A,B)↔Kab

  THEOREM "Frog: S◁{Ta,(A,B)↔Kab}Kas ⊢ S⫢A⫢(A,B)↔Kab" IS
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, A⫢(A,B)↔Kab,
        B⫢(∀k.Aö(A,B)↔k), B⫢(∀k.Sö(A⫢(A,B)↔k)), S⫢#Ta, B⫢#Ts,
        S◁{Ta,(A,B)↔Kab}Kas
        ⊢ S⫢A⫢(A,B)↔Kab
  THEOREM "Frog: B◁{Ts,A⫢(A,B)↔Kab}Kbs ⊢ B⫢(A,B)↔Kab" IS
        A⫢(A,S)↔Kas, S⫢(A,S)↔Kas, B⫢(B,S)↔Kbs, S⫢(B,S)↔Kbs, A⫢(A,B)↔Kab,
        B⫢(∀k.Aö(A,B)↔k), B⫢(∀k.Sö(A⫢(A,B)↔k)), S⫢#Ta, B⫢#Ts,
        B◁{Ts,A⫢(A,B)↔Kab}Kbs 
        ⊢ B⫢(A,B)↔Kab
END 
