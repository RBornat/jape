/* $Id$ */

CONSTANT pass fail mod

CONJECTUREPANEL "Programs"  IS
  THEOREM IS 
    {i=2} (i:=i+1) {i=3}
  THEOREM WHERE i NOTIN j,t AND j NOTIN t IS 
    {i=Ki∧j=Kj} (t:=i; i:=j; j:=t) {i=Kj∧j=Ki}
  THEOREM WHERE i NOTIN j IS 
    {i=Ki∧j=Kj} (i:=j; j:=i) {i=Kj∧j=Ki}
  THEOREM WHERE i NOTIN j,k AND j NOTIN k IS
    {j=Kj∧k=Kk} if j>k then i:=j else i:= k fi {j=Kj∧k=Kk∧(j>k→i=Kj)∧(k≥j→i=Kk)}
  THEOREM WHERE i NOTIN j,k AND j NOTIN k IS
    {j=Kj∧k=Kk} if j≥k then i:=j else i:= k fi {j=Kj∧k=Kk∧(j≥k→i=Kj)∧(k≥j→i=Kk)}
  THEOREM WHERE i NOTIN r IS 
    {i=Ki} if i≥40 then r:=pass else r:=fail fi {i=Ki∧(i<40→r=fail)∧(i≥40→r=pass)}
  THEOREM (OBJECT x) WHERE i NOTIN n IS
    {n≥2} 
      (i:=2; while n mod i ≠ 0 do i:=i+1 od) 
    {2≤i∧i≤n∧∀x.(2≤x ∧ x<i → n mod x ≠0) ∧ n mod i = 0}
  THEOREM WHERE i NOTIN prime,n AND prime NOTIN n IS
    {2≤i∧i≤n∧∀x.(2≤x ∧ x<i → n mod x ≠0) ∧ n mod i = 0}
      (prime := i=n)
    {prime ↔¬(∃y.(2≤y∧y<n∧n mod y = 0))}
  THEOREM WHERE i NOTIN prime,n AND prime NOTIN n IS
    {2≤i∧i≤n∧∀x.(2≤x ∧ x<i → n mod x ≠0) ∧ n mod i = 0}
      (prime := i=n)
    {prime ↔∀y.(2≤y∧y<n → n mod y ≠ 0)}
  THEOREM WHERE i NOTIN j,k AND j NOTIN k IS
    {i=Ki∧j=Kj∧i≥0}
      (k:=0; while i≠0 do k:=k+j; i:=i-1 od)
    {k=Ki×Kj}
  THEOREM WHERE i NOTIN j,kq,kr AND j NOTIN kq,kr AND kq NOTIN kr IS
    {i=Ki∧j=Kj∧i≥0∧j>0}
      (kq:=0; kr:=i; while kr≥j do kr:=kr-j; kq:=kq+1 od)
    {kq×Kj+kr=Ki∧0≤kr∧kr<Kj}
  THEOREM WHERE i NOTIN j,k AND j NOTIN k IS
    {i=Ki∧j=Kj∧i≥0}
      (k:=0;
       while i≠0 do
         if i mod 2=1 then k:=k+j else skip fi;
         i:=i÷2; j:=j×2
       od)
    {k=Ki×Kj}
  THEOREM WHERE i NOTIN j,k AND j NOTIN k IS
    {i≥0∧k+i×j=Ki×Kj∧i≠0}
      if i mod 2=1 then k:=k+j else skip fi
    {i÷2≥0∧k+i÷2×(j×2)=Ki×Kj}
  THEOREM WHERE i NOTIN j,kq,kc,kr AND j NOTIN kq,kc,kr AND kq NOTIN kc,kr AND kc NOTIN kr IS
    {i=Ki ∧ j=Kj ∧ i≥0 ∧ j>0} 
      (kq:=0; kr:=i; kc:=0)
    {j=Kj×2↑kc ∧ j>0 ∧ kq×j+kr=Ki ∧ 0≤kr}
      while j≤kr do j:=j×2; kc:=kc+1 od
    {j=Kj×2↑kc ∧ j>0 ∧ kq×j+kr=Ki ∧ 0≤kr∧kr<j} 
      while kc≠0 do 
         j:=j÷2; kc:=kc-1; kq:=kq×2; 
         if j≥kr then kr:=kr-j; kq:=kq+1 else skip fi 
      od 
    {kq×Kj+kr=Ki ∧ 0≤kr∧kr<Kj}

END

CONJECTUREPANEL "Verification conditions" IS 
  THEOREM IS
    n≥2→2≤2∧2≤n∧∀x.(2≤x∧x<2→n mod x≠0)
  THEOREM IS
    2≤i∧i≤n∧∀x.(2≤x∧x<i→n mod x≠0)∧n mod i≠0→
        2≤i+1∧i+1≤n∧∀x.(2≤x∧x<i+1→n mod x≠0)
  THEOREM IS
    2≤i∧i≤n∧∀x.(2≤x∧x<i→n mod x≠0)∧n mod i≠0→n-i>0
  THEOREM IS
    2≤i∧i≤n∧∀x.(2≤x∧x<i→n mod x≠0)∧n mod i≠0∧n-i=Km→n-(i+1)<Km
  THEOREM IS
    2≤i∧i≤n∧∀x.(2≤x∧x<i→n mod x≠0)∧¬(n mod i≠0)→
        2≤i∧i≤n∧∀x.(2≤x∧x<i→n mod x≠0)∧n mod i=0
        
  THEOREM IS
    i=Ki∧j=Kj∧i≥0→i≥0∧0+i×j=Ki×Kj
  THEOREM IS
    i≥0∧k+i×j=Ki×Kj∧i≠0→i-1≥0∧k+j+(i-1)×j=Ki×Kj
  THEOREM IS
    i≥0∧k+i×j=Ki×Kj∧i≠0→i>0
  THEOREM IS
    i≥0∧k+i×j=Ki×Kj∧i≠0∧i=Km→i-1<Km
  THEOREM IS
    i≥0∧k+i×j=Ki×Kj∧¬(i≠0)→k=Ki×Kj
  
  THEOREM IS
    i=Ki∧j=Kj∧i≥0∧j>0→i≥0∧j>0∧j=Kj∧0×j+i=Ki∧0≤i
  THEOREM IS
    i≥0∧j>0∧j=Kj∧kq×j+kr=Ki∧0≤kr∧kr≥j→i≥0∧j>0∧j=Kj∧(kq+1)×j+(kr-j)=Ki∧0≤kr-j
  THEOREM IS
    i≥0∧j>0∧j=Kj∧kq×j+kr=Ki∧0≤kr∧kr≥j→kr>0
  THEOREM IS
    i≥0∧j>0∧j=Kj∧kq×j+kr=Ki∧0≤kr∧kr≥j∧kr=Km→kr-j<Km
  THEOREM IS
    i≥0∧j>0∧j=Kj∧kq×j+kr=Ki∧0≤kr∧¬(kr≥j)→kq×Kj+kr=Ki∧0≤kr∧kr<Kj
END

CONJECTUREPANEL "Lemmas" IS
  THEOREM IS
    A→A
  THEOREM IS
    A≤B, A≠B ⊢ A<B
  THEOREM IS
    A≥B, A≠B ⊢ A>B
  THEOREM WHERE x NOTIN A, B IS
    ∀x.(A≤x∧x<B→P(x)), P(B) ⊢ ∀x.(A≤x∧x<B+1→P(x))
END