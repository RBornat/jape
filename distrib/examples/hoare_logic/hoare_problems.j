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
END

CONJECTUREPANEL "Lemmas" IS
  THEOREM IS
    A→A
  DERIVED RULE IS
    FROM A≤B AND A≠B INFER A<B
  DERIVED RULE WHERE x NOTIN A, B IS
    FROM ∀x.(A≤x∧x<B→P(x)) AND P(B) INFER ∀x.(A≤x∧x<B+1→P(x))
END