/* $Id$ */

CONSTANT pass fail mod

CONJECTUREPANEL "Programs"  IS
  THEOREM WHERE i NOTIN r IS 
    {i=Ki} if i≥40 then r:=pass else r:=fail fi {i=Ki∧(i<40→r=fail)∧(i≥40→r=pass)}
  THEOREM (OBJECT x) WHERE i NOTIN n IS
    {n≥2} (i:=2; while n mod i ≠ 0 do i:=i+1 od) {∀x.(2≤x ∧ x<i → n mod x ≠0) ∧ n mod i = 0}
END

CONJECTUREPANEL "Lemmas" IS 
    THEOREM (OBJECT x) WHERE i NOTIN n IS
	{n≥2} (i:=2) {i≤n ∧ ∀x.(2≤x ∧ x<i → n mod x ≠0)}
    THEOREM (OBJECT x) WHERE i NOTIN n IS
	{i≤n∧∀x.(2≤x∧x<i→n mod x≠0)∧n mod i≠0∧n-i=Km}(i:=i+1){n-i<Km}
    THEOREM (OBJECT x) WHERE i NOTIN n IS
	i≤n∧∀x.(2≤x∧x<i→n mod x≠0)∧n mod i≠0→n-i>0
    THEOREM (OBJECT x) WHERE i NOTIN n IS
	{i≤n∧∀x.(2≤x∧x<i→n mod x≠0)∧n mod i≠0}
	    (i:=i+1)
	{i≤n∧∀x.(2≤x∧x<i→n mod x≠0)}
END