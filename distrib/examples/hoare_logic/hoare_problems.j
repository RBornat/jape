/* $Id$ */
CONJECTUREPANEL "Programs"  IS
  THEOREM WHERE i NOTIN x, n, a IS 
	{∃j.(0≤j<n∧x=a[j])}(i:=0; while x≠a[i] do i:=i+1 od){0≤i<n∧x=a[i]}
END
