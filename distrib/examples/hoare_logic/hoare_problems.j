/* $Id$ */

CONSTANT pass fail

CONJECTUREPANEL "Programs"  IS
  THEOREM WHERE i NOTIN r IS 
	{i=Ki} if i≥40 then r:=pass else r:=fail fi {i=Ki∧(i<40→r=fail)∧(i≥40→r=pass)}
END
