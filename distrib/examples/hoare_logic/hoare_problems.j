/* $Id$ */

CONSTANT pass fail

CONJECTUREPANEL "Programs"  IS
  THEOREM IS 
	{i=Ki} if i≥0 then r:=pass else r:=fail fi {i=Ki∧(i<40→r=fail)∧(i≥40→r=pass)}
END
