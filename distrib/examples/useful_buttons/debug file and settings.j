/* $Id$ */

MENU File IS
	SEPARATOR
	BUTTON "Debug output to É" IS createdbugfile
	BUTTON "Close debug file" IS closedbugfile
END

MENU Edit IS
  SEPARATOR
  CHECKBOX tactictracing "trace tactics"
  CHECKBOX applydebug    "trace rule/theorem application"
  CHECKBOX unifydebug    "trace unification"
  CHECKBOX rewritedebug  "trace rewrites"
  CHECKBOX substdebug    "trace substitution simplification"
  CHECKBOX thingdebug    "trace rule/theorem/tactic store/retrieve"
  SEPARATOR
  CHECKBOX showallprovisos "show all provisos"
  CHECKBOX showallproofsteps	"show all proof steps"
  CHECKBOX hidecut         "hide cuts"
  CHECKBOX hidehyp         "hide identity lines"
END
