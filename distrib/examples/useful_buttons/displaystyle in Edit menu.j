/* $Id$ */

MENU Edit IS
	RADIOBUTTON displaystyle IS
		"Box display" IS box
	AND  "Tree display" IS tree
	END
	SEPARATOR
  SEPARATOR
  CHECKBOX showallprovisos 		"show all provisos"
  CHECKBOX showallproofsteps	"show all proof steps"
  CHECKBOX hidecut         		"hide cuts"
  CHECKBOX hidehyp         		"hide identity lines"
END
