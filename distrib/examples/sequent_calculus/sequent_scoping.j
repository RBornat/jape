/* $Id$ */

/* LF-style variables in the sequent calculus (multiple and single conclusion) */

PREFIX	10		var
POSTFIX	10		inscope

RULES "inscope" ARE
	‚, var x æ x inscope
AND	FROM ‚ æ A inscope AND ‚ æ B inscope INFER ‚ æ AçB inscope
AND	FROM ‚ æ A inscope AND ‚ æ B inscope INFER ‚ æ A¦B inscope
AND	FROM ‚ æ A inscope AND ‚ æ B inscope INFER ‚ æ AëB inscope
AND	FROM ‚ æ A inscope INFER ‚ æ ÂA inscope
AND	FROM ‚, var x æ A inscope INFER ‚ æ èx.A inscope
AND	FROM ‚, var x æ A inscope INFER ‚ æ äx.A inscope 
END

AUTOMATCH "inscope"

