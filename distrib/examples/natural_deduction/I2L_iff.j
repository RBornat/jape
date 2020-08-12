﻿/*
    Copyright (C) 2000-8 Richard Bornat
     
        richard@bornat.me.uk

    This file is part of the I2L logic encoding, distributed with jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).

*/

INFIX 100R ↔ /* iff, same prio as → */

RULE "↔ elim"            IS FROM A ↔ B INFER (A→B)∧(B→A)
RULE "↔ intro"           IS FROM (A→B)∧(B→A) INFER A ↔ B

MENU "Backward"
    BEFOREENTRY "∧ intro"
    ENTRY   "↔ intro" IS BackwardOnlyC (QUOTE (_A↔_B)) 
                                        (Noarg (SEQ "↔ intro" fstep fstep) "↔ intro") "↔ intro" "A↔B"
END

