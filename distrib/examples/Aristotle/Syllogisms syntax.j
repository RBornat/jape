/*
    Copyright (C) 2020 Richard Bornat
     
        richard@bornat.me.uk

    This file is part of the Aristotleian deductive logic encoding, distributed with jape.

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

CLASS VARIABLE P S M X Y               /* TERM, really */
CLASS FORMULA A B C

PREFIX 100 ∏⁺ ∏⁻ ∑⁺ ∑⁻
POSTFIX 50 *

KEYBOARD IS ∏⁺ ∏⁻ ∑⁺ ∑⁻ *

SEQUENT IS BAG ⊢ FORMULA
