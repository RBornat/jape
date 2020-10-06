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

CONJECTUREPANEL "Syllogisms"
  THEOREM   "Barbara: ∏⁺(M,P), ∏⁺(S,M) ⊢ ∏⁺(S,P)" IS ∏⁺(M,P), ∏⁺(S,M) ⊢ ∏⁺(S,P)
  THEOREM  "Celarent: ∏⁻(M,P), ∏⁺(S,M) ⊢ ∏⁻(S,P)" IS ∏⁻(M,P), ∏⁺(S,M) ⊢ ∏⁻(S,P)
  THEOREM   "Bocardo: ∑⁻(M,P), ∏⁺(M,S) ⊢ ∑⁻(S,P)" IS ∑⁻(M,P), ∏⁺(M,S) ⊢ ∑⁻(S,P)
  THEOREM   "Baroco : ∏⁺(P,M), ∑⁻(S,M) ⊢ ∑⁻(S,P)" IS ∏⁺(P,M), ∑⁻(S,M) ⊢ ∑⁻(S,P)
  THEOREM   "Disamis: ∑⁺(M,P), ∏⁺(M,S) ⊢ ∑⁺(S,P)" IS ∑⁺(M,P), ∏⁺(M,S) ⊢ ∑⁺(S,P)
  THEOREM   "Festino: ∏⁻(P,M), ∑⁺(S,M) ⊢ ∑⁻(S,P)" IS ∏⁻(P,M), ∑⁺(S,M) ⊢ ∑⁻(S,P)
  THEOREM   "Calemes: ∏⁺(P,M), ∏⁻(M,S) ⊢ ∏⁻(S,P)" IS ∏⁺(P,M), ∏⁻(M,S) ⊢ ∏⁻(S,P)
  THEOREM  "Fresison: ∏⁻(P,M), ∑⁺(M,S) ⊢ ∑⁻(S,P)" IS ∏⁻(P,M), ∑⁺(M,S) ⊢ ∑⁻(S,P)
  THEOREM   "Dimatis: ∑⁺(P,M), ∏⁺(M,S) ⊢ ∑⁺(S,P)" IS ∑⁺(P,M), ∏⁺(M,S) ⊢ ∑⁺(S,P)
  THEOREM "Camestres: ∏⁺(P,M), ∏⁻(S,M) ⊢ ∏⁻(S,P)" IS ∏⁺(P,M), ∏⁻(S,M) ⊢ ∏⁻(S,P)
  THEOREM   "Ferison: ∏⁻(M,P), ∑⁺(M,S) ⊢ ∑⁻(S,P)" IS ∏⁻(M,P), ∑⁺(M,S) ⊢ ∑⁻(S,P)
  THEOREM     "Darii: ∏⁺(M,P), ∑⁺(S,M) ⊢ ∑⁺(S,P)" IS ∏⁺(M,P), ∑⁺(S,M) ⊢ ∑⁺(S,P)
  THEOREM    "Cesare: ∏⁻(P,M), ∏⁺(S,M) ⊢ ∏⁻(S,P)" IS ∏⁻(P,M), ∏⁺(S,M) ⊢ ∏⁻(S,P)
  THEOREM    "Datisi: ∏⁺(M,P), ∑⁺(M,S) ⊢ ∑⁺(S,P)" IS ∏⁺(M,P), ∑⁺(M,S) ⊢ ∑⁺(S,P)
  THEOREM     "Ferio: ∏⁻(M,P), ∑⁺(S,M) ⊢ ∑⁻(S,P)" IS ∏⁻(M,P), ∑⁺(S,M) ⊢ ∑⁻(S,P)
  THEOREM   "Bamalip: ∏⁺(P,M), ∏⁺(M,S) ⊢ ∑⁺(S,P)" IS ∏⁺(P,M), ∏⁺(M,S) ⊢ ∑⁺(S,P)
  THEOREM   "Calemos: ∏⁺(P,M), ∏⁻(M,S) ⊢ ∑⁻(S,P)" IS ∏⁺(P,M), ∏⁻(M,S) ⊢ ∑⁻(S,P)
  THEOREM    "Fesapo: ∏⁻(P,M), ∏⁺(M,S) ⊢ ∑⁻(S,P)" IS ∏⁻(P,M), ∏⁺(M,S) ⊢ ∑⁻(S,P)
  THEOREM   "Barbari: ∏⁺(M,P), ∏⁺(S,M) ⊢ ∑⁺(S,P)" IS ∏⁺(M,P), ∏⁺(S,M) ⊢ ∑⁺(S,P)
  THEOREM  "Felapton: ∏⁻(M,P), ∏⁺(M,S) ⊢ ∑⁻(S,P)" IS ∏⁻(M,P), ∏⁺(M,S) ⊢ ∑⁻(S,P)
  THEOREM "Camestros: ∏⁺(P,M), ∏⁻(S,M) ⊢ ∑⁻(S,P)" IS ∏⁺(P,M), ∏⁻(S,M) ⊢ ∑⁻(S,P)
  THEOREM  "Celaront: ∏⁻(M,P), ∏⁺(S,M) ⊢ ∑⁻(S,P)" IS ∏⁻(M,P), ∏⁺(S,M) ⊢ ∑⁻(S,P)
  THEOREM   "Darapti: ∏⁺(M,P), ∏⁺(M,S) ⊢ ∑⁺(S,P)" IS ∏⁺(M,P), ∏⁺(M,S) ⊢ ∑⁺(S,P)
  THEOREM    "Cesaro: ∏⁻(P,M), ∏⁺(S,M) ⊢ ∑⁻(S,P)" IS ∏⁻(P,M), ∏⁺(S,M) ⊢ ∑⁻(S,P)

  BUTTON Apply IS apply applysyllogism COMMAND
END
