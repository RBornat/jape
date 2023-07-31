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

CONJECTUREPANEL "First figure"
  THEOREM   "Barbara: ∏⁺(M,P), ∏⁺(S,M) ⊢ ∏⁺(S,P)" IS ∏⁺(M,P), ∏⁺(S,M) ⊢ ∏⁺(S,P)
  THEOREM  "Celarent: ∏⁻(M,P), ∏⁺(S,M) ⊢ ∏⁻(S,P)" IS ∏⁻(M,P), ∏⁺(S,M) ⊢ ∏⁻(S,P)
  THEOREM     "Darii: ∏⁺(M,P), ∑⁺(S,M) ⊢ ∑⁺(S,P)" IS ∏⁺(M,P), ∑⁺(S,M) ⊢ ∑⁺(S,P)
  THEOREM     "Ferio: ∏⁻(M,P), ∑⁺(S,M) ⊢ ∑⁻(S,P)" IS ∏⁻(M,P), ∑⁺(S,M) ⊢ ∑⁻(S,P)
  THEOREM   "Barbari: ∏⁺(M,P), ∏⁺(S,M) ⊢ ∑⁺(S,P)" IS ∏⁺(M,P), ∏⁺(S,M) ⊢ ∑⁺(S,P)

  BUTTON Apply IS apply applysyllogism COMMAND
END

CONJECTUREPANEL "Second figure"
  THEOREM    "Cesare: ∏⁻(P,M), ∏⁺(S,M) ⊢ ∏⁻(S,P)" IS ∏⁻(P,M), ∏⁺(S,M) ⊢ ∏⁻(S,P)
  THEOREM   "Festino: ∏⁻(P,M), ∑⁺(S,M) ⊢ ∑⁻(S,P)" IS ∏⁻(P,M), ∑⁺(S,M) ⊢ ∑⁻(S,P)
  THEOREM   "Baroco : ∏⁺(P,M), ∑⁻(S,M) ⊢ ∑⁻(S,P)" IS ∏⁺(P,M), ∑⁻(S,M) ⊢ ∑⁻(S,P)
  THEOREM "Camestres: ∏⁺(P,M), ∏⁻(S,M) ⊢ ∏⁻(S,P)" IS ∏⁺(P,M), ∏⁻(S,M) ⊢ ∏⁻(S,P)
  THEOREM    "Cesaro: ∏⁻(P,M), ∏⁺(S,M) ⊢ ∑⁻(S,P)" IS ∏⁻(P,M), ∏⁺(S,M) ⊢ ∑⁻(S,P)

  BUTTON Apply IS apply applysyllogism COMMAND
END

CONJECTUREPANEL "Third figure"
  THEOREM   "Darapti: ∏⁺(M,P), ∏⁺(M,S) ⊢ ∑⁺(S,P)" IS ∏⁺(M,P), ∏⁺(M,S) ⊢ ∑⁺(S,P)
  THEOREM  "Felapton: ∏⁻(M,P), ∏⁺(M,S) ⊢ ∑⁻(S,P)" IS ∏⁻(M,P), ∏⁺(M,S) ⊢ ∑⁻(S,P)
  THEOREM   "Disamis: ∑⁺(M,P), ∏⁺(M,S) ⊢ ∑⁺(S,P)" IS ∑⁺(M,P), ∏⁺(M,S) ⊢ ∑⁺(S,P)
  THEOREM    "Datisi: ∏⁺(M,P), ∑⁺(M,S) ⊢ ∑⁺(S,P)" IS ∏⁺(M,P), ∑⁺(M,S) ⊢ ∑⁺(S,P)
  THEOREM   "Bocardo: ∑⁻(M,P), ∏⁺(M,S) ⊢ ∑⁻(S,P)" IS ∑⁻(M,P), ∏⁺(M,S) ⊢ ∑⁻(S,P)
  THEOREM   "Ferison: ∏⁻(M,P), ∑⁺(M,S) ⊢ ∑⁻(S,P)" IS ∏⁻(M,P), ∑⁺(M,S) ⊢ ∑⁻(S,P)

  BUTTON Apply IS apply applysyllogism COMMAND
END

CONJECTUREPANEL "Fourth figure"
  THEOREM   "Calemes: ∏⁺(P,M), ∏⁻(M,S) ⊢ ∏⁻(S,P)" IS ∏⁺(P,M), ∏⁻(M,S) ⊢ ∏⁻(S,P)
  THEOREM  "Fresison: ∏⁻(P,M), ∑⁺(M,S) ⊢ ∑⁻(S,P)" IS ∏⁻(P,M), ∑⁺(M,S) ⊢ ∑⁻(S,P)
  THEOREM   "Dimatis: ∑⁺(P,M), ∏⁺(M,S) ⊢ ∑⁺(S,P)" IS ∑⁺(P,M), ∏⁺(M,S) ⊢ ∑⁺(S,P)
  THEOREM   "Bamalip: ∏⁺(P,M), ∏⁺(M,S) ⊢ ∑⁺(S,P)" IS ∏⁺(P,M), ∏⁺(M,S) ⊢ ∑⁺(S,P)
  THEOREM   "Calemos: ∏⁺(P,M), ∏⁻(M,S) ⊢ ∑⁻(S,P)" IS ∏⁺(P,M), ∏⁻(M,S) ⊢ ∑⁻(S,P)
  THEOREM    "Fesapo: ∏⁻(P,M), ∏⁺(M,S) ⊢ ∑⁻(S,P)" IS ∏⁻(P,M), ∏⁺(M,S) ⊢ ∑⁻(S,P)
  THEOREM "Camestros: ∏⁺(P,M), ∏⁻(S,M) ⊢ ∑⁻(S,P)" IS ∏⁺(P,M), ∏⁻(S,M) ⊢ ∑⁻(S,P)
  THEOREM  "Celaront: ∏⁻(M,P), ∏⁺(S,M) ⊢ ∑⁻(S,P)" IS ∏⁻(M,P), ∏⁺(S,M) ⊢ ∑⁻(S,P)

  BUTTON Apply IS apply applysyllogism COMMAND
END

/* for applying syllogisms as theorems. Part-copied from ../forwardstep_technology/forwardstep.j */

MACRO trueforward(tac) IS
    LETGOAL _A 
        (CUTIN (LETGOAL _B (UNIFY _A _B) tac)) 
        (ANY (MATCH see))
        
TACTIC fstep IS
    ALT (ANY (MATCH see)) (trueforward SKIP) /* avoid nasty 'see matches two ways', I hope */

TACTIC applysyllogism (s) IS
    WHEN (LETHYPS _As (CUTIN (RESOLVE s) (WITHHYPSEL see) (WITHHYPSEL see)))
         (SEQ (RESOLVE s) fstep fstep )
