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
  THEOREM   "Barbara: every M is P, every S is M ⊢ every S is P" IS every M is P, every S is M ⊢ every S is P
  THEOREM  "Celarent: no M is P, every S is M ⊢ no S is P" IS no M is P, every S is M ⊢ no S is P
  THEOREM     "Darii: every M is P, some S is M ⊢ some S is P" IS every M is P, some S is M ⊢ some S is P
  THEOREM     "Ferio: no M is P, some S is M ⊢ some S is non P" IS no M is P, some S is M ⊢ some S is non P
  THEOREM   "Barbari: every M is P, every S is M ⊢ some S is P" IS every M is P, every S is M ⊢ some S is P

  BUTTON Apply IS apply applysyllogism COMMAND
END

CONJECTUREPANEL "Second figure"
  THEOREM    "Cesare: no P is M, every S is M ⊢ no S is P" IS no P is M, every S is M ⊢ no S is P
  THEOREM   "Festino: no P is M, some S is M ⊢ some S is non P" IS no P is M, some S is M ⊢ some S is non P
  THEOREM   "Baroco : every P is M, some S is non M ⊢ some S is non P" IS every P is M, some S is non M ⊢ some S is non P
  THEOREM "Camestres: every P is M, no S is M ⊢ no S is P" IS every P is M, no S is M ⊢ no S is P
  THEOREM    "Cesaro: no P is M, every S is M ⊢ some S is non P" IS no P is M, every S is M ⊢ some S is non P

  BUTTON Apply IS apply applysyllogism COMMAND
END

CONJECTUREPANEL "Third figure"
  THEOREM   "Darapti: every M is P, every M is S ⊢ some S is P" IS every M is P, every M is S ⊢ some S is P
  THEOREM  "Felapton: no M is P, every M is S ⊢ some S is non P" IS no M is P, every M is S ⊢ some S is non P
  THEOREM   "Disamis: some M is P, every M is S ⊢ some S is P" IS some M is P, every M is S ⊢ some S is P
  THEOREM    "Datisi: every M is P, some M is S ⊢ some S is P" IS every M is P, some M is S ⊢ some S is P
  THEOREM   "Bocardo: some M is non P, every M is S ⊢ some S is non P" IS some M is non P, every M is S ⊢ some S is non P
  THEOREM   "Ferison: no M is P, some M is S ⊢ some S is non P" IS no M is P, some M is S ⊢ some S is non P

  BUTTON Apply IS apply applysyllogism COMMAND
END

CONJECTUREPANEL "Fourth figure"
  THEOREM   "Calemes: every P is M, no M is S ⊢ no S is P" IS every P is M, no M is S ⊢ no S is P
  THEOREM  "Fresison: no P is M, some M is S ⊢ some S is non P" IS no P is M, some M is S ⊢ some S is non P
  THEOREM   "Dimatis: some P is M, every M is S ⊢ some S is P" IS some P is M, every M is S ⊢ some S is P
  THEOREM   "Bamalip: every P is M, every M is S ⊢ some S is P" IS every P is M, every M is S ⊢ some S is P
  THEOREM   "Calemos: every P is M, no M is S ⊢ some S is non P" IS every P is M, no M is S ⊢ some S is non P
  THEOREM    "Fesapo: no P is M, every M is S ⊢ some S is non P" IS no P is M, every M is S ⊢ some S is non P
  THEOREM "Camestros: every P is M, no S is M ⊢ some S is non P" IS every P is M, no S is M ⊢ some S is non P
  THEOREM  "Celaront: no M is P, every S is M ⊢ some S is non P" IS no M is P, every S is M ⊢ some S is non P

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
