(* $Id$ *)

(* don't open this freely.  It's meant to be a bit abstract, but it's
 * here because some people need to see it.
 * RB
 *)

open Term.Funs

type proviso = FreshProviso of (bool * bool * bool * term)
             | UnifiesProviso of (term * term)
             | NotinProviso of (term * term)
             | NotoneofProviso of (term list * term * term)

(* Meaning of provisos at present :
   FreshProviso (h, g, r, v)    : Variable v doesn't occur free in hypotheses (if h), 
                                  conclusions (if g); non-principal formulae only (if r)
   UnifiesProviso (t1, t2)      : t1 must unify with t2.
   NotinProviso (v, t)          : variable v must not occur free in t
   NotoneofProviso (vs, pat, _C) : in any element of collection _C that matches pat,
                                  variables vs don't occur in the places indicated
                                  by pat.
 *)
                      

 
