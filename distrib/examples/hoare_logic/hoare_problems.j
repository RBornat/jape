/*
    $Id$

    Copyright (C) 2004-8 Richard Bornat
     
        richard@bornat.me.uk

    This file is part of the Hoare logic example distribution, which is part of jape.

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

TACTIC TheoremForward (thm) IS CUTIN (ALT thm (RESOLVE thm))

TACTIC TheoremForwardOrBackward(thm) IS
    WHEN    
        (LETHYP _A 
            (ALT    (TheoremForward (WITHHYPSEL (WITHARGSEL thm)))))
        (LETHYPS _As
            (Fail ("At present Jape can't deal with multiple hypothesis selections when applying theorems. Sorry.\
                    \\nCancel all but one of them and try again.")))
        (LETGOAL _A
            (ALT (WITHARGSEL thm) 
                (RESOLVE (WITHARGSEL thm)) 
                (TheoremForward (WITHARGSEL thm))
                (Fail   "Theorem application failed -- tell Richard Bornat")))
        (LETOPENSUBGOAL G _A 
            (Fail ("Error in I2L Jape (open subgoal in TheoremForwardOrBackward). Tell Richard Bornat.")))
        (LETOPENSUBGOALS _As
            (ALERT  ("There is more than one unproved conclusion in the proof. Please select one – \
                        \or select a hypothesis – to show \
                        \Jape where to apply the theorem.")
                    ("OK", STOP) 
                    ("Huh?", Explainhypothesisandconclusionwords)))
        (ALERT  "The proof is finished -- there are no unproved conclusions left."
                ("OK", STOP) 
                ("Huh?", Explainunprovedconclusionwords))
CONSTANT pass fail

CONJECTUREPANEL "Variable Programs"  IS
  THEOREM IS 
    {i=2} (i:=i+1) {i=3}
  THEOREM WHERE DISTINCT i,j,t IS 
    {i=Ki∧j=Kj} (t:=i; i:=j; j:=t) {i=Kj∧j=Ki}
  THEOREM WHERE DISTINCT i,j IS 
    {i=Ki∧j=Kj} (i:=j; j:=i) {i=Kj∧j=Ki}
  THEOREM WHERE DISTINCT i,j,k IS
    {⊤} if j>k then i:=j else i:= k fi {(j≥k→i=j)∧(k≥j→i=k)}
  THEOREM WHERE DISTINCT i,j,k IS
    {j=Kj∧k=Kk} if j>k then i:=j else i:= k fi {j=Kj∧k=Kk∧(j>k→i=Kj)∧(k≥j→i=Kk)}
  THEOREM WHERE DISTINCT i,j,k IS
    {j=Kj∧k=Kk} if j≥k then i:=j else i:= k fi {j=Kj∧k=Kk∧(j≥k→i=Kj)∧(k≥j→i=Kk)}
  THEOREM WHERE DISTINCT i,r IS 
    {i=Ki} if i≥40 then r:=pass else r:=fail fi {i=Ki∧(i<40→r=fail)∧(i≥40→r=pass)}
  THEOREM (OBJECT x) WHERE DISTINCT i,n IS
    {n≥2} 
      (i:=2)
    {2≤i∧i≤n∧∀x.(2≤x ∧ x<i → n mod x ≠0)}
      while n mod i ≠ 0 do i:=i+1 od 
    {2≤i∧i≤n∧∀x.(2≤x ∧ x<i → n mod x ≠0) ∧ n mod i = 0}
  THEOREM WHERE DISTINCT i,prime,n IS
    {2≤i∧i≤n∧∀x.(2≤x ∧ x<i → n mod x ≠0) ∧ n mod i = 0}
      (prime := i=n)
    {prime ↔¬(∃y.(2≤y∧y<n∧n mod y = 0))}
  THEOREM WHERE DISTINCT i,prime,n IS
    {2≤i∧i≤n∧∀x.(2≤x ∧ x<i → n mod x ≠0) ∧ n mod i = 0}
      (prime := i=n)
    {prime ↔∀y.(2≤y∧y<n → n mod y ≠ 0)}
  THEOREM WHERE DISTINCT i,j,k IS
    {i=Ki∧j=Kj∧i≥0}
      (k:=0) 
    {i≥0 ∧ k+i×j=Ki×Kj}
      while i≠0 do k:=k+j; i:=i-1 od
    {k=Ki×Kj}
  THEOREM WHERE DISTINCT i,j,kq,kr IS
    {i=Ki∧j=Kj∧i≥0∧j>0}
      (kq:=0; kr:=i)
    {j=Kj∧j>0∧kr≥0∧kq×j+kr=Ki}
      while kr≥j do kr:=kr-j; kq:=kq+1 od
    {kq×Kj+kr=Ki∧0≤kr∧kr<Kj}
  THEOREM WHERE DISTINCT i,j,k IS
    {i=Ki∧j=Kj∧i≥0}
      (k:=0)
    {i≥0 ∧ k+i×j=Ki×Kj}
       while i≠0 do
         if i mod 2=1 then k:=k+j else skip fi;
         i:=i÷2; j:=j×2
       od
    {k=Ki×Kj}
  THEOREM WHERE DISTINCT i,j,kq,kc,kr IS
    {i=Ki ∧ j=Kj ∧ i≥0 ∧ j>0} 
      (kq:=0; kr:=i; kc:=0)
    {j=Kj×2↑kc ∧ j>0 ∧ kq×j+kr=Ki ∧ 0≤kr}
      while j≤kr do j:=j×2; kc:=kc+1 od
    {j=Kj×2↑kc ∧ j>0 ∧ kq×j+kr=Ki ∧ 0≤kr∧kr<j} 
      while kc≠0 do 
         j:=j÷2; kc:=kc-1; kq:=kq×2; 
         if j≥kr then kr:=kr-j; kq:=kq+1 else skip fi 
      od 
    {kq×Kj+kr=Ki ∧ 0≤kr∧kr<Kj}
END

CONJECTUREPANEL "Array Programs" IS
  THEOREM WHERE DISTINCT a,i IS
    {a[i]=2} (a[i]:=a[i]+1) {a[i]=3}
  THEOREM WHERE DISTINCT a,i IS
    {a[i]=0} (a[i]:=a[i]+1;a[i]:=a[i]+1) {a[i]=2}
  THEOREM WHERE DISTINCT a,i IS
    {∃x.(0≤x∧x<length(a) ∧ a[x]=0)}
        (i:=0)
    {0≤i∧i<length(a) ∧ ∃x.(i≤x∧x<length(a) ∧ a[x]=0)}
        while a[i]≠0 do i:=i+1 od
    {a[i]=0}
  THEOREM partition WHERE DISTINCT m,n,i,j,p,a,done,t IS
    {0≤m∧m<n∧n≤length(a)∧∃x.(m≤x∧x<n∧a[x]=p)}
      (i:=m; j:=n; done:=⊥)
    {0≤m∧m≤i∧i≤j∧j≤n∧n≤length(a)∧
     ∃yl.(m≤yl∧yl<j∧a[yl]≤p)∧∃yh.(i≤yh∧yh<n∧a[yh]≥p)∧
     ∀xl.(m≤xl∧xl<i→a[xl]≤p)∧∀xh.(j≤xh∧xh<n→a[xh]≥p)∧
     (done→i=j∨(i+1=j∧a[i]=p))}
      while ¬done do
          skip
        {0≤m∧m≤i∧i≤j∧j≤n∧n≤length(a)∧
         ∃yl.(m≤yl∧yl<j∧a[yl]≤p)∧∃yh.(i≤yh∧yh<n∧a[yh]≥p)∧
         ∀xl.(m≤xl∧xl<i→a[xl]≤p)∧∀xh.(j≤xh∧xh<n→a[xh]≥p)∧
         ¬done}
          while a[i]<p do i:=i+1 od
        {0≤m∧m≤i∧i≤j∧j≤n∧n≤length(a)∧
         ∃yl.(m≤yl∧yl<j∧a[yl]≤p)∧
         ∀xl.(m≤xl∧xl<i→a[xl]≤p)∧∀xh.(j≤xh∧xh<n→a[xh]≥p)∧
         ¬done∧a[i]≥p}
          while a[j-1]>p do j:=j-1 od
        {0≤m∧m≤i∧i≤j∧j≤n∧n≤length(a)∧
         ∀xl.(m≤xl∧xl<i→a[xl]≤p)∧∀xh.(j≤xh∧xh<n→a[xh]≥p)∧
         ¬done∧a[i]≥p∧a[j-1]≤p}
          if i+1<j then
            j:=j-1;
            t:=a[i]; a[i]:=a[j]; a[j]:=t;
            i:=i+1
          else
            done:=⊤
          fi
      od
    {0≤m∧m≤i∧i≤j∧j≤n∧n≤length(a)∧i<n∧m<j∧
     ∀xl.(m≤xl∧xl<i→a[xl]≤p)∧∀xh.(j≤xh∧xh<n→a[xh]≥p)∧
     (i=j∨(i+1=j∧a[i]=p))}
END

CONJECTUREPANEL "Useful Lemmas" IS
  THEOREM IS
    A→A
  THEOREM IS
    A≤B, A≠B ⊢ A<B
  THEOREM IS
    A≥B, A≠B ⊢ A>B
  THEOREM WHERE x NOTIN A, B IS
    ∀x.(A≤x∧x<B→P(x)), P(B) ⊢ ∀x.(A≤x∧x<B+1→P(x))
  THEOREM WHERE x NOTIN A, B IS
    P(A-1), ∀x.(A≤x∧x<B→P(x)) ⊢ ∀x.(A-1≤x∧x<B→P(x))
  BUTTON Apply IS apply (TheoremForwardOrBackward COMMAND)
END