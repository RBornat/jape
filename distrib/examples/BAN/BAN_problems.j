/* @(#)authentication_problems.j	1.2 93/02/10 16:14:07 */

THEOREM "Otway-Rees 1a" IS
   S |* (A,S) <-> Kas,
   S <| ({Na+Nc }Kas + {Nb+Nc}Kbs)
   |- S |* A |~ (Na+Nc)

THEOREM "Otway-Rees 1b" IS
   S |* (B,S) <-> Kbs,
   S <| ({Na+Nc }Kas + {Nb+Nc}Kbs)
   |- S |* B |~ (Nb+Nc)


THEOREM "Otway-Rees 2a" IS
   A |* (A,S) <-> Kas,
   forall K . A |* S |=> (A,B) <-> K,
   A |* #Na,
   A <| {Na + (A,B)<->Kab + B|~Nc}Kas
   |- A |* (A,B) <-> Kab
  
THEOREM "Otway-Rees 2b" IS
   B |* (B,S) <-> Kbs,
   forall K . B |* S |=> (A,B) <-> K,
   B |* #Nb,
   B <| ({Na + (A,B)<->Kab + B|~Nc}Kas + {Nb + (A,B)<->Kab + A|~Nc}Kbs)
   |- B |* (A,B) <-> Kab
  
THEOREM "Otway-Rees 2c" IS
   A |* (A,S) <-> Kas,
   forall X . A |* S |=> B |~ X,
   A |* #Na,
   A |* #Nc,
   A <| {Na + (A,B)<->Kab + B|~Nc}Kas
   |- A |* B |* Nc
  
THEOREM "Otway-Rees 2d" IS
   B |* (B,S) <-> Kbs,
   forall X . B |* S |=> A |~ X,
   A |* #Na,
   B |* #Nb,
   B <| ({Na + (A,B)<->Kab + B|~Nc}Kas + {Nb + (A,B)<->Kab + A|~Nc}Kbs)
   |- B |* A |~ Nc
