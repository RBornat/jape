THEOREM "Otway-Rees 1a" IS
   S |* (A,S) <-> Kas,
   S <| ({Na+Nc }Kas + {Nb+Nc}Kbs)
   |- S |* A |~ (Na+Nc)

THEOREM "Otway-Rees 1b" IS
  A |* (A,S) <-> Kas,
  S |* (A,S) <-> Kas,
  B |* (B,S) <-> Kbs,
  S |* (B,S) <-> Kbs,
  S |* (A,B) <-> Kab,
  forall K . A |* S |=> (A,B) <-> K,
  forall X . A |* S |=> B |~ X,
  forall K . B |* S |=> (A,B) <-> K,
  forall X . B |* S |=> A |~ X,
  A |* #Na,
  B |* #Nb,
  A |* #Nc, 
   B <| {Na+Nc}Kas, 
   S <| ({Na+Nc }Kas + {Nb+Nc}Kbs)
   |- S |* B |~ (Nb+Nc)

THEOREM "Otway-Rees 2a" IS
  A |* (A,S) <-> Kas,
  S |* (A,S) <-> Kas,
  B |* (B,S) <-> Kbs,
  S |* (B,S) <-> Kbs,
  S |* (A,B) <-> Kab,
  forall K . A |* S |=> (A,B) <-> K,
  forall X . A |* S |=> B |~ X,
  forall K . B |* S |=> (A,B) <-> K,
  forall X . B |* S |=> A |~ X,
  A |* #Na,
  B |* #Nb,
  A |* #Nc, 
   B <| {Na+Nc}Kas,
   S <| ({Na+Nc }Kas + {Nb+Nc}Kbs),
   B <| ({Na + (A,B)<->Kab + B|~Nc}Kas + {Nb + (A,B)<->Kab + A|~Nc}Kbs),
   A <| {Na + (A,B)<->Kab + B|~Nc}Kas
  |- A |* (A,B) <-> Kab
  
THEOREM "Otway-Rees 2b" IS
  A |* (A,S) <-> Kas,
  S |* (A,S) <-> Kas,
  B |* (B,S) <-> Kbs,
  S |* (B,S) <-> Kbs,
  S |* (A,B) <-> Kab,
  forall K . A |* S |=> (A,B) <-> K,
  forall X . A |* S |=> B |~ X,
  forall K . B |* S |=> (A,B) <-> K,
  forall X . B |* S |=> A |~ X,
  A |* #Na,
  B |* #Nb,
  A |* #Nc, 
   B <| {Na+Nc}Kas,
   S <| ({Na+Nc }Kas + {Nb+Nc}Kbs),
   B <| ({Na + (A,B)<->Kab + B|~Nc}Kas + {Nb + (A,B)<->Kab + A|~Nc}Kbs),
   A <| {Na + (A,B)<->Kab + B|~Nc}Kas
  |- B |* (A,B) <-> Kab
  
THEOREM "Otway-Rees 2c" IS
  A |* (A,S) <-> Kas,
  S |* (A,S) <-> Kas,
  B |* (B,S) <-> Kbs,
  S |* (B,S) <-> Kbs,
  S |* (A,B) <-> Kab,
  forall K . A |* S |=> (A,B) <-> K,
  forall X . A |* S |=> B |~ X,
  forall K . B |* S |=> (A,B) <-> K,
  forall X . B |* S |=> A |~ X,
  A |* #Na,
  B |* #Nb,
  A |* #Nc, 
   B <| {Na+Nc}Kas,
   S <| ({Na+Nc }Kas + {Nb+Nc}Kbs),
   B <| ({Na + (A,B)<->Kab + B|~Nc}Kas + {Nb + (A,B)<->Kab + A|~Nc}Kbs),
   A <| {Na + (A,B)<->Kab + B|~Nc}Kas
  |- A |* B |* Nc
  
THEOREM "Otway-Rees 2d" IS
  A |* (A,S) <-> Kas,
  S |* (A,S) <-> Kas,
  B |* (B,S) <-> Kbs,
  S |* (B,S) <-> Kbs,
  S |* (A,B) <-> Kab,
  forall K . A |* S |=> (A,B) <-> K,
  forall X . A |* S |=> B |~ X,
  forall K . B |* S |=> (A,B) <-> K,
  forall X . B |* S |=> A |~ X,
  A |* #Na,
  B |* #Nb,
  A |* #Nc, 
   B <| {Na+Nc}Kas,
   S <| ({Na+Nc }Kas + {Nb+Nc}Kbs),
   B <| ({Na + (A,B)<->Kab + B|~Nc}Kas + {Nb + (A,B)<->Kab + A|~Nc}Kbs),
   A <| {Na + (A,B)<->Kab + B|~Nc}Kas
  |- B |* A |~ Nc
