/* $Id$ */

RULE "PŠ(Q,P)êK, P‘{X}K Û PŠQ•X" IS FROM PŠ(Q,P)êK AND P‘{X}K INFER PŠQ•X
RULE "PŠQØK, P‘{X}Kø Û PŠQ•X" IS FROM PŠQØK AND P‘{X}Kø INFER PŠQ•X
RULE "PŠ(P,Q)ŸY, P‘<X>Y Û PŠQ•X" IS FROM PŠ(P,Q)ŸY AND P‘<X>Y INFER PŠQ•X
RULE "PŠ#X, PŠQ•X Û PŠQŠX" IS FROM PŠ#X AND PŠQ•X INFER PŠQŠX
RULE "PŠQšX, PŠQŠX Û PŠX" IS FROM PŠQšX AND PŠQŠX INFER PŠX
RULE "PŠX, PŠY Û PŠ(X+Y)" IS FROM PŠX AND PŠY INFER PŠ(X+Y)
RULE "PŠ(X+Y) Û PŠX" IS FROM PŠ(X+Y) INFER PŠX
RULE "PŠ(X+Y) Û PŠY" IS FROM PŠ(X+Y) INFER PŠY
RULE "PŠQŠ(X+Y) Û PŠQŠX" IS FROM PŠQŠ(X+Y) INFER PŠQŠX
RULE "PŠQŠ(X+Y) Û PŠQŠY" IS FROM PŠQŠ(X+Y) INFER PŠQŠY
RULE "PŠQ•(X+Y) Û PŠQ•X" IS FROM PŠQ•(X+Y) INFER PŠQ•X
RULE "PŠQ•(X+Y) Û PŠQ•Y" IS FROM PŠQ•(X+Y) INFER PŠQ•Y
RULE "P‘(X+Y) Û P‘X" IS FROM P‘(X+Y) INFER P‘X
RULE "P‘(X+Y) Û P‘Y" IS FROM P‘(X+Y) INFER P‘Y
RULE "P‘<X>Y Û P‘X" IS FROM P‘<X>Y INFER P‘X
RULE "PŠ(P,Q)êK, P‘{X}K Û P‘X" IS FROM PŠ(P,Q)êK AND P‘{X}K INFER P‘X
RULE "PŠPØK, P‘{X}K Û P‘X" IS FROM PŠPØK AND P‘{X}K INFER P‘X
RULE "PŠQØ K, P‘{X}Kø Û P‘X" IS FROM PŠQØ K AND P‘{X}Kø INFER P‘X
RULE "PŠ#X Û PŠ#(X+Y)" IS FROM PŠ#X INFER PŠ#(X+Y)
RULE "PŠ#Y Û PŠ#(X+Y)" IS FROM PŠ#Y INFER PŠ#(X+Y)
RULE "PŠ(R,R')êK Û PŠ(R',R)êK" IS FROM PŠ(R,R')êK INFER PŠ(R',R)êK
RULE "PŠQŠ(R,R')êK Û PŠQŠ(R,R')êK" IS FROM PŠQŠ(R,R')êK INFER PŠQŠ(R',R)êK
RULE "PŠ(R,R')ŸK Û PŠ(R',R)ŸK" IS FROM PŠ(R,R')ŸK INFER PŠ(R',R)ŸK
RULE "PŠQŠ(R,R')ŸK Û PŠQŠ(R',R)ŸK" IS FROM PŠQŠ(R,R')ŸK INFER PŠQŠ(R',R)ŸK	

/* I hope we can use hyp, else a sequent presentation is impossible; I' m sure that we can use cut */
RULE hyp IS INFER X æ X
RULE cut(X) IS FROM X AND X æ Y INFER Y
    
IDENTITY hyp
CUT cut

/* I think we have weakening.  I hope we do, because otherwise theorem application is difficult */
RULE weaken(X) IS FROM Y INFER X æ Y
WEAKEN weaken

RULE "PŠ(èx.X) Û PŠX[x\\Y]"(Y) IS FROM PŠ(èx.X) INFER PŠX[x\Y]

