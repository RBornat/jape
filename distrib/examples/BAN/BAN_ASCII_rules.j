/* $Id$ */

RULE "P|=-(Q,P)êK, P<|{X}K Û P|=-Q|~X" IS FROM P|=-(Q,P)êK AND P<|{X}K INFER P|=-Q|~X
RULE "P|=-QØK, P<|{X}Kø Û P|=-Q|~X" IS FROM P|=-QØK AND P<|{X}Kø INFER P|=-Q|~X
RULE "P|=-(P,Q)ŸY, P<|<X>Y Û P|=-Q|~X" IS FROM P|=-(P,Q)ŸY AND P<|<X>Y INFER P|=-Q|~X
RULE "P|=-#X, P|=-Q|~X Û P|=-Q|=-X" IS FROM P|=-#X AND P|=-Q|~X INFER P|=-Q|=-X
RULE "P|=-Q|ÛX, P|=-Q|=-X Û P|=-X" IS FROM P|=-Q|ÛX AND P|=-Q|=-X INFER P|=-X
RULES "P|=-X,  P|=-Y,  ... Û P|=-(X,Y,...)" ARE
	FROM P|=-X AND P|=-Y INFER P|=-(X,Y)
 AND	FROM P|=-X AND P|=-Y AND P|=-Z INFER P|=-(X,Y,Z)
 AND	FROM P|=-W AND P|=-X AND P|=-Y AND P|=-Z INFER P|=-(W,X,Y,Z)
END
RULES "P|=-(...,X,...) Û P|=-X"(X) ARE 
	FROM P|=-(X,Y) INFER P|=-X
 AND	FROM P|=-(Y,X) INFER P|=-X
 AND	FROM P|=-(X,Y,Z) INFER P|=-X
 AND	FROM P|=-(Z,X,Y) INFER P|=-X
 AND	FROM P|=-(Y,Z,X) INFER P|=-X
 AND	FROM P|=-(X,Y,Z,W) INFER P|=-X
 AND	FROM P|=-(W,X,Y,Z) INFER P|=-X
 AND	FROM P|=-(Z,W,X,Y) INFER P|=-X
 AND	FROM P|=-(Y,Z,W,X) INFER P|=-X
END
RULES "P|=-Q|=-(...,X,...) Û P|=-Q|=-X"(X) ARE 
	FROM P|=-Q|=-(X,Y) INFER P|=-Q|=-X
 AND	FROM P|=-Q|=-(Y,X) INFER P|=-Q|=-X
 AND	FROM P|=-Q|=-(X,Y,Z) INFER P|=-Q|=-X
 AND	FROM P|=-Q|=-(Z,X,Y) INFER P|=-Q|=-X
 AND	FROM P|=-Q|=-(Y,Z,X) INFER P|=-Q|=-X
 AND	FROM P|=-Q|=-(X,Y,Z,W) INFER P|=-Q|=-X
 AND	FROM P|=-Q|=-(W,X,Y,Z) INFER P|=-Q|=-X
 AND	FROM P|=-Q|=-(Z,W,X,Y) INFER P|=-Q|=-X
 AND	FROM P|=-Q|=-(Y,Z,W,X) INFER P|=-Q|=-X
END
RULES "P|=-Q|~(...,X,...) Û P|=-Q|~X"(X) ARE
	FROM P|=-Q|~(X,Y) INFER P|=-Q|~X
 AND	FROM P|=-Q|~(Y,X) INFER P|=-Q|~X
 AND	FROM P|=-Q|~(X,Y,Z) INFER P|=-Q|~X
 AND	FROM P|=-Q|~(Z,X,Y) INFER P|=-Q|~X
 AND	FROM P|=-Q|~(Y,Z,X) INFER P|=-Q|~X
 AND	FROM P|=-Q|~(X,Y,Z,W) INFER P|=-Q|~X
 AND	FROM P|=-Q|~(W,X,Y,Z) INFER P|=-Q|~X
 AND	FROM P|=-Q|~(Z,W,X,Y) INFER P|=-Q|~X
 AND	FROM P|=-Q|~(Y,Z,W,X) INFER P|=-Q|~X
END
RULES "P<|(...,X,...) Û P<|X"(X) ARE 
	FROM P<|(X,Y) INFER P<|X
 AND	FROM P<|(Y,X) INFER P<|X
 AND	FROM P<|(X,Y,Z) INFER P<|X
 AND	FROM P<|(Z,X,Y) INFER P<|X
 AND	FROM P<|(Y,Z,X) INFER P<|X
 AND	FROM P<|(X,Y,Z,W) INFER P<|X
 AND	FROM P<|(W,X,Y,Z) INFER P<|X
 AND	FROM P<|(Z,W,X,Y) INFER P<|X
 AND	FROM P<|(Y,Z,W,X) INFER P<|X
END
RULE "P<|<X>Y Û P<|X" IS FROM P<|<X>Y INFER P<|X
RULE "P|=-(P,Q)êK, P<|{X}K Û P<|X" IS FROM P|=-(P,Q)êK AND P<|{X}K INFER P<|X
RULE "P|=-PØK, P<|{X}K Û P<|X" IS FROM P|=-PØK AND P<|{X}K INFER P<|X
RULE "P|=-QØ K, P<|{X}Kø Û P<|X" IS FROM P|=-QØ K AND P<|{X}Kø INFER P<|X
RULES "P|=-#X Û P|=-#(...,X,...)"(X) ARE
	FROM P|=-#X INFER P|=-#(X,Y)
 AND	FROM P|=-#X INFER P|=-#(Y,X)
 AND	FROM P|=-#X INFER P|=-#(X,Y,Z)
 AND	FROM P|=-#X INFER P|=-#(Z,X,Y)
 AND	FROM P|=-#X INFER P|=-#(Y,Z,X)
 AND	FROM P|=-#X INFER P|=-#(X,Y,Z,W)
 AND	FROM P|=-#X INFER P|=-#(W,X,Y,Z)
 AND	FROM P|=-#X INFER P|=-#(Z,W,X,Y)
 AND	FROM P|=-#X INFER P|=-#(Y,ZW,X)
END
RULE "P|=-(R,R')êK Û P|=-(R',R)êK" IS FROM P|=-(R,R')êK INFER P|=-(R',R)êK
RULE "P|=-Q|=-(R,R')êK Û P|=-Q|=-(R',R)êK" IS FROM P|=-Q|=-(R,R')êK INFER P|=-Q|=-(R',R)êK
RULE "P|=-(R,R')ŸK Û P|=-(R',R)ŸK" IS FROM P|=-(R,R')ŸK INFER P|=-(R',R)ŸK
RULE "P|=-Q|=-(R,R')ŸK Û P|=-Q|=-(R',R)ŸK" IS FROM P|=-Q|=-(R,R')ŸK INFER P|=-Q|=-(R',R)ŸK	

/* I hope we can use hyp, else a sequent presentation is impossible; I' m sure that we can use cut */
RULE hyp IS INFER X æ X
RULE cut(X) IS FROM X AND X æ Y INFER Y
    
IDENTITY hyp
CUT cut

/* I think we have weakening.  I hope we do, because otherwise theorem application is difficult */
RULE weaken(X) IS FROM Y INFER X æ Y
WEAKEN weaken

RULE "P|=-èx.X(x) Û P|=-X(Y)"(Y,ABSTRACTION X) IS FROM P|=-èx.X(x) INFER P|=-X(Y)

