/* $Id$ */

RULE "P|È(Q,P)ÍK, P<|{X}K € P|ÈQ|~X" IS FROM P|È(Q,P)ÍK AND P<|{X}K INFER P|ÈQ|~X
RULE "P|ÈQÿK, P<|{X}K¯ € P|ÈQ|~X" IS FROM P|ÈQÿK AND P<|{X}K¯ INFER P|ÈQ|~X
RULE "P|È(P,Q)üY, P<|<X>Y € P|ÈQ|~X" IS FROM P|È(P,Q)üY AND P<|<X>Y INFER P|ÈQ|~X
RULE "P|È#X, P|ÈQ|~X € P|ÈQ|ÈX" IS FROM P|È#X AND P|ÈQ|~X INFER P|ÈQ|ÈX
RULE "P|ÈQ|€X, P|ÈQ|ÈX € P|ÈX" IS FROM P|ÈQ|€X AND P|ÈQ|ÈX INFER P|ÈX
RULES "P|ÈX,  P|ÈY,  ... € P|È(X,Y,...)" ARE
	FROM P|ÈX AND P|ÈY INFER P|È(X,Y)
 AND	FROM P|ÈX AND P|ÈY AND P|ÈZ INFER P|È(X,Y,Z)
 AND	FROM P|ÈW AND P|ÈX AND P|ÈY AND P|ÈZ INFER P|È(W,X,Y,Z)
END
RULES "P|È(...,X,...) € P|ÈX"(X) ARE 
	FROM P|È(X,Y) INFER P|ÈX
 AND	FROM P|È(Y,X) INFER P|ÈX
 AND	FROM P|È(X,Y,Z) INFER P|ÈX
 AND	FROM P|È(Z,X,Y) INFER P|ÈX
 AND	FROM P|È(Y,Z,X) INFER P|ÈX
 AND	FROM P|È(X,Y,Z,W) INFER P|ÈX
 AND	FROM P|È(W,X,Y,Z) INFER P|ÈX
 AND	FROM P|È(Z,W,X,Y) INFER P|ÈX
 AND	FROM P|È(Y,Z,W,X) INFER P|ÈX
END
RULES "P|ÈQ|È(...,X,...) € P|ÈQ|ÈX"(X) ARE 
	FROM P|ÈQ|È(X,Y) INFER P|ÈQ|ÈX
 AND	FROM P|ÈQ|È(Y,X) INFER P|ÈQ|ÈX
 AND	FROM P|ÈQ|È(X,Y,Z) INFER P|ÈQ|ÈX
 AND	FROM P|ÈQ|È(Z,X,Y) INFER P|ÈQ|ÈX
 AND	FROM P|ÈQ|È(Y,Z,X) INFER P|ÈQ|ÈX
 AND	FROM P|ÈQ|È(X,Y,Z,W) INFER P|ÈQ|ÈX
 AND	FROM P|ÈQ|È(W,X,Y,Z) INFER P|ÈQ|ÈX
 AND	FROM P|ÈQ|È(Z,W,X,Y) INFER P|ÈQ|ÈX
 AND	FROM P|ÈQ|È(Y,Z,W,X) INFER P|ÈQ|ÈX
END
RULES "P|ÈQ|~(...,X,...) € P|ÈQ|~X"(X) ARE
	FROM P|ÈQ|~(X,Y) INFER P|ÈQ|~X
 AND	FROM P|ÈQ|~(Y,X) INFER P|ÈQ|~X
 AND	FROM P|ÈQ|~(X,Y,Z) INFER P|ÈQ|~X
 AND	FROM P|ÈQ|~(Z,X,Y) INFER P|ÈQ|~X
 AND	FROM P|ÈQ|~(Y,Z,X) INFER P|ÈQ|~X
 AND	FROM P|ÈQ|~(X,Y,Z,W) INFER P|ÈQ|~X
 AND	FROM P|ÈQ|~(W,X,Y,Z) INFER P|ÈQ|~X
 AND	FROM P|ÈQ|~(Z,W,X,Y) INFER P|ÈQ|~X
 AND	FROM P|ÈQ|~(Y,Z,W,X) INFER P|ÈQ|~X
END
RULES "P<|(...,X,...) € P<|X"(X) ARE 
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
RULE "P<|<X>Y € P<|X" IS FROM P<|<X>Y INFER P<|X
RULE "P|È(P,Q)ÍK, P<|{X}K € P<|X" IS FROM P|È(P,Q)ÍK AND P<|{X}K INFER P<|X
RULE "P|ÈPÿK, P<|{X}K € P<|X" IS FROM P|ÈPÿK AND P<|{X}K INFER P<|X
RULE "P|ÈQÿ K, P<|{X}K¯ € P<|X" IS FROM P|ÈQÿ K AND P<|{X}K¯ INFER P<|X
RULES "P|È#X € P|È#(...,X,...)"(X) ARE
	FROM P|È#X INFER P|È#(X,Y)
 AND	FROM P|È#X INFER P|È#(Y,X)
 AND	FROM P|È#X INFER P|È#(X,Y,Z)
 AND	FROM P|È#X INFER P|È#(Z,X,Y)
 AND	FROM P|È#X INFER P|È#(Y,Z,X)
 AND	FROM P|È#X INFER P|È#(X,Y,Z,W)
 AND	FROM P|È#X INFER P|È#(W,X,Y,Z)
 AND	FROM P|È#X INFER P|È#(Z,W,X,Y)
 AND	FROM P|È#X INFER P|È#(Y,ZW,X)
END
RULE "P|È(R,R')ÍK € P|È(R',R)ÍK" IS FROM P|È(R,R')ÍK INFER P|È(R',R)ÍK
RULE "P|ÈQ|È(R,R')ÍK € P|ÈQ|È(R',R)ÍK" IS FROM P|ÈQ|È(R,R')ÍK INFER P|ÈQ|È(R',R)ÍK
RULE "P|È(R,R')üK € P|È(R',R)üK" IS FROM P|È(R,R')üK INFER P|È(R',R)üK
RULE "P|ÈQ|È(R,R')üK € P|ÈQ|È(R',R)üK" IS FROM P|ÈQ|È(R,R')üK INFER P|ÈQ|È(R',R)üK	

/* I hope we can use hyp, else a sequent presentation is impossible; I' m sure that we can use cut */
RULE hyp IS INFER X Ê X
RULE cut(X) IS FROM X AND X Ê Y INFER Y
    
IDENTITY hyp
CUT cut

/* I think we have weakening.  I hope we do, because otherwise theorem application is difficult */
RULE weaken(X) IS FROM Y INFER X Ê Y
WEAKEN weaken

RULE "P|ÈËx.X(x) € P|ÈX(Y)"(Y,ABSTRACTION X) IS FROM P|ÈËx.X(x) INFER P|ÈX(Y)

