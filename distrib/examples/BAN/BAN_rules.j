INFIX 40 40 isin 
INFIX 61 60 +

INFIX 100 100 <|
INFIX 210 200 |*
INFIX 300 300 |=>
INFIX 400 400 |~
PREFIX #
INFIX 500 500 <->
INFIX 600 600 |->
INFIX 700 700 -><-
PREFIX inverse

OUTFIX {  }
OUTFIX <  >

LEFTFIX forall .
BIND x SCOPE P IN forall x . P

MENU TheirRules IS
    RULE "|* <-> , <| {} => |* |~"(P,Q,K,X) IS
        FROM P |* (Q,P) <-> K
        AND P <| { X } K
        INFER P |* Q |~ X
        
    RULE "|* |->, <| {} => |* |~"(P,Q,K,X) IS
        FROM P |* Q |-> K
        AND P <| {X} (inverse K)
        INFER P |* Q |~ X
    
    RULE "|* -><-, <| <> => |* |~"(P,Q,Y,X) IS
        FROM P |* (P,Q) -><- Y
        AND P <| <X> Y
        INFER P |* Q |~ X
        
    RULE "|* #, |* |~ => |* |*"(P,Q,X) IS
        FROM P |* #X
        AND P |* Q |~ X
        INFER P |* Q |* X
        
    RULE "|* |=>, |* |* => |*"(Q,P,X) IS
        FROM P |* Q |=> X
        AND P |* Q |* X
        INFER P |* X
        
    RULE "|* X, |* Y => |* X+Y"(P,X,Y) IS
         FROM P |* X
          AND P |* Y
        INFER P |* (X+Y)
        
    RULE "|* Y, X isin Y => |* X"(P,X,Y) IS
        FROM P |* Y
          AND X isin Y
        INFER P |* X
    
    RULE "|* |* Y, X isin Y => |* |* X"(P,Q,X,Y) IS
         FROM P |* Q |* Y
          AND X isin Y
        INFER P |* Q |* X
        
    RULE "|* |~ Y, X isin Y => |* |~ X"(P,Q,X,Y) IS
         FROM P |* Q |~ Y
          AND X isin Y
        INFER P |* Q |~ X
        
    RULE "<| Y, X isin Y => <| X"(P,X,Y) IS
         FROM P <| Y
          AND X isin Y
        INFER P <| X
        
    RULE "<| <X>Y => <| X"(P,X,Y) IS
        FROM P <| <X>Y
        INFER P <| X
        
    RULE "|* <->, <| {} => <|"(P,Q,K,X) IS
        FROM P |* (P,Q) <-> K
        AND P <| {X}K
        INFER P <| X
        
    RULE "|* |->, <| {} => <|"(P,K,X) IS
        FROM P |* P |-> K
        AND P <| {X}K
        INFER P <| X
        
    RULE "|* |->, <| {} inverse => <|"(P,Q,K,X) IS
        FROM P |* Q |-> K
        AND P <| {X} (inverse K)
        INFER P <| X
        
    RULE "|* #X, X isin Y => |* #Y"(P,X,Y) IS
         FROM P |* #X
          AND X isin Y
        INFER P |* #Y
        
    RULE "(|*) <-> commutes"(P,R,R',K) IS
        FROM P |* (R,R') <-> K
        INFER P |* (R',R) <-> K
        
    RULE "(|* |*) <-> commutes"(P,Q,R,R',K) IS
        FROM P |* Q |* (R,R') <-> K
        INFER P |* Q |* (R',R) <-> K
        
    RULE "(|*) -><- commutes"(P,R,R',K) IS
        FROM P |* (R,R') -><- K
        INFER P |* (R',R) -><- K
        
    RULE "(|* |*) -><- commutes"(P,Q,R,R',K) IS
        FROM P |* Q |* (R,R') -><- K
        INFER P |* Q |* (R',R) -><- K

    BUTTON "-" IS "-"
    
    RULE "X isin X"(X) IS INFER X isin X
    RULE "X isin A => X isin A+B"(X,A,B) IS
         FROM X isin A
        INFER X isin A+B
    RULE "X isin B => X isin A+B"(X,A,B) IS
         FROM X isin B
        INFER X isin A+B
END

MENU MyRules IS    
    RULE "hyp"(X) IS INFER X |- X
    
    BUTTON "-" IS "-"
    
    RULE "<| (X+Y) |-"(P,X,Y,C) IS
         FROM P <| X, P <| Y |- C
        INFER P <| (X+Y) |- C

    RULE "forall|-"(E,x,P) 
        WHERE VAR x AND SAFE P[x\E] IS 
        INFER forall x . P |- P[x\E]
END
