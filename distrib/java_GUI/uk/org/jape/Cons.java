//
//  Cons.java
//  japeserver
//
//  Created by Richard Bornat on Wed Sep 04 2002.
//  Copyright (c) 2002 __MyCompanyName__. All rights reserved.
//

public class Cons {
    public final Object car;
    public final Cons cdr;
    public Cons(Object car, Cons cdr) {
        this.car = car; this.cdr = cdr;
    }
}
