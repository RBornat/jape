//
//  ProtocolError.java
//  japeserver
//
//  Created by Richard Bornat on Wed Sep 04 2002.
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
//


public class ProtocolError extends Exception {
    ProtocolError(String s) { super(s); }
    ProtocolError() { super(); }
}
    
