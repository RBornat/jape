//
//  Reply.java
//  japeserver
//
//  Created by Richard Bornat on Tue Sep 03 2002.
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
//

import java.util.*;

public class Reply {
    private static Vector messages = new Vector();
    private static boolean clientlistening = false;
    
    synchronized public static void openchannel() {
        clientlistening = true;
        sendmessage();
    }
    
    synchronized private static void sendmessage() {
        if (clientlistening && messages.size()!=0) {
            String s = (String)messages.remove(0);
            // System.err.println("sending "+s);
            System.out.println(s);
            clientlistening=false;
        }
    }
    
    synchronized public static void sendCOMMAND(String s) {
        // System.err.println("queuing "+"COMMAND "+s);
        messages.add("COMMAND "+s);
        sendmessage();
    }

    synchronized public static void reply(String s) throws ProtocolError {
        // System.err.println("replying "+s);
        if (!clientlistening)
            System.out.println(s);
        else
            throw (new ProtocolError("replying "+s+"\" while client is not expecting reply"));
    }	
}
