//
//  Reply.java
//  japeserver
//
//  Created by Richard Bornat on Tue Sep 03 2002.
//  Copyright (c) 2002 __MyCompanyName__. All rights reserved.
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
            System.out.println(s);
            clientlistening=false;
        }
    }
    
    synchronized public static void sendCOMMAND(String s) {
        messages.add("COMMAND "+s);
        sendmessage();
    }

    synchronized public static void reply(String s) throws ProtocolError {
        if (!clientlistening)
            System.out.println(s);
        else
            throw (new ProtocolError("replying "+s+"\" while client is not expecting reply"));
    }	
}
