/* 
    $Id$

    Copyright © 2002 Richard Bornat & Bernard Sufrin
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

    This file is part of japeserver, which is part of jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).
    
*/

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
            if (Debugging.protocol_tracing) System.err.println("sending "+s);
            System.out.println(s);
            clientlistening=false;
        }
    }
    
    synchronized public static void sendCOMMAND(String s) {
        String m = "COMMAND "+s;
        if (Debugging.protocol_tracing) System.err.println("queuing "+m);
        messages.add(m);
        sendmessage();
    }

    synchronized public static void reply(String s) throws ProtocolError {
        if (Debugging.protocol_tracing) System.err.println("replying "+s);
        if (!clientlistening)
            System.out.println(s);
        else
            throw (new ProtocolError("replying "+s+"\" while client is not expecting reply"));
    }	
}
