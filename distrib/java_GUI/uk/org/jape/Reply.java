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

import java.awt.FontMetrics;
import java.awt.Rectangle;

import java.io.IOException;

import java.util.Vector;

public class Reply implements DebugConstants {
    public final static String stringSep = "\u0001"; // what separates parts of multi-string replies
    
    private static Vector messages = new Vector();
    private static boolean clientlistening = false;
    
    synchronized public static void openchannel() {
        clientlistening = true;
        flushmessages();
    }

    public static final JapeCharEncoding decoder = new JapeCharEncoding(System.in, System.out);
    
    synchronized private static void flushmessages() {
        try {
            if (clientlistening && messages.size()!=0) {
                String s = (String)messages.remove(0);
                if (protocol_tracing) System.err.println("GUI sends "+s);
                decoder.outputln(s);
                decoder.flush();
                clientlistening=false;
            }
        } catch (IOException e) {
            Alert.abort("Reply.flushmessages gets IOException \""+e.getMessage()+"\"");
        }
    }

    synchronized public static void send(String s) {
        if (protocol_tracing) System.err.println("queuing "+s);
        messages.add(s);
        flushmessages();
    }
    
    public static void sendCOMMAND(String s) {
        send("COMMAND "+s);
    }

    synchronized public static void reply(String s) throws ProtocolError {
        if (!clientlistening) {
            try {
                if (protocol_tracing) System.err.println("GUI replies "+s);
                decoder.outputln(s);
                decoder.flush();
            } catch (IOException e) {
                Alert.abort("Reply.reply gets IOException \""+e.getMessage()+"\"");
            }
        }
        else
            throw new ProtocolError("replying \""+s+"\" while client is not expecting reply");
    }

    synchronized public static void reply(int i) throws ProtocolError {
        reply(i+"");
    }

    synchronized public static void reply(TextDimension td) throws ProtocolError {
        reply(td.width+" "+td.ascent+" "+td.descent);
    }
    
    synchronized public static void reply(FontMetrics fm) throws ProtocolError {
        reply(fm.getMaxAscent()+" "+fm.getMaxDescent()+" "+fm.getLeading());
    }

    synchronized public static void reply(Rectangle r) throws ProtocolError {
        reply(r.x+" "+r.y+" "+r.width+" "+r.height);
    }
}
