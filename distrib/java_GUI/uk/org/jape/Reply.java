/* 
    $Id$

    Copyright © 2003-4 Richard Bornat & Bernard Sufrin
     
    richard@bornat.me.uk
    sufrin@comlab.ox.ac.uk

    This file is part of the Jape GUI, which is part of Jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).
    
*/

package uk.org.jape;

import java.awt.FontMetrics;
import java.awt.Rectangle;

import java.io.IOException;

import java.util.Vector;

public class Reply implements DebugConstants {
    public static final String stringSep = "\u0091"; // what separates parts of multi-string replies
    
    private static Vector messages = new Vector();
    private static boolean enginelistening = false;
    
    synchronized public static boolean openchannel() {
    enginelistening = true;
    flushmessages();
    return enginelistening;
    }

    synchronized private static void flushmessages() {
    if (enginelistening && messages.size()!=0) {
	String s = (String)messages.remove(0);
	if (DebugVars.protocol_tracing) 
	    Logger.log.println("GUI sends "+s);
	outputln(s);
	enginelistening=false;
    }
    }

    synchronized public static void send(String s) {
	if (DebugVars.protocol_tracing) Logger.log.println("queuing "+s);
	    messages.add(s);
	    flushmessages();
    }
    
    synchronized public static void sendCOMMAND(String s) {
	send("COMMAND "+s);
    }

    synchronized public static void reply(String s) throws ProtocolError {
    if (!enginelistening) {
	if (DebugVars.protocol_tracing) Logger.log.println("GUI replies "+s);
	outputln(s);
    }
    else
	throw new ProtocolError("replying "+JapeUtils.enQuote(s)+" while client is not expecting reply");
    }

    synchronized private static void outputln(String s) {
    try {
	Engine.toEngine().write(s, 0, s.length()); 
	Engine.toEngine().write('\n');
	Engine.toEngine().flush();
	} catch (IOException e) {
	    Alert.showAlert("Reply.outputln("+JapeUtils.enQuote(s)+") gets exception "+e);
	}
    }
        
    synchronized public static void reply(int i) throws ProtocolError {
	reply(""+i);
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
