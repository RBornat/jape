/* 
        Copyright © 2003-19 Richard Bornat & Bernard Sufrin
     
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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;

public class Engine implements DebugConstants {
    private static Process engine;
    private static BufferedReader fromEngine, logEngine;
    private static BufferedWriter toEngine;
    
    public Engine (String[] cmd) {
        super();
        String apn = new java.io.File(".").getAbsoluteFile().toString();
        // Alert.abort(apn);
        try {
            // System.out.println(System.getProperty("user.dir"));
            engine = Runtime.getRuntime().exec(cmd);
        } catch (Exception exn) {
            String s = "can't start proof engine in directory "+System.getProperty("user.dir")+
                       " (exception "+exn+")\ncmd =[";
            for (int i=0; i<cmd.length; i++) {
                s = s+JapeUtils.enQuote(cmd[i]);
                if (i+1<cmd.length)
                    s = s+", ";
            }
            s=s+"("+apn+")";
            Alert.guiAbort(s+"]");
            engine = null; // shut up compiler
        }

        try {
            fromEngine = new BufferedReader(new InputStreamReader(engine.getInputStream(), "UTF-8"));
            toEngine = new BufferedWriter(new OutputStreamWriter(engine.getOutputStream(), "UTF-8"));
            logEngine = new BufferedReader(new InputStreamReader(engine.getErrorStream(), "UTF-8"));
        } catch (UnsupportedEncodingException e) {
            Alert.guiAbort("We don't support encoding "+JapeUtils.enQuote("UTF-8"));
        }

        Thread engineLogger = new Logger.StreamLog("engine log", new EngineLog());
        engineLogger.start();
        new Dispatcher().start();
    }
    
    public static BufferedReader fromEngine() {
        return fromEngine;
    }
    public static BufferedWriter toEngine() {
        return toEngine;
    }
    public static BufferedReader logEngine() {
        return logEngine;
    }
    
    class EngineLog implements Logger.LineReader {
        public String readLine() {
            try {
                return logEngine.readLine();
            } catch (Exception exn) {
                return null;
            }
        }
    }
}



