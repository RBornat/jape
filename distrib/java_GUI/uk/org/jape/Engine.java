/* 
    $Id$

    Copyright © 2003 Richard Bornat & Bernard Sufrin
     
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

import java.io.InputStream;
import java.io.OutputStream;

public class Engine implements DebugConstants {
    private static Process engine;

    static {
        try {
            engine = Runtime.getRuntime().exec("./jape_engine");
        } catch (Exception exn) {
            /* StringWriter sw = new StringWriter();
            System.getProperties().list(new PrintWriter(sw)); */
            Alert.abort("can't start proof engine (exception "+exn+")\n"+
                        "(.="+System.getProperties().getProperty("user.dir")+")");
            engine = null; // shut up compiler
        }

        final InputStream fromEngine = engine.getInputStream();
        final OutputStream toEngine = engine.getOutputStream();
        final InputStream logEngine = engine.getErrorStream();

        JapeCharEncoding.init(fromEngine, toEngine, logEngine);

        class EngineLog implements Logger.LineReader {
            public String readLine() {
                try {
                    return JapeCharEncoding.loginputline();
                } catch (Exception exn) {
                    return null;
                }
            }
        }
        
        Thread engineLogger = new Logger.StreamLog("engine log", new EngineLog());
        engineLogger.start();

        new Dispatcher().start();
    }
}
