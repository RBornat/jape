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

import java.awt.Component;
import java.awt.Container;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import java.util.Vector;

public class Alert {
    // oh the ceaseless dance of interface conversions ..
    public static final int Plain = JOptionPane.PLAIN_MESSAGE;
    public static final int Info = JOptionPane.INFORMATION_MESSAGE;
    public static final int Warning = JOptionPane.WARNING_MESSAGE;
    public static final int Error = JOptionPane.ERROR_MESSAGE;
    public static final int Question = JOptionPane.QUESTION_MESSAGE;
    
    private static int messagekind(int severity) throws ProtocolError {
        switch (severity) {
          case 0: return Info;
          case 1: return Warning;
          case 2: return Error;
          case 3: return Question;
          default: throw (new ProtocolError(severity+" should be message severity (0:info, 1:warning, 2:error, 3: question)"));
        }
    }
    
    private static JLabel makeLabel(String s) {
        JLabel l = new JLabel(s);
        JapeFont.setComponentFont(JapeFont.DIALOGLABEL, l);
        return l;
    }
    
    // this is where we will eventually process multi-line strings and 
    // handle aspect ratio
    private static JLabel[] makeMessage(String s) {
        JLabel l = makeLabel(s);
        TextDimension m = JapeFont.measure(l, s);
        if (m.width>japeserver.screenBounds.width*2/3) {
            String[] split = JapeFont.minwaste(l, s, japeserver.screenBounds.width*4/10);
            JLabel[] ls = new JLabel[split.length];
            for (int i=0; i<ls.length; i++)
                ls[i] = makeLabel(split[i]);
            return ls;
        }
        else {
            JLabel[] ls = { l };
            return ls;
        }
    }
    
    public static void showAlert(int messagekind, String message) {
        // I don't think this needs invokeLater ...
        JOptionPane.showMessageDialog(null,makeMessage(message),null,messagekind);
    }
    
    static String quit="Quit", cont="Continue";

    public static void showErrorAlert(String message) {
        String[] buttons = { quit, cont };
        int reply = JOptionPane.showOptionDialog(
                        null, makeMessage(message), "GUI error", 0, Error,
                        null, buttons, quit);
        if (reply==0)
            System.exit(2);
    }
    
    // this doesn't deal with fonts yet ... I think we have to make a Component (sigh)
    // and I haven't worked out what to do with defaultbutton ...
    public static void newAlert (String[] buttons, int severity, String message, int defaultbutton) 
    throws ProtocolError {
        if (buttons.length==1 && buttons[0].equals("OK")) {
            showAlert(messagekind(severity), message);
            Reply.reply("0"); // I hope
        }
        else {
            String s = "can't yet show alert: [";
            for (int i=0; i<buttons.length; i++) 
                s=s+(i==0?"\"":",\"")+buttons[i]+"\"";
            showErrorAlert(s+" "+severity+" \""+message+"\" "+defaultbutton);
            Reply.reply(defaultbutton+""); // I hope
        }
    }
}
