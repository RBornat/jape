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

import java.awt.Component;
import java.awt.Container;

import java.util.Vector;

import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

public class Alert implements DebugConstants {
    // oh the ceaseless dance of interface conversions ..
    public static final int Plain    = JOptionPane.PLAIN_MESSAGE;
    public static final int Info     = JOptionPane.INFORMATION_MESSAGE;
    public static final int Warning  = JOptionPane.WARNING_MESSAGE;
    public static final int Error    = JOptionPane.ERROR_MESSAGE;
    public static final int Question = JOptionPane.QUESTION_MESSAGE;
    
    private static int messagekind(int severity) throws ProtocolError {
        switch (severity) {
          case 0 : return Info;
          case 1 : return Warning;
          case 2 : return Error;
          case 3 : return Question;
          default: throw new ProtocolError(severity+" should be message severity (0:info, 1:warning, 2:error, 3: question)");
        }
    }
    
    private static JLabel makeLabel(String s) {
        JLabel l = new JLabel(s);
        JapeFont.setComponentFont(l, JapeFont.DIALOGLABEL);
        return l;
    }

    private static String stringOfLabels(JLabel[] labels) {
        String s = "JLabel[] {";
        for (int i=0; i<labels.length; i++) {
            s = s+"\""+labels[i].getText()+"\"";
            if (i+1<labels.length)
                s = s+"; ";
        }
        return s+"}";
    }
    
    // this is where we will eventually handle aspect ratio
    private static Object makeMessage(Object o) {
        if (o instanceof String) {
            String s = (String)o;
            int nli;
            JLabel[] result;
            
            if ((nli=s.indexOf('\n'))!=-1) {
                JLabel[] first = (JLabel[])makeMessage(s.substring(0,nli));
                JLabel[] second = (JLabel[])makeMessage(s.substring(nli+1));
                result = new JLabel[first.length+second.length];
                for (int i=0; i<first.length; i++)
                    result[i]=first[i];
                for (int i=0; i<second.length; i++)
                    result[i+first.length]=second[i];
            }
            else {
                JLabel l = makeLabel(s);
                TextDimension m = JapeFont.measure(l, s);
                if (m.width>japeserver.screenBounds.width*2/3) {
                    String[] split = MinWaste.minwaste(l, s, japeserver.screenBounds.width*4/10);
                    result = new JLabel[split.length];
                    for (int i=0; i<split.length; i++)
                        result[i] = makeLabel(split[i]);
                }
                else
                    result = new JLabel[] { l };
            }

            if (makeMessage_tracing)
                Logger.log.println("makeMessage \""+s+"\" => "+stringOfLabels(result));
            return result;
        }
        else
            return o;
    }

    public static void showAlert(Component parent, int messagekind, Object message) {
        // I don't think this needs invokeLater ...
        JOptionPane.showMessageDialog(parent,makeMessage(message),null,messagekind);
    }

    public static void showAlert(int messagekind, Object message) {
        showAlert(JapeWindow.getTopWindow(), messagekind, message);
    }

    static String quit="Quit", cont="Continue";

    public static void showErrorAlert(String message) {
        String[] buttons = { quit, cont };
        int reply = JOptionPane.showOptionDialog(JapeWindow.getTopWindow(), makeMessage(message),
                                                 "GUI error", 0, Error,
                                                 null, buttons, quit);
        if (reply==0)
            System.exit(2);
    }

    public static void abort(String message) {
        String[] buttons = { quit };
        int reply = JOptionPane.showOptionDialog(JapeWindow.getTopWindow(), makeMessage(message),
                                                 "GUI disaster", 0, Error,
                                                 null, buttons, quit);
        System.exit(2);
    }

    // this doesn't deal with fonts yet ... I think we have to make a Component (sigh)
    private static int query(Component parent, String[] buttons, int messageKind,
                             String message, int defaultbutton) {
        return JOptionPane.showOptionDialog(parent, makeMessage(message), null, 0, messageKind,
                                            null, buttons, buttons[defaultbutton]);
    }
    
    public static int ask(String[] buttons, int severity, String message, int defaultbutton)
                                 throws ProtocolError {
        return query(JapeWindow.getTopWindow(), buttons, messagekind(severity),
                     message, defaultbutton);
    }

    public static final int OK     = 0,
                            Cancel = 1;

    public static int askOKCancel(Component parent, String message) {
        return query(parent, new String[] { "OK", "Cancel"}, Question, message, 0);
    }

    public static int askOKCancel(String message) {
        return askOKCancel(JapeWindow.getTopWindow(), message);
    }

    public static int askDangerously(String message, String doit, String dont) {
        String[] buttons = { doit, "Cancel", dont };
        int reply = JOptionPane.showOptionDialog(JapeWindow.getTopWindow(), makeMessage(message),
                                                 null, 0, Question,
                                                 null, buttons, doit);
        // in reply 0 means Cancel, 1 means doit, 2 means don't
        switch (reply) {
            case 0 : return 1;
            case 1 : return 0;
            default: return reply;
        }
    }
}
