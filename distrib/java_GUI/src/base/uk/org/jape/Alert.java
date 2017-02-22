/* 
    Copyright © 2003-17 Richard Bornat & Bernard Sufrin
     
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

import java.awt.Component;
import java.awt.Dimension;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

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
    
    public static JLabel makeLabel(String s) {
	JLabel l = new JLabel(s.length()==0 ? " " : s);
	JapeFont.setComponentFont(l, JapeFont.DIALOGLABEL);
	return l;
    }

    private static String stringOfLabels(JLabel[] labels) {
	String s = "JLabel[] {";
	for (int i=0; i<labels.length; i++) {
	    s = s+JapeUtils.enQuote(labels[i].getText());
	    if (i+1<labels.length)
		s = s+"; ";
	}
	return s+"}";
    }
    
    // this is where we will eventually handle aspect ratio
    // and here we go ...
    // if x*y = A, and we want x/y to be 4/3 (like an old TV), then
    // x = sqrt(4*A/3). Hmm.
    
    // this method makes a message with a maximum width as given
    
    private static Object makeMessage(Object o, int maxwidth) {
	if (o instanceof String) {
	    String s = (String)o;
	    int nli;
	    JLabel[] result;
	    
	    if ((nli=s.indexOf('\n'))!=-1) {
		JLabel[] first = (JLabel[])makeMessage(s.substring(0,nli),maxwidth);
		JLabel[] second = (JLabel[])makeMessage(s.substring(nli+1),maxwidth);
		result = new JLabel[first.length+second.length];
		for (int i=0; i<first.length; i++)
		    result[i]=first[i];
		for (int i=0; i<second.length; i++)
		    result[i+first.length]=second[i];
	    }
	    else {
		JLabel l = makeLabel(s);
		TextDimension m = JapeFont.measure(l, s);
		if (m.width>maxwidth) {
		    String[] split = MinWaste.minwaste(l, s, maxwidth);
		    result = new JLabel[split.length];
		    for (int i=0; i<split.length; i++)
			result[i] = makeLabel(split[i]);
		}
		else
		    result = new JLabel[] { l };
	    }

	    if (makeMessage_tracing)
		Logger.log.println("makeMessage "+JapeUtils.enQuote(s)+" => "+stringOfLabels(result));
	    return result;
	}
	else
	    return o;
    }

    // some constants (not too critical) to make it possible to do window aspect ratio
    static final int OSXiconmargin = 20, OSXiconwidth = 64, OSXiconheight = 64, 
                     OSXrightmargin = 15, OSXtopmargin = 15, OSXbuttonsEtcmargin = 45,
                     
                     iconleftmargin = 12, iconwidth = 32, iconrightmargin = 12,
                     iconheight = 32, topmargin = 12, rightmargin = 12, 
                     buttonsEtcmargin = 45;
    
   private static Dimension measureAspect(JLabel[] labels) {
        int height = 0, width = 0;
        for (JLabel lab : labels) {
            TextDimension m = JapeFont.measure(lab, lab.getText());
            height += m.height;
            if (width<m.width) width = m.width;
        }
        
        // convert to window dimensions
        if (Jape.onMacOSX) { 
            width += OSXiconmargin+OSXiconwidth+OSXiconmargin+OSXrightmargin;
            if (height<OSXiconheight) height = OSXiconheight;
            height += OSXtopmargin+OSXbuttonsEtcmargin;
        }
        else {
            width += iconleftmargin+iconwidth+iconrightmargin+rightmargin;
            if (height<iconheight) height = iconheight;
            height += topmargin+buttonsEtcmargin;
        }
        
        return new Dimension(width,height);
    }
    
    // chosen by experiment. Looks better than 4/3.
    static final double idealratio = (double)16.0/(double)9.0;
    
    private static Object makeMessage(Object o) {
        int maxwidth = Jape.screenBounds.width*2/3;
        Object r = makeMessage(o,maxwidth);
        if (r instanceof JLabel[]) {
            JLabel[] labels = (JLabel[]) r;
            Dimension d = measureAspect(labels);
            int idealwidth = (int)(Math.sqrt((double)d.height*(double)d.width*idealratio));

            if (idealwidth<d.width) {
                // try aspect ratio adjustment
                // (really it should be aspect ratio of the window, but that's too hard)
                // because we have lots of time (it's an alert window, for goodness sake!)
                // do it by binary search
                int left = idealwidth, right = d.width;

                // debugging statements cos I wanted to watch it work :-)
                /* Logger.log.println("tried "+maxwidth+
                        " ratio "+((double)d.width/(double)d.height)+
                        " (should be "+idealratio+")"); */
                for (int mid = (left+right/2); left!=right; mid = (left+right)/2) {
                    labels = (JLabel[]) makeMessage(o, mid);
                    d = measureAspect(labels);
                    /* Logger.log.println("tried left "+left+" right "+right+" mid "+mid+
                            " ratio "+((double)d.width/(double)d.height)+
                            " (should be "+idealratio+")"); */
                    if ((double)d.width/(double)d.height<idealratio)
                        left = mid+1;
                    else
                        right = mid;
                }
            }

            return labels;
        }
        else
            return r;
    }
    
    public static void showAlert(Component parent, int messagekind, Object message) {
	// I don't think this needs invokeLater ...
	JOptionPane.showMessageDialog(parent,makeMessage(message),null,messagekind);
    }

    public static void showAlert(int messagekind, Object message) {
	showAlert(JapeWindow.getTopWindow(), messagekind, message);
    }

    public static void showAlert(Object message) {
	showAlert(Info, message);
    }

    static String quit="Quit", cont="Continue";

    public static void showErrorAlert(String message) {
	String[] buttons = { quit, cont };
	int reply = JOptionPane.showOptionDialog(JapeWindow.getTopWindow(), makeMessage(message),
						 "GUI error", 0, Error,
						 null, buttons, quit);
	// -1 means, in effect, continue (i.e. cancel)
	if (reply==0)
	   Jape.crash("GUI error: "+message);
    }

    public static void abort(String title, String message) {
        String[] buttons = { quit };
        JOptionPane.showOptionDialog(JapeWindow.getTopWindow(), makeMessage(message),
                                                 title, 0, Error,
                                                 null, buttons, quit);
        // window close doesn't matter
        Logger.crash("disaster "+title+": "+message, 2);
    }
    
    public static void guiAbort(String message) {
        abort("GUI disaster", message);
    }

    // this doesn't deal with fonts yet ... I think we have to make a Component (sigh)
    public static int myShowOptionDialog(Component parent, String[] buttons, int messageKind,
			     String message, int defaultbutton) {
	/* if the buttons are either just OK or include Cancel, we take window closure 
	 * (Java, sigh) as Cancel. Otherwise we stop closure and put an explanatory 
	 * dialog message on the window-close button.
	 */
        
        // if it's just OK ...
        if (buttons.length==1 && buttons[0].equalsIgnoreCase("OK")) {
            JOptionPane.showMessageDialog(parent,makeMessage(message),null,messageKind);
            return 0;
        }

        // if it has Cancel ...
        int hascancel = -1;
        for (int j=0; j<buttons.length; j++)
            if (buttons[j].equals("Cancel")) 
                hascancel=j;
        
        if (hascancel>=0) {
            int i = JOptionPane.showOptionDialog(parent, makeMessage(message), null, 0, messageKind,
                    null, buttons, buttons[defaultbutton]);
            return i==JOptionPane.CLOSED_OPTION ? hascancel : i;
        }
        
        // otherwise we do it the slow way
        return nonCancellableDialog(parent,message,messageKind,buttons,defaultbutton,
                    "Please don't press the close button: press one of the buttons "+stringOfButtons(buttons,0),
                    "Please read this explanation.\n\n"+
                    "The dialog you tried to close asks you to make a choice. "+
                    "Jape can't proceed until you choose.\n\n"+
                    "You make your choice by pressing one of the buttons "+stringOfButtons(buttons,0)+
                    " -- closing the window isn't a choice, so the close button doesn't work.\n\n"+
                    "And it doesn't work on this window, either.");
    }
    
    public static int myShowOptionDialog(String[] buttons, int severity, String message, int defaultbutton)
				 throws ProtocolError {
	int result = myShowOptionDialog(JapeWindow.getTopWindow(), buttons, messagekind(severity),
			   message, defaultbutton);
	/* Logger.log.println("Alert.ask; message="+JapeUtils.enQuote(message)+
			   "; buttons="+JapeUtils.stringOfArray(buttons, ",", true)+
			   "; default="+defaultbutton+
			   "; result="+result); */
	return result;
    }

    public static int nonCancellableDialog(Component parent, String message, final int messageKind, 
            String[] buttons, int defaultbutton, final String altmessage1, final String altmessage2) {
        final JOptionPane pane = new JOptionPane(makeMessage(message), messageKind, 0, 
                null, buttons, buttons[defaultbutton]);
        final JDialog dialog = pane.createDialog(parent, null);
        dialog.setContentPane(pane);
        dialog.setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
        if (altmessage1!=null) {
            dialog.addWindowListener(new WindowAdapter() {
                public void windowClosing(WindowEvent we) {
                    nonCancellableDialog(dialog, altmessage1, messageKind, new String[]{"OK"}, 
                            0, altmessage2, null);
                }
            });
        }
        dialog.setVisible(true);
        
        Object selectedValue = pane.getValue();
        for (int i=0; i<buttons.length; i++)
            if (buttons[i].equals(selectedValue))
                return i;
        
        // oh bugger: can't happen
        guiAbort("nonCancellableDialog returns "+selectedValue+" with "+buttons.length+" buttons");
        return 0; // really won't happen
    }
    
    public static String stringOfButtons(String[] buttons, int i) {
        if (i<0 || i>=buttons.length)
            return "";
        else
        if (i==buttons.length-1)
            return "\""+buttons[i]+"\"";
        else
        if (i==buttons.length-2)
            return "\""+buttons[i]+"\" and \""+buttons[i+1]+"\"";
        else
            return "\""+buttons[i]+"\", "+stringOfButtons(buttons,i+1);
    }
    public static final int OK	   = 0,
			    Cancel = 1;

    public static int askOKCancel(Component parent, String message) {
	int q = JOptionPane.showConfirmDialog(parent, makeMessage(message), null, 
	                JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE);
	return q==JOptionPane.OK_OPTION ? OK : Cancel;
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
            case 1 : // cancel
            case JOptionPane.CLOSED_OPTION: // window closed
                     return 0;
	    case 0 : // doit
	             return 1;
            case 2 : // dont
                     return 2;
            default: // can't happen
                     return 0;
	}
    }
}
