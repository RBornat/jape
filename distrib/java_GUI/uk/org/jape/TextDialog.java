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

import java.awt.GridLayout;
import java.awt.Insets;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class TextDialog {
    private static String[] operators;

    public static void setOperators(String[] _operators) {
	operators=_operators;
    }

    private static String lastCommand = "";
    
    public static void runTextCommandDialog() {
	String newCommand = runTextDialog("Text Command", lastCommand, 
					  "Type a command to be sent to the Jape engine");
	if (newCommand!=null) {
	    lastCommand = newCommand;
	    Reply.sendCOMMAND(newCommand);
	}
    }
	
    private static String lastConjecture = "";
    
    public static void runNewConjectureDialog(String panel) {
	String newConjecture = runTextDialog("New Conjecture", lastConjecture, 
					     "Type a new conjecture for the "+panel+" panel");
	if (newConjecture!=null) {
	    lastConjecture = newConjecture;
	    Reply.sendCOMMAND("addnewconjecture "+JapeUtils.enQuote(panel)+
			      " "+newConjecture);
	}
    }
    
    private static String lastUnify = "";
    
    public static void runUnifyDialog(String formula) {
	String newFormula = runTextDialog("Unify", lastUnify, 
					  "Type a formula to unify with "+formula);
	try {
	    if (newFormula!=null) {
		lastUnify = newFormula;
		Reply.reply(newFormula);
	    }
	    else
		Reply.reply(""); 
	} catch (ProtocolError e) {
	    Alert.abort("TextDialog.runUnifyDialog can't reply");
	}   
    }

    public static String runTextDialog(String title, String oldText, String message) {
	final JTextField textField = new JTextField(oldText);
	JapeFont.setComponentFont(textField, JapeFont.TEXTINPUT);
	class OperatorButtonListener implements ActionListener {
	    public void actionPerformed(ActionEvent newEvent) {
		try {
		    textField.getDocument().insertString(textField.getCaretPosition(),
							 newEvent.getActionCommand(), null);
		    textField.requestFocus();
		} catch (Exception exn) {
		    Alert.abort("Button "+newEvent.getActionCommand()+" threw exception "+exn);
		}
	    }
	}
	OperatorButtonListener buttonPaneListener = new OperatorButtonListener();
	JPanel display = new JPanel();
	display.setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 0));
	GridLayout gridLayout = new GridLayout(operators==null?2:3, 1);
	display.setLayout(gridLayout);
	display.add(new JLabel(message));
	display.add(textField);
	if (operators!=null) {
	    ButtonPane buttonPane = new ButtonPane();
	    for (int i=0; i<operators.length; i++) {
		JButton button = new JButton(operators[i]);
		button.addActionListener(buttonPaneListener);
		JapeFont.setComponentFont(button, JapeFont.TEXTINPUT);
		buttonPane.addButton(button);
	    }
	    buttonPane.doLayout();
	    display.add(buttonPane);
	}

	// in order to get the text focus in the text field when we start, it's
	// necessary to run JOptionPane by hand.  Code based on the Sun 1.3.1 docs.
	String [] options = { "OK", "Cancel" };
	JOptionPane pane = new JOptionPane(display, JOptionPane.PLAIN_MESSAGE, 0,
					   null, options, options[0]);
	// pane.set.Xxxx(...); // Configure
	JDialog dialog = pane.createDialog(JapeWindow.getTopWindow(), title);
	dialog.addWindowListener(new WindowAdapter() {
	    public void windowActivated(WindowEvent e) {
		// Logger.log.println("dialog windowActivated");
		textField.requestFocus();
	    }
	});
	dialog.show();
	
	Object selectedValue = pane.getValue();
	/* int reply = JOptionPane.CLOSED_OPTION;
	if (selectedValue!=null)
	    for(int counter = 0, maxCounter = options.length;
		counter < maxCounter; counter++) {
		if(options[counter].equals(selectedValue)) {
		    reply = counter; break;
		}
	    } */
		
	String result;
	return selectedValue!=null && options[0].equals(selectedValue) && 
	    (result = textField.getText()).length()!=0 ? result : null;
    }
}
