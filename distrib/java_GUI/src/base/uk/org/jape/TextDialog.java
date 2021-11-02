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

import java.awt.Component;
import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
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
    
    private static String lastTacticCommand = "";
    
    public static void runTacticCommandDialog() {
        String newCommand = runTextDialog("Tactic", lastTacticCommand, 
                                          "Type a 'tactic' to be applied in the proof window");
        if (newCommand!=null) {
            lastTacticCommand = newCommand;
            Reply.sendCOMMAND("APPLY "+newCommand);
        }
    }
        
    private static String lastConjecture = "";
    
    public static void runNewConjectureDialog(String panel) {
        String newConjecture = runTextDialog("New Conjecture", lastConjecture, 
                                             "Type a new conjecture for the "+panel+" panel");
        if (newConjecture!=null) {
            lastConjecture = newConjecture;
            Reply.sendCOMMAND("addnewconjecture "+JapeUtils.enQuote(panel)+" "+newConjecture); /* argument must not be quoted */
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
            Alert.guiAbort("TextDialog.runUnifyDialog can't reply");
        }   
    }

    public static String runTextDialog(String title, String oldText, String message) {
        final JTextField textField = new JTextField(oldText);
        JapeFont.setComponentFont(textField, JapeFont.TEXTINPUT);
        if (oldText!=null){
            textField.setSelectionStart(0);
            textField.setSelectionEnd(oldText.length());
            // Logger.log.println("new window");
        }
        class OperatorButtonListener implements ActionListener {
            public void actionPerformed(ActionEvent newEvent) {
                String s = newEvent.getActionCommand();
                try {
                    int caret = textField.getCaretPosition(),
                    selstart = textField.getSelectionStart(),
                    selend = textField.getSelectionEnd();

                    // Logger.log.println("selection is "+selstart+","+selend);
                    if (selstart<selend) {
                        // imitate a text editor, sigh!
                        textField.getDocument().remove(selstart, selend-selstart);
                        caret = selstart;
                    }
                    textField.getDocument().insertString(caret, s, null);
                    // Logger.log.println("selection now "+selstart+","+selend);
                    textField.requestFocusInWindow();
                    // Logger.log.println("selection should be "+newcaret+","+newcaret+" is in fact "+textField.getSelectionStart()+","+textField.getSelectionEnd());
                } catch (Exception exn) {
                    Alert.guiAbort("Button "+s+" threw exception "+exn);
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
            for (String o: operators) {
                JButton button = new JButton(o);
                button.addActionListener(buttonPaneListener);
                JapeFont.setComponentFont(button, JapeFont.TEXTINPUT);
                buttonPane.addButton(button);
                button.setFocusable(false);
            }
            buttonPane.doLayout();
            display.add(buttonPane);
        }

        // in order to get the text focus in the text field when we start, it's
        // necessary to run JOptionPane by hand.  Code based on the Sun 1.3.1 docs.
        // And then lots of pragmatic stuff to make it give the focus from the OK 
        // button (which gets it by default) and give it to the text field.
        String [] options = { "OK", "Cancel" };
        final JOptionPane pane = new JOptionPane(display, JOptionPane.PLAIN_MESSAGE, JOptionPane.OK_CANCEL_OPTION,
                                           null, options, options[0]);
        // pane.set.Xxxx(...); // Configure
        final JDialog dialog = pane.createDialog(JapeWindow.getTopWindow(), title);
        // for some reason the dialog doesn't entirely include the OK and Cancel buttons ...
        if (!Jape.onMacOSX)
            dialog.setSize(dialog.getWidth(), dialog.getHeight()+26);
        dialog.addWindowListener(new WindowAdapter() {
            public void windowActivated(WindowEvent e) {
                for (Component c: pane.getComponents()){
                    // pragmatically, it is in a JPanel 
                    if (c instanceof JPanel) {
                        for (Component c1: ((Container)c).getComponents()){
                            if (c1 instanceof JButton && ((JButton)c1).getText().equals("OK")) {
                                ((JButton)c1).addFocusListener(new FocusAdapter(){
                                    Boolean first = true;
                                    public void focusGained(FocusEvent e){
                                        if (first) {
                                            first = false;
                                            textField.requestFocusInWindow();
                                        }
                                    }
                                });
                            }
                        }
                    }
                }
            }
        });

        if (Jape.onMacOSX && System.getProperty("java.vm.version").startsWith("1.5.0_")) {
            textField.addFocusListener(new FocusAdapter(){
                Boolean first = true, atend = false;
                public void focusLost(FocusEvent e) {
                    // for some reason it loses the focus once ...
                    atend = !first && textField.getSelectionEnd()==textField.getText().length();
                    // Logger.log.println("focus lost "+first+" "+atend);
                    first = false;
                }
                public void focusGained(FocusEvent e) {
                    if (atend) {
                        // Logger.log.println("resetting selection to end of "+textField.getText());
                        textField.setSelectionStart(textField.getSelectionEnd());
                    }
                }
            });}
        
        JapeMenu.setDialogMenuBar(JapeMenu.TEXTDIALOGWINDOW_BAR|JapeMenu.DIALOGWINDOW_BAR, 
                                  dialog, title);
        dialog.setVisible(true);
        
        Object selectedValue = pane.getValue();
        /* int reply = JOptionPane.CLOSED_OPTION;
        if (selectedValue!=null)
            for(int counter = 0, maxCounter = options.length;
                counter < maxCounter; counter++) {
                if(options[counter].equals(selectedValue)) {
                    reply = counter; break;
                }
            } */
                
        String result = textField.getText();

        JapeMenu.undoDialogMenuBar();
        dialog.dispose(); // I hope
        
        return selectedValue!=null && options[0].equals(selectedValue) && 
                 result.length()!=0 ? result : null;
    }
}

