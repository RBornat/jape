/* 
    $Id$

    Copyright © 2003 Richard Bornat & Bernard Sufrin
     
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
import java.awt.GridLayout;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class TextDialog {
    private static String[] operators;

    public static void setOperators(String[] _operators) {
        operators=_operators;
    }

    public static void runNewConjectureDialog(String panel) {
        final String message = "Type a new conjecture for the "+panel+" panel";
        JTextField textField = new JTextField();
        class OperatorButtonListener implements ActionListener {
            private final String buttontitle;
            public OperatorButtonListener(String buttontitle) {
                this.buttontitle = buttontitle;
            }
            public void actionPerformed(ActionEvent newEvent) {
                Logger.log.println("operator button "+JapeUtils.enQuote(buttontitle));
            }
        }
        JPanel display = new JPanel();
        GridLayout gridLayout = new GridLayout(operators==null?2:3, 1);
        display.setLayout(gridLayout);
        display.add(new JLabel(message));
        display.add(textField);
        if (operators!=null) {
            ButtonPane buttonPane = new ButtonPane();
            for (int i=0; i<operators.length; i++) {
                JButton button = new JButton(operators[i]);
                button.addActionListener(new OperatorButtonListener(operators[i]));
                buttonPane.addButton(button);
            }
            buttonPane.doLayout();
            display.add(buttonPane);
        }

        String [] options = { "OK", "Cancel" };
        int reply = JOptionPane.showOptionDialog(JapeWindow.getTopWindow(), display,
                                                 "New Conjecture", 0,
                                                 JOptionPane.PLAIN_MESSAGE,
                                                 null, options, options[0]);
        if (reply==0 && textField.getText().length()!=0)
            Alert.showAlert(Alert.Plain, "panel "+JapeUtils.enQuote(panel)+
                            "; command "+JapeUtils.enQuote(textField.getText()));
    }
}
