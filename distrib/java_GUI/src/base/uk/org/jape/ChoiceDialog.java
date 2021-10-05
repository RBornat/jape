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
import java.awt.event.MouseEvent;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;

public class ChoiceDialog {

        @SuppressWarnings("serial")
        private static class Choice extends JPanel {
                final int n, nlines;
                Choice(String str, int n) {
                        JLabel[] m = wrap(str);
                        this.nlines = m.length;
                        this.n = n;
                        this.setLayout(new BoxLayout(this,BoxLayout.PAGE_AXIS));
                        add(Box.createVerticalStrut(2));
                        for (int i=0; i<m.length; i++)
                                add(m[i]);
                        add(Box.createVerticalStrut(2));
                }
                public String toString() {
                        return "ChoiceDialog.Choice["+getX()+","+getY()+","+getWidth()+"x"+getHeight()+
                        "; n="+n+"; nlines="+nlines+"]";
                }
        }

        static class Renderer implements ListCellRenderer<Choice> {
                public Component getListCellRendererComponent(JList<? extends Choice> list, Choice value, int index,
                                boolean isSelected, boolean cellHasFocus) {
                        // Logger.log.println("rendering "+value+" at index "+index+
                        //         " isSelected "+isSelected+" cellHasFocus "+cellHasFocus);
                        // JapeUtils.showContainer((Choice)value);
                        Choice e = (Choice)value;
                        if (isSelected) {
                                e.setBackground(list.getSelectionBackground());
                                e.setForeground(list.getSelectionForeground());
                        }
                        else {
                                e.setBackground(list.getBackground());
                                e.setForeground(list.getForeground());
                        }
                        return e;
                }
        }

        private static Vector<Choice> list = new Vector<Choice>();

        static JLabel[] wrap(String s, int width) {
                JLabel[] result;
                int nli;
                if ((nli=s.indexOf('\n'))!=-1) {
                        JLabel[] first = wrap(s.substring(0,nli));
                        JLabel[] second = wrap(s.substring(nli+1));
                        result = new JLabel[first.length+second.length];
                        System.arraycopy(first, 0, result, 0, first.length);
                        System.arraycopy(second, 0, result, first.length, second.length);
                } 
                else {
                        JLabel l = Alert.makeLabel(s);
                        TextDimension m = JapeFont.measure(l, s);
                        if (m.width>Jape.screenBounds.width*2/3) {
                                String[] split = MinWaste.minwaste(l, s, width);
                                result = new JLabel[split.length];
                                for (int i=0; i<split.length; i++)
                                        result[i] = Alert.makeLabel(split[i]);
                        }
                        else
                                result = new JLabel[] { l };
                }

                return result;
        }

        static JLabel[] wrap(String s) {
                return wrap(s, Jape.screenBounds.width*4/10);
        }

        private static class ListSelectionListener extends JapeMouseAdapter {
                public void doubleclicked(MouseEvent e) {
                        // JList list = (JList)e.getSource();
                        // int index = list.locationToIndex(e.getPoint());
                        // Alert.showAlert(list.getModel().getElementAt(index).toString());
                        Alert.showAlert("(Press OK in the \"Choose a match\" window to accept the choice you made).");
                }
        }

        static void clearChoices() {
                list.removeAllElements();
        }

        static void setChoice(String str, int n) {
                list.add(new Choice(str, n));
        }

        static int makeChoice(String message) {
                Choice[] choices = (Choice[])list.toArray(new Choice[list.size()]);
                JList<Choice> choicelist = new JList<Choice>(choices);
                JScrollPane sp = new JScrollPane(choicelist);

                int i, nlines;
                for (i=0, nlines=0; i<choices.length && nlines<8; i++)
                        nlines += choices[i].nlines;
                choicelist.setVisibleRowCount(i);
                choicelist.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                choicelist.setSelectedIndex(0);
                choicelist.addMouseListener(new ListSelectionListener());
                choicelist.setCellRenderer(new Renderer());

                JLabel[] m = wrap(message);
                Object[] ml = new Object[m.length+1];

                System.arraycopy(m, 0, ml, 0, m.length);
                ml[ml.length-1] = sp;

                int res = JOptionPane.showConfirmDialog(null, ml, "Choose a match", 
                                JOptionPane.OK_CANCEL_OPTION);
                return res==JOptionPane.OK_OPTION ? ((Choice)choicelist.getSelectedValue()).n : 0;
        }
}

