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

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

public class DebugVars {
    public static final boolean showDebugVars = true;

    public static boolean protocol_tracing        = false,
			  menuaction_tracing      = false,
			  loopback_tracing        = false,
			  printdialog_tracing     = false,
			  containerlayout_tracing = false,
			  paint_tracing           = false,
			  measure_tracing         = false,
			  drag_tracing            = false;

/*      private static class Choice extends JPanel {
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
    }  */   

    @SuppressWarnings("serial")
    public static class DebugPanel extends JPanel {
        public final JCheckBox [] boxes;
        DebugPanel() {
            JCheckBox [] boxes = { 
                new JCheckBox("trace engine/GUI protocol messages"), // 0
                new JCheckBox("trace menu activity"),                // 1
                new JCheckBox("trace System.err loopback"),          // 2
                new JCheckBox("trace print dialog actions"),         // 3
                new JCheckBox("trace container layout actions"),     // 4
                new JCheckBox("trace painting"),                     // 5
                new JCheckBox("trace font measuring"),               // 6
                new JCheckBox("trace drag-and-drop")                 // 7
            };
            
            boxes[0].setSelected(protocol_tracing);
            boxes[1].setSelected(menuaction_tracing);
            boxes[2].setSelected(loopback_tracing);
            boxes[3].setSelected(printdialog_tracing);
            boxes[4].setSelected(containerlayout_tracing);
            boxes[5].setSelected(paint_tracing);
            boxes[6].setSelected(measure_tracing);
            boxes[7].setSelected(drag_tracing);
            
            this.setLayout(new BoxLayout(this,BoxLayout.PAGE_AXIS));
            add(Box.createVerticalStrut(2));
            for (int i=0; i<boxes.length; i++)
                    add(boxes[i]);
            add(Box.createVerticalStrut(2));
            
            this.boxes = boxes;
        }
    }
    public static void runDebugSettingsDialog() {
        DebugPanel panel = new DebugPanel();
        
	int reply = JOptionPane.showConfirmDialog(JapeWindow.getTopWindow(), panel, "Debug settings", 
	                                            JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE);

	if (reply==JOptionPane.OK_OPTION) 
	    actUpon(panel);
    }
    
    public static void actUpon(DebugPanel panel) {

        protocol_tracing        = panel.boxes[0].isSelected();
        menuaction_tracing      = panel.boxes[1].isSelected();
        loopback_tracing        = panel.boxes[2].isSelected();
        printdialog_tracing     = panel.boxes[3].isSelected();
        containerlayout_tracing = panel.boxes[4].isSelected();
        paint_tracing           = panel.boxes[5].isSelected();
        measure_tracing         = panel.boxes[6].isSelected();
        drag_tracing            = panel.boxes[7].isSelected();
    
    }
}

