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

import javax.swing.JCheckBox;
import javax.swing.JOptionPane;

public class DebugVars {
    public static final boolean showDebugVars = true;

    public static boolean protocol_tracing    = false,
			  menuaction_tracing  = false,
			  loopback_tracing    = false,
			  printdialog_tracing = false,
			  containerlayout_tracing = false;

    public static void runDebugSettingsDialog() {
	String [] buttons = { "OK", "Cancel" };
	JCheckBox [] tracing = {
	    new JCheckBox("trace engine/GUI protocol messages"), // 0
	    new JCheckBox("trace menu activity"),		 // 1
	    new JCheckBox("trace System.err loopback"),		 // 2
	    new JCheckBox("trace print dialog actions"),	 // 3
	    new JCheckBox("trace container layout actions")      // 4
	};
	tracing[0].setSelected(protocol_tracing);
	tracing[1].setSelected(menuaction_tracing);
	tracing[2].setSelected(loopback_tracing);
	tracing[3].setSelected(printdialog_tracing);
	tracing[4].setSelected(containerlayout_tracing);
	int reply = JOptionPane.showOptionDialog(JapeWindow.getTopWindow(), tracing,
						 "Debug settings", 0,
						 JOptionPane.PLAIN_MESSAGE,
						 null, buttons, buttons[0]);
	if (reply==0) {
	    protocol_tracing	 = tracing[0].isSelected();
	    menuaction_tracing	 = tracing[1].isSelected();
	    loopback_tracing	 = tracing[2].isSelected();
	    printdialog_tracing	 = tracing[3].isSelected();
	    containerlayout_tracing	 = tracing[4].isSelected();
	}
    }
}

