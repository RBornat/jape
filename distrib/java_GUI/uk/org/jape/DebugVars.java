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

import javax.swing.JCheckBox;
import javax.swing.JOptionPane;

public class DebugVars {
    public static final boolean showDebugVars = true;

    public static boolean protocol_tracing = false;

    public static void runDebugSettingsDialog() {
        String [] buttons = { "OK", "Cancel" };
        JCheckBox protocol_tracing = new JCheckBox("trace engine/GUI protocol messages");
        protocol_tracing.setSelected(DebugVars.protocol_tracing);
        int reply = JOptionPane.showOptionDialog(JapeWindow.getTopWindow(), protocol_tracing,
                                                 "Debug settings", 0,
                                                 JOptionPane.PLAIN_MESSAGE,
                                                 null, buttons, buttons[0]);
        if (reply==0)
            DebugVars.protocol_tracing = protocol_tracing.isSelected();
    }

}
