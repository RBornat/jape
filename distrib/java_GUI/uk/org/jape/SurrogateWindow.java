/* 
    $Id$

    Copyright © 2003-5 Richard Bornat & Bernard Sufrin
     
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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Font;
import java.awt.Graphics;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.SwingConstants;

public abstract class SurrogateWindow extends JapeWindow {
    static final String message = "Jape!";
    // private Font font = new Font("serif", Font.ITALIC+Font.BOLD, 36);
    private ImageIcon logoIcon = new ImageIcon(Images.getImage("japelogo.gif"));

    public SurrogateWindow(String title) {
	super(title);
	int hextra = 60, vextra = 60;
	Container pane = getContentPane();
	pane.setBackground(Color.white);
	JPanel panel = new JPanel();
	pane.add(panel,BorderLayout.CENTER);
	panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
	panel.setBackground(Color.white);
	panel.add(Box.createVerticalGlue());
	JLabel logo = new JLabel("© Richard Bornat and Bernard Sufrin 1991-2005", logoIcon, JLabel.CENTER);
	logo.setVerticalTextPosition(SwingConstants.BOTTOM);
	logo.setHorizontalTextPosition(SwingConstants.CENTER);
        logo.setAlignmentX(Component.CENTER_ALIGNMENT);
	panel.add(logo);
	panel.add(Box.createVerticalGlue());
	String version = AboutBox.getVersion();
	if (version!=null) {
	    vextra += 20;
	    JLabel vlabel = new JLabel("Version "+version, JLabel.CENTER);
	    vlabel.setAlignmentX(Component.CENTER_ALIGNMENT);
	    panel.add(vlabel);
	    panel.add(Box.createVerticalGlue());
	}
	JLabel link = new JLabel("Freeware under GPL licence: see www.jape.org.uk", JLabel.CENTER);
        link.setAlignmentX(Component.CENTER_ALIGNMENT);
	panel.add(link); // It ought to be in the middle ...
	panel.add(Box.createVerticalGlue());
	pack();
	setSize(getWidth()+hextra, getHeight()+vextra);
	setBar(); // by experiment, seems to be necessary before setVisible
		  // no setVisible here ...

	setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
	addWindowListener(new WindowAdapter() {
	    public void windowClosing(WindowEvent e) {
		SurrogateWindow.this.windowCloser();
	    }
	});
    }

    public int getBarKind() {
	return JapeMenu.OTHERWINDOW_BAR;
    }
    
    protected boolean servesAsControl() { return false; }

    protected abstract void windowCloser();
}

