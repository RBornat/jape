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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager;

import javax.swing.JButton;

// a sort of FlowLayout panel, but one which works.

public class ButtonPane extends Container implements DebugConstants {

    public ButtonPane() {
	this(0);
    }

    public ButtonPane(int minacross) {
	super();
	setLayout(new ButtonPaneLayout(minacross));
    }

    public void addButton(JButton button) {
	if (Jape.onMacOSX)
	    button.putClientProperty("JButton.buttonType", "toolbar");
	add(button);
    }

    public void removeButton(JButton button) {
	remove(button);
    }

    public Dimension doLayout(int width) {
	setSize(width, getHeight());
	getLayout().layoutContainer(this);
	return getSize();
    }
    
    private class ButtonPaneLayout implements LayoutManager {
	private final int minacross;

	public ButtonPaneLayout(int minacross) {
	    this.minacross = minacross;
	}

	/* Called by the Container add methods. Layout managers that don't associate
	 * strings with their components generally do nothing in this method.
	 */
	public void addLayoutComponent(String s, Component c) { }

	/* Adds the specified component to the layout, using the specified constraint object. */
	public void addLayoutComponent(Component comp, Object constraints) { }

	/* Invalidates the layout, indicating that if the layout manager has cached information
	 * it should be discarded.
	 */
	public void invalidateLayout(Container pane) { } // we don't cache

	private int leading(Dimension d) { return d.height/5; } // empirical

	/* Called by the Container remove and removeAll methods. Many layout managers
	 * do nothing in this method, relying instead on querying the container for its
	 * components, using the Container getComponents method.
	 */
	public void removeLayoutComponent(Component c) { }

	/* Called by the Container getPreferredSize method, which is itself called under
	 * a variety of circumstances. This method should calculate and return the ideal
	 * size of the container, assuming that the components it contains will be at or
	 * above their preferred sizes. This method must take into account the container's
	 * internal borders, which are returned by the getInsets method.
	 */

	public Dimension preferredLayoutSize(Container pane) {
	    int buttonpanelheight=0, buttonpanelwidth=0;
	    
	    if (getComponentCount()==0) {
		if (buttonlayout_tracing)
		    Logger.log.println("preferredButtonPanelSize no buttons");
	    }
	    else {
		if (buttonlayout_tracing)
		    Logger.log.println("preferredButtonPanelSize");
		Dimension d = getComponent(0).getPreferredSize();
		int leading = leading(d);
		int buttonheight = d.height;
		int packedwidth = leading;
		int buttoncount = getComponentCount();

		buttonpanelheight = d.height+2*leading;
		buttonpanelwidth = packedwidth;
		
		for (int i=0; i<buttoncount; i++) {
		    Component button = getComponent(i);
		    d = button.getPreferredSize();

		    if (buttonlayout_tracing)
			Logger.log.println(i+": "+d.width+","+d.height);

		    packedwidth += leading+d.width;
		    buttonpanelwidth = Math.max(packedwidth+leading, buttonpanelwidth);

		    if (minacross!=0 && i+1<buttoncount && (i+1)%minacross==0) {
			packedwidth = leading;
			buttonpanelheight += d.height+leading;
		    }
		}
	    }
	    
	    if (buttonlayout_tracing)
		Logger.log.println("preferredButtonPanelSize = "+
				   buttonpanelwidth+","+buttonpanelheight);

	    return new Dimension(buttonpanelwidth, buttonpanelheight);
	}


	/* Called by the Container getMinimumSize method, which is itself called under
	* a variety of circumstances. This method should calculate and return the minimum
	* size of the container, assuming that the components it contains will be at or
	* above their minimum sizes. This method must take into account the container's
	* internal borders, which are returned by the getInsets method.
	*/

	public Dimension minimumLayoutSize(Container pane) {
	    return preferredLayoutSize(pane);
	}

	/* Called when the container is first displayed, and each time its size changes.
	 * A layout manager's layoutContainer method doesn't actually draw components.
	 * It simply invokes each component's resize, move, and reshape methods to set
	 * the component's size and position. This method must take into account the
	 * container's internal borders, which are returned by the getInsets method.
	 * You can't assume that the preferredLayoutSize or minimumLayoutSize method
	 * will be called before layoutContainer is called.
	 */
	public void layoutContainer(Container pane) {
	    int buttoncount = pane.getComponentCount(),
		width = pane.getWidth();

	    if (buttoncount!=0) {
		Dimension d = pane.getComponent(0).getPreferredSize();
		int leading = leading(d), bh = d.height;
		int y = leading, packedwidth = leading;
		for (int i=0; i<buttoncount; i++) {
		    Component button = pane.getComponent(i);
		    button.setSize(button.getPreferredSize());
		    int bw = button.getWidth();
		    if (packedwidth==leading || packedwidth+leading+bw+leading<=width) {
			for (int j=0; j<i; j++) {
			    Component buttonj = pane.getComponent(j);
			    if (buttonj.getY()==y)
				buttonj.setLocation(buttonj.getX()-bw-leading,y);
			}
		    }
		    else {
			centrerow(pane, i, y, packedwidth+leading, width);
			y += bh+leading;
			packedwidth = leading;
		    }
		    button.setLocation(width-bw-leading, y);
		    if (packedwidth==0)
			packedwidth = bw+leading;
		    else
			packedwidth += bw+leading;
		}
		centrerow(pane, buttoncount, y, packedwidth+leading, width);
		pane.setSize(width, y+bh+leading);
	    }
	    else
		pane.setSize(width, 0);
	    
	    if (buttonlayout_tracing) {
		Logger.log.print("ButtonPane.layoutContainer: ");
		JapeUtils.showContainer(pane);
	    }
	}
    }

    private void centrerow(Container pane, int lim, int y, int packedwidth, int width) {
	// centre the buttons for those who like that sort of thing
	// actually it looks quite nice: think I'll keep it.
	int shift = (width-packedwidth)/2;
	for (int i=0; i<lim; i++) {
	    Component c = pane.getComponent(i);
	    if (c.getY()==y)
		c.setLocation(c.getX()-shift, y);
	}
    }
}