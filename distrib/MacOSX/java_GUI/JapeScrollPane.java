/*
    $Id$
    
    Copyright © 2002 Richard Bornat & Bernard Sufrin
    
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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import java.awt.LayoutManager;
import javax.swing.ScrollPaneConstants;

public class JapeScrollPane extends JComponent implements ScrollPaneConstants {
    private Component view;
    private JPanel viewport;
    private JScrollBar vsb,
                       hsb;

    public JapeScrollPane() {
        super();
        hsb = new JScrollBar(JScrollBar.HORIZONTAL);
        vsb = new JScrollBar(JScrollBar.VERTICAL);
        viewport = new JPanel();
        view = null; // unnecessary, but it makes me feel better
        setLayout(new JapeScrollPaneLayout());
    }
    
    public Component add(Component c) {
        if (view!=null)
            viewport.remove(view);
        view=c;
        viewport.add(c);
        return c;
    }

    private class JapeScrollPaneLayout implements LayoutManager {

        /* Called by the Container add methods. Layout managers that don't associate
        * strings with their components generally do nothing in this method.
        */
        public void addLayoutComponent(String s, Component c) { }

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
        public Dimension preferredLayoutSize(Container c) {
            layoutContainer(c);
            return getSize();
        }

        /* Called by the Container getMinimumSize method, which is itself called under
        * a variety of circumstances. This method should calculate and return the minimum
        * size of the container, assuming that the components it contains will be at or
        * above their minimum sizes. This method must take into account the container's
        * internal borders, which are returned by the getInsets method.
        */
        public Dimension minimumLayoutSize(Container c) { return preferredLayoutSize(c); }

        /* Called when the container is first displayed, and each time its size changes.
        * A layout manager's layoutContainer method doesn't actually draw components.
        * It simply invokes each component's resize, move, and reshape methods to set
        * the component's size and position. This method must take into account the
        * container's internal borders, which are returned by the getInsets method.
        * You can't assume that the preferredLayoutSize or minimumLayoutSize method
        * will be called before layoutContainer is called.
        */
        public void layoutContainer(Container c) {
            Dimension size = getSize();
            int vsbWidth = vsb.getPreferredSize().width;
            int hsbHeight = hsb.getPreferredSize().height;
            int sparewidth = size.width-vsbWidth;
            int spareheight = size.height-hsbHeight;

            vsb.setBounds(sparewidth, 0, vsbWidth, spareheight);
            hsb.setBounds(0, spareheight, sparewidth, hsbHeight);
            viewport.setBounds(0, 0, sparewidth, spareheight);
        }
    }
}
