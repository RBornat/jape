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
import java.awt.Graphics;
import java.awt.Insets;
import javax.swing.JComponent;
import javax.swing.JPanel;
import java.awt.LayoutManager;
import java.awt.Rectangle;
import javax.swing.Scrollable;

/* A panel to which you can add components at arbitrary positions */

public class KPanel extends JPanel implements Scrollable {

    KPanel() { super(); super.add(child); setLayout(new KPanelLayout()); }

    protected final Child child = new Child();

    public Component add(Component c) {
        child.add(c);
        child.revalidate();
        return c;
    }

    // this anchor stuff doesn't work yet, but one day it will ...
    public final int ANCHOR_NORTH = 0;
    public final int ANCHOR_NORTHEAST = 1;
    public final int ANCHOR_EAST = 2;
    public final int ANCHOR_SOUTHEAST = 3;
    public final int ANCHOR_SOUTH = 4;
    public final int ANCHOR_SOUTHWEST = 5;
    public final int ANCHOR_WEST = 6;
    public final int ANCHOR_NORTHWEST = 7;

    // call this on a KPanel that already has a size, please.
    public void setChildAnchor(int anchor) {
        int x, y;
        switch (anchor) {
            case ANCHOR_NORTH:     x=getWidth()/2; y=0;             break;
            case ANCHOR_NORTHEAST: x=getWidth();   y=0;             break;
            case ANCHOR_EAST:      x=getWidth();   y=getHeight()/2; break;
            case ANCHOR_SOUTHEAST: x=getWidth();   y=getHeight();   break;
            case ANCHOR_SOUTH:     x=getWidth()/2; y=getHeight();   break;
            case ANCHOR_SOUTHWEST: x=0;            y=getHeight();   break;
            case ANCHOR_WEST:      x=0;            y=getHeight()/2; break;
            case ANCHOR_NORTHWEST: x=0;            y=0;             break;
            default: System.err.println("setChildAnchor "+anchor); return;
        }
        child.setLocation(x,y);
        revalidate();
    }

    public void computeBounds() {
        getLayout().layoutContainer(this);
    }

    public void paintChildren(Graphics g) {
        int x=child.getX(), y=child.getY();
        g.translate(x, y);
        child.paint(g);
        g.translate(-x,-y);
    }

    public void repaintInChild(long tm, int x, int y, int width, int height) {
        super.repaint(tm, x+child.getX(), y+child.getY(), width, height);
    }

    protected class KPanelLayout implements LayoutManager {

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
            child.getLayout().layoutContainer(child);
            Rectangle vr = child.getVisualBounds();
            Insets i = c.getInsets();
            vr.x -= i.left; vr.width += i.left+i.right;
            vr.y -= i.top; vr.height += i.top+i.bottom;
            // new top is at cx+vcx; new left at cy+vcy.
            // we must adjust child location when we adjust our own
            int deltax = child.getX()+vr.x, deltay=child.getY()+vr.y;
            if (deltax!=0 || deltay!=0) {
                setLocation(getX()+deltax, getY()+deltay);
                child.setLocation(-vr.x, -vr.y);
            }
            Dimension size = getSize();
            if (vr.width!=size.width || vr.height!=size.height) {
                size.width=vr.width; size.height=vr.height;
                setSize(size); setPreferredSize(size); setMinimumSize(size);
            }
        }
    }

    // implementation of Scrollable interface
    
    // Returns the preferred size of the viewport for a view component.
    public Dimension getPreferredScrollableViewportSize() { return getPreferredSize(); }

    // Components that display logical rows or columns should compute the
    // scroll increment that will completely expose one new row or column,
    // depending on the value of orientation.
    public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction) {
        return 10; // for now
    }

    // Components that display logical rows or columns should compute the scroll
    // increment that will completely expose one block of rows or columns, depending
    // on the value of orientation.
    public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction) {
        return 100; // for now
    }

    // Return true if a viewport should always force the height of this
    // Scrollable to match the height of the viewport.
    public boolean getScrollableTracksViewportHeight() {
        return false; // has to be false, or else scrollbars don't work (!!)
    }

    // Return true if a viewport should always force the width of this
    // Scrollable to match the width of the viewport.
    public boolean getScrollableTracksViewportWidth() {
        return false; // has to be false, or else it don't work (!!)
    }

    protected class Child extends JComponent {
        Child() { super(); setLayout(new ChildLayout()); }

        protected Rectangle visualBounds = new Rectangle(0,0,0,0);

        public Rectangle getVisualBounds() {
            return new Rectangle(visualBounds.x, visualBounds.y, visualBounds.width, visualBounds.height);
        }

        public boolean contains(int x, int y) {
            return visualBounds.contains(x,y);
        }

        // Because I know the way the Component repaint hierarchy works, I can intervene ...
        public void repaint(long tm, int x, int y, int width, int height) {
            repaintInChild(tm, x, y, width, height);
        }

        protected class ChildLayout implements LayoutManager {
            /* Called by the Container add methods. Layout managers that don't associate
            * strings with their components generally do nothing in this method.
            */
            public void addLayoutComponent(String s, Component c) { System.err.println("Child addLayoutComponent "+c); }

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
                Rectangle newBounds=null;
                int nc = getComponentCount();
                for (int i=0; i<nc; i++) {
                    if (newBounds==null)
                        newBounds = getComponent(i).getBounds();
                    else
                        newBounds.add(getComponent(i).getBounds());
                }
                if (newBounds==null)
                    newBounds = new Rectangle(0,0,0,0);
                Dimension size = getSize();
                if (newBounds.width!=size.width || newBounds.height!=size.height) {
                    size.width=newBounds.width; size.height=newBounds.height;
                    setSize(size); setPreferredSize(size); setMinimumSize(size);
                }
                if (!newBounds.equals(visualBounds)) {
                    visualBounds=newBounds;
                }
            }
        }
    }
}
