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

import java.awt.Color;
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

/*  A Container to which you can add components at arbitrary positions

    Because of the way Container is implemented, especially the fact that
    (the important version of) findComponentAt is final, it's impossible to
    mess with the component hierarchy, so you have to have a child inside
    the container which holds the components you are drawing.  This doesn't
    matter, until you have to scan through the components and such.  So some
    users of this class have to be aware of the container/child distinction.
    Oh dear ...
 */

public class ContainerWithOrigin extends Container {

    public ContainerWithOrigin() {
        super(); super.add(child);
        setLayout(new ContainerWithOriginLayout());
    }

    protected final Child child = new Child();

    protected void computeBounds() {
        child.getLayout().layoutContainer(child);
        getLayout().layoutContainer(this);
    }

    // add, remove, removeAll, paint, repaint all work on the child


    public Component add(Component c) {
        child.add(c);
        computeBounds();
        return c;
    }

    public void remove(Component c) {
        child.remove(c);
        computeBounds();
    }

    public void removeAll() {
        child.removeAll();
        computeBounds();
    }

    public void paint(Graphics g) {
        int x=child.getX(), y=child.getY();
        g.translate(x, y);
        child.paint(g);
        g.translate(-x,-y);
    }

    public void repaint(long tm, int x, int y, int width, int height) {
        super.repaint(tm, x+child.getX(), y+child.getY(), width, height);
    }
    
    public void repaintFromChild(long tm, int x, int y, int width, int height) {
        repaint(tm, x, y, width, height);
    }

    protected class ContainerWithOriginLayout implements LayoutManager {

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
            if (Debugging.ContainerWithOrigin)
                System.err.print("[layoutContainer");
            // child.getLayout().layoutContainer(child);
            // we don't have insets, we are an invisible container
            int vcx = child.visualBounds.x, vcy = child.visualBounds.y,
                vcw = child.visualBounds.width, vch = child.visualBounds.height;
            // new top is at cx+vcx; new left at cy+vcy.
            // we must adjust child location when we adjust our own
            int deltax = child.getX()+vcx, deltay = child.getY()+vcy;
            if (deltax!=0 || deltay!=0) {
                setLocation(getX()+deltax, getY()+deltay);
                child.setLocation(-vcx, -vcy);
                if (Debugging.ContainerWithOrigin)
                    System.err.print("; moved adult to "+getX()+","+getY()+
                                       ", child to "+child.getX()+","+child.getY());
            }
            Dimension size = getSize();
            if (vcw!=getWidth() || vch!=getHeight()) {
                setSize(vcw, vch);
            if (Debugging.ContainerWithOrigin)
                System.err.print("; set adult size to "+getWidth()+","+getHeight());

            }
            if (Debugging.ContainerWithOrigin)
                System.err.println("]");
        }
    }

    protected class Child extends Container {
        Child() { super(); setLayout(new ChildLayout()); }

        protected Rectangle visualBounds = new Rectangle(0,0,0,0);

        public Rectangle getVisualBounds() {
            return new Rectangle(visualBounds.x, visualBounds.y,
                                 visualBounds.width, visualBounds.height);
        }

        public boolean contains(int x, int y) {
            return visualBounds.contains(x,y);
        }

        // Because I know the way the Component repaint hierarchy works, I can intervene ...
        public void repaint(long tm, int x, int y, int width, int height) {
            repaintFromChild(tm, x, y, width, height);
        }

        protected class ChildLayout implements LayoutManager {
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
                if (Debugging.ContainerWithOrigin)
                    System.err.print("(childLayoutContainer");
                Rectangle newBounds=null, tmp=new Rectangle();
                int nc = getComponentCount();
                for (int i=0; i<nc; i++) {
                    if (newBounds==null) {
                        newBounds = new Rectangle();
                        getComponent(i).getBounds(newBounds);
                    }
                    else {
                        getComponent(i).getBounds(tmp);
                        newBounds.add(tmp); 
                    }
                }
                // the difference between the main panel's position and visualBounds is,
                // essentially, the origin!
                if (newBounds==null)
                    newBounds = new Rectangle(visualBounds.x, visualBounds.y, 0, 0);
                if (newBounds.width!=getWidth() || newBounds.height!=getHeight()) {
                    setSize(newBounds.width, newBounds.height);
                    if (Debugging.ContainerWithOrigin)
                        System.err.print("; childsize:="+getSize());
                }
                if (!newBounds.equals(visualBounds)) {
                    visualBounds=newBounds;
                    if (Debugging.ContainerWithOrigin)
                        System.err.print("; visualBounds:="+visualBounds);
                }
                if (Debugging.ContainerWithOrigin)
                    System.err.println(")");
            }
        }
    }
}
