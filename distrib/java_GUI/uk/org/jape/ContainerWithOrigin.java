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
import java.awt.Point;
import java.awt.Rectangle;
import javax.swing.Scrollable;

/* A container to which you can add components at arbitrary positions */

public class ContainerWithOrigin extends Container {

    public ContainerWithOrigin() {
        super(); super.add(child);
        setLayout(new ContainerWithOriginLayout());
    }

    protected final Child child = new Child();

    public Component add(Component c) {
        child.add(c);
        computeBounds();
        return c;
    }

    public void computeBounds() {
        child.getLayout().layoutContainer(child);
        getLayout().layoutContainer(this);
    }

    public void paint(Graphics g) {
        int x=child.getX(), y=child.getY();
        g.translate(x, y);
        child.paint(g);
        g.translate(-x,-y);
    }

    public void repaintFromChild(long tm, int x, int y, int width, int height) {
        super.repaint(tm, x+child.getX(), y+child.getY(), width, height);
    }

    private boolean inViewport = false;
    private int viewWidth, viewHeight;

    // this allows us to be in a viewport, and to extend across the entire viewport,
    // so that mouse events are received by us.
    // To activate this mechanism, make a descendant implement Clickable
    protected void declareViewportSize(int width, int height) {
        inViewport = true;
        viewWidth = width; viewHeight = height;
        getLayout().layoutContainer(this);
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
            if (Debugging.containerlayout)
                System.err.print("[layoutContainer");
            // child.getLayout().layoutContainer(child);
            Rectangle vr = child.getVisualBounds();
            Point pos = getLocation(), childpos = child.getLocation();
            Dimension size = getSize();
            if (Debugging.containerlayout)
                System.err.print(" childpos="+childpos+
                                 ", vr="+vr+", pos="+pos+", size="+size+
                                 ", insets="+getInsets()+";");
            // we don't have any insets, we are an invisible container.
            // new top is at cx+vcx; new left at cy+vcy.
            // we must adjust child location when we adjust our own
            int deltax = childpos.x+vr.x, deltay=childpos.y+vr.y;
            if (deltax!=0 || deltay!=0) {
                pos.x+=deltax; pos.y+=deltay; setLocation(pos);
                setLocation(getX()+deltax, getY()+deltay);
                childpos.x = -vr.x; childpos.y = -vr.y;
                child.setLocation(childpos);
                if (Debugging.containerlayout)
                    System.err.print(" childpos:="+childpos+", pos:="+pos);
            }
            if (vr.width!=size.width || vr.height!=size.height) {
                size.width=vr.width; size.height=vr.height;
                setSize(size);
                if (Debugging.containerlayout)
                    System.err.print(" size:="+size);
            }
            if (inViewport) {
                int diff;
                if ((diff=pos.x)>0) {
                    pos.x=0; size.width+=diff; childpos.x+=diff;
                }
                if ((diff=pos.y)>0) {
                    pos.y=0; size.height+=diff; childpos.y+=diff;
                }
                if ((diff=viewWidth-(pos.x+size.width))>0)
                    size.width+=diff;
                if ((diff=viewHeight-(pos.y+size.height))>0)
                    size.height+=diff;
                setLocation(pos); child.setLocation(childpos); setSize(size);
                if (Debugging.containerlayout)
                    System.err.print("; inViewport so childpos:="+childpos+
                                     ", pos:="+pos+", size:="+size);
            }
            if (Debugging.containerlayout)
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
                if (Debugging.containerlayout)
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
                if (newBounds==null)
                    newBounds = new Rectangle();
                Dimension size = getSize();
                if (newBounds.width!=size.width || newBounds.height!=size.height) {
                    size.width=newBounds.width; size.height=newBounds.height;
                    setSize(size);
                    if (Debugging.containerlayout)
                        System.err.print("; childsize:="+size);
                }
                if (!newBounds.equals(visualBounds)) {
                    visualBounds=newBounds;
                    if (Debugging.containerlayout)
                        System.err.print("; visualBounds:="+visualBounds);
                }
                if (Debugging.containerlayout)
                    System.err.println(")");
            }
        }
    }
}
