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

/*
   This exists because it seems to be impossible to force a JScrollPane to leave the position of
   its view alone, and on the other hand impossible to anchor the view to a corner or a midpoint
   in the viewport when resizing.

   It's ok, but it would be nice to be able to do resizing more efficiently.
 */

import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.Component;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import javax.swing.JScrollBar;
import java.awt.LayoutManager;
import java.awt.Rectangle;

@SuppressWarnings("serial")
public class AnchoredScrollPane extends Container implements DebugConstants {
    private Component view;
    private Container viewport;
    private JScrollBar vsb, hsb;
    public final int scrollbarthickness;

    public AnchoredScrollPane(final String id) {
        super();
        hsb = new JScrollBar(JScrollBar.HORIZONTAL);
        hsb.setUnitIncrement(10); hsb.setBlockIncrement(100); // for now
        hsb.addAdjustmentListener(new H());
        vsb = new JScrollBar(JScrollBar.VERTICAL);
        vsb.setUnitIncrement(10); vsb.setBlockIncrement(100); // for now
        vsb.addAdjustmentListener(new V());
        addMouseWheelListener
        ( new MouseWheelListener()
          {
            public void mouseWheelMoved(MouseWheelEvent e) 
            { int rot = e.getWheelRotation();
              /* the Java API for MouseWheels only ever yields 1: so doing it the right way, namely:
                   int amt = e.getScrollType()==MouseWheelEvent.WHEEL_UNIT_SCROLL ? e.getScrollAmount() : 1;
                 won't work unless we intercept mouse wheel events somewhere further up the line. 
                 
                 The neurone-conservation principle means that I'm not
                 inclined to do that for the tiny mileage we'll get. (BS, July 2012)
              */              
              if (vsb!=null)
              {   int amt = vsb.getUnitIncrement();
                  vsb.setValue(vsb.getValue()+rot*amt);
              }
            }
          }
        );
        viewport = new Container() {
            public void paint(Graphics g) {
                if (DebugVars.paint_tracing) {
                    Logger.log.println("painting viewport in AnchoredScrollPane "+id);
                    JapeUtils.showContainer(this);
                }
                g.setColor(getBackground());
                g.fillRect(0,0, getWidth(), getHeight());
                super.paint(g);
            }
        };
        viewport.setLayout(null);
        view = null; // unnecessary, but it makes me feel better
        setLayout(new AnchoredScrollPaneLayout());
        super.add(hsb); super.add(vsb); super.add(viewport);
        viewport.setBackground(JapePrefs.ProofBackgroundColour);
        scrollbarthickness = hsb.getPreferredSize().height;
    }

    public Container getViewport() { return viewport; }

    private class H implements AdjustmentListener {
        public void adjustmentValueChanged(AdjustmentEvent e) {
            switch (e.getAdjustmentType()) {
                case AdjustmentEvent.TRACK:
                    if (view!=null) {
                        Point p = view.getLocation();
                        p.x -= hsb.getValue();
                        view.setLocation(p);
                        validate(); repaint();
                    }
                    break;
                default:
                    Logger.log.println("H sees AdjustmentEvent "+e.getAdjustmentType());
                    break;
            }
        }
    }

    private class V implements AdjustmentListener {
        public void adjustmentValueChanged(AdjustmentEvent e) {
            switch (e.getAdjustmentType()) {
                case AdjustmentEvent.TRACK:
                    if (view!=null) {
                        Point p = view.getLocation();
                        p.y -= vsb.getValue();
                        view.setLocation(p);
                        validate(); repaint();
                    }
                    break;
                default:
                    Logger.log.println("V sees AdjustmentEvent "+e.getAdjustmentType());
                    break;
            }
        }
    }

    public static final int ANCHOR_NORTH = 0;
    public static final int ANCHOR_NORTHEAST = 1;
    public static final int ANCHOR_EAST = 2;
    public static final int ANCHOR_SOUTHEAST = 3;
    public static final int ANCHOR_SOUTH = 4;
    public static final int ANCHOR_SOUTHWEST = 5;
    public static final int ANCHOR_WEST = 6;
    public static final int ANCHOR_NORTHWEST = 7;

    private int anchor = ANCHOR_NORTHWEST;

    public Component add(Component c) {
        return add(c, ANCHOR_NORTHWEST);
    }

    public Component add(Component c, int anchor) {
        remove(view);
        view=c;
        viewport.add(c);
        c.addComponentListener(new ComponentAdapter() {
            public void componentMoved(ComponentEvent e) {
                validate();
            }
            public void componentResized(ComponentEvent e) {
                validate();
            }
        });
        setAnchor(anchor);
        return c;
    }

    public void remove(Component c) {
        if (c!=null && c==view) {
            viewport.remove(c);
        }
    }
    
    public void setAnchor(int anchor) {
        /* we don't do anything with the anchor at this point: anchoring is a resizing action */
        switch (anchor) {
            case ANCHOR_NORTH:
            case ANCHOR_NORTHEAST:
            case ANCHOR_EAST:
            case ANCHOR_SOUTHEAST:
            case ANCHOR_SOUTH:
            case ANCHOR_SOUTHWEST:
            case ANCHOR_WEST:
            case ANCHOR_NORTHWEST:
                break;
            default:
                anchor = ANCHOR_NORTHWEST;
        }
        this.anchor = anchor;
    }

    public void setScrollBarValues() {
        if (view==null) {
            hsb.setValues(0,0,0,0);
            vsb.setValues(0,0,0,0);
        }
        else {
            Rectangle act = view.getBounds();
            Rectangle vis = viewport.getBounds();
            if (vis.x<=act.x && act.x+act.width<=vis.x+vis.width || vis.width==0)
                hsb.setValues(vis.x,0,vis.x,vis.x);
            else
                hsb.setValues(vis.x, vis.width,
                              Math.min(vis.x,act.x), Math.max(vis.x+vis.width,act.x+act.width));
            if (vis.y<=act.y && act.y+act.height<=vis.y+vis.height || vis.height==0)
                vsb.setValues(vis.y,0,vis.y,vis.y);
            else
                vsb.setValues(vis.y, vis.height,
                              Math.min(vis.y,act.y), Math.max(vis.y+vis.height,act.y+act.height));
            
        }
    }

    /* this is where the anchor policy bites */
    @SuppressWarnings("unused")
    public void setBounds(int x, int y, int w, int h) {
        if (view!=null) { 
            Point viewPos = view.getLocation();
            int width = Math.max(getWidth()-scrollbarthickness,0), height = Math.max(getHeight()-scrollbarthickness,0),
                w1 = Math.max(w-scrollbarthickness,0), h1=Math.max(h-scrollbarthickness,0);
            if (anchoredpane_tracing)
                Logger.log.print("Anchored move ("+anchor+") from "+viewPos.x+","+viewPos.y+
                                 " in "+width+","+height);
            switch (anchor) {
                case ANCHOR_NORTH:
                    viewPos.x += (w1-width)/2; break;
                case ANCHOR_NORTHEAST:
                    viewPos.x += w1-width; break;
                case ANCHOR_EAST:
                    viewPos.x += w1-width; viewPos.y += (h1-height)/2; break;
                case ANCHOR_SOUTHEAST:
                    viewPos.x += w1-width; viewPos.y += h1-height; break;
                case ANCHOR_SOUTH:
                    viewPos.x += (w1-width)/2; viewPos.y += h1-height; break;
                case ANCHOR_SOUTHWEST:
                    viewPos.y += h1-height; break;
                case ANCHOR_WEST:
                    viewPos.y += (h1-height)/2; break;
                case ANCHOR_NORTHWEST:
                default:
                    break;
            }
            view.setLocation(viewPos);
            if (anchoredpane_tracing)
                Logger.log.println(" to "+viewPos.x+","+viewPos.y+" in "+w1+","+h1);
        }
        super.setBounds(x,y,w,h);
        if (view!=null && anchoredpane_tracing) {
            if (view instanceof ContainerWithOrigin)
                Logger.log.print(" ("+((ContainerWithOrigin)view).getViewGeometry()+")");
            Logger.log.println();
        }
    }
    
    private class AnchoredScrollPaneLayout implements LayoutManager {

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
        private Dimension minSize;
        public Dimension minimumLayoutSize(Container c) {
            if (minSize==null) {
                minSize = vsb.getMinimumSize();
                minSize.width+=minSize.height; minSize.height=minSize.width;
            }
            return new Dimension(Math.max(40,minSize.width), Math.max(40,minSize.height));
        }

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
            int sparewidth = size.width-scrollbarthickness;
            int spareheight = size.height-scrollbarthickness;

            vsb.setBounds(sparewidth, 0, scrollbarthickness, spareheight);
            hsb.setBounds(0, spareheight, sparewidth, scrollbarthickness);
            viewport.setBounds(0, 0, sparewidth, spareheight);
            setScrollBarValues();
        }
    }
}

