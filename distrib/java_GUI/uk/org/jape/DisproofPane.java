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

import javax.swing.border.Border;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import javax.swing.JLabel;
import java.awt.LayoutManager;

// BoxLayout doesn't work for laying out this thing,
// if you use BorderLayout to give lots of space to the worldPane.
// So I do it by steam (again; sigh!)

public class DisproofPane extends Container {
    AnchoredScrollPane worldPane;
    DisproofCanvas seqCanvas;
    WorldCanvas worldCanvas;
    Container seqView;
    Box tileCanvas;

    public DisproofPane() {
        super();
        setLayout(new DisproofPaneLayout());
        setBackground(Color.white);
        worldPane = new AnchoredScrollPane();
        add(worldPane);
        worldCanvas = new WorldCanvas(worldPane.getViewport(), true);
        worldPane.add(worldCanvas);
        worldPane.setAnchor(AnchoredScrollPane.ANCHOR_SOUTH);
        tileCanvas = new Box(BoxLayout.Y_AXIS);
        add(tileCanvas);
        seqView = new Container() {
            public void validate() { }
            public Dimension getPreferredSize() {
                return getSize();
            }
            public Dimension getMinimumSize() {
                return getSize();
            }
        };
        add(seqView);
        seqView.setLayout(null);
        seqCanvas = new DisproofCanvas(seqView, false) {
            public float getAlignmentX() { return CENTER_ALIGNMENT; }
        };
        seqView.add(seqCanvas);
    }

    public void setSequentBox(int width, int ascent, int descent) {
        seqCanvas.setSequentBox(width, ascent, descent);
        seqView.setSize(width, ascent+descent+2*linethickness);
        getLayout().layoutContainer(this);
    }

    private int linethickness = 1;
    
    public void setlinethickness(int linethickness) {
        linethickness = seqCanvas.linethickness = worldCanvas.linethickness = linethickness;
    }
    
    public void setTiles(String[] tiles) {
        int spacing = LocalSettings.TileSpacing;
        Border padding = BorderFactory.createEmptyBorder(spacing/2,spacing,spacing/2,spacing),
               raisedbevel = BorderFactory.createRaisedBevelBorder(),
               loweredbevel = BorderFactory.createLoweredBevelBorder(),
               compoundbevel = BorderFactory.createCompoundBorder(raisedbevel, loweredbevel),
               tileborder = BorderFactory.createCompoundBorder(compoundbevel, padding);
        
        tileCanvas.removeAll();
        tileCanvas.add(Box.createGlue());
        for (int i=0; i<tiles.length; i++) {
            JLabel tile = new JLabel(tiles[i]);
            tile.setBorder(tileborder);
            tileCanvas.add(tile);
            if (i+1<tiles.length)
                tileCanvas.add(Box.createVerticalStrut(spacing));
        }
        tileLayoutPending = true;
    }

    private boolean tileLayoutPending = false;
    
    protected void doTileLayout() {
        if (tileLayoutPending) {
            tileLayoutPending = false;
            BoxLayout layout = (BoxLayout)tileCanvas.getLayout();
            layout.invalidateLayout(tileCanvas);
            layout.layoutContainer(tileCanvas);
        }
    }

    public void paint(Graphics g) {
        g.setColor(getBackground());
        g.fillRect(0, 0, getWidth(), getHeight());
        super.paint(g);
    }
    
    protected class DisproofPaneLayout implements LayoutManager {

        /* Called by the Container add methods. Layout managers that don't associate
        * strings with their components generally do nothing in this method.
        */
        public void addLayoutComponent(String s, Component c) { }

        /* Returns the alignment along the x axis. This specifies how the component would like
        * to be aligned relative to other components. The value should be a number between 0
        * and 1 where 0 represents alignment along the origin, 1 is aligned the furthest away
        * from the origin, 0.5 is centered, etc.
        */
        public float getLayoutAlignmentX(Container pane) { return (float)0; } // why not?

        /* Returns the alignment along the y axis. See above */
        public float getLayoutAlignmentY(Container pane) { return (float)0; } // why not?

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

        private int gap() { return 5*linethickness; }
        
        public Dimension preferredLayoutSize(Container pane) {
            Dimension seqSize = seqView.getPreferredSize();
            doTileLayout();
            Dimension tileSize = tileCanvas.getPreferredSize();
            Dimension worldSize = worldPane.getPreferredSize();

            return new Dimension(Math.max(seqSize.width,tileSize.width+worldSize.width)+2*gap(),
                                 seqSize.height+2*gap()+Math.max(tileSize.height+worldPane.scrollbarthickness,
                                                                 worldSize.height));
        }

        /* Called by the Container getMinimumSize method, which is itself called under
         * a variety of circumstances. This method should calculate and return the minimum
         * size of the container, assuming that the components it contains will be at or
         * above their minimum sizes. This method must take into account the container's
          * internal borders, which are returned by the getInsets method.
        */

        public Dimension minimumLayoutSize(Container pane) {
            Dimension d = seqView.getSize();
            d.height+=2*gap()+worldPane.scrollbarthickness+6*worldCanvas.worldRadius();
            /* bizarre as it seems, 6* is a good choice ... */
            return d;
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
            Dimension paneSize = pane.getSize();
            Dimension seqSize = seqView.getSize();
            doTileLayout();
            Dimension tileSize = tileCanvas.getPreferredSize();

            int bottom = paneSize.height-gap(),
                right = paneSize.width-gap(),
                seqtop = bottom-seqSize.height,
                worldbottom = seqtop-gap(),
                tileleft = right-tileSize.width;
            
            seqView.setLocation((paneSize.width-seqSize.width)/2, seqtop);
            tileCanvas.setBounds
                (tileleft, worldbottom-worldPane.scrollbarthickness-tileSize.height,
                 tileSize.width, tileSize.height);
            worldPane.setBounds(0, 0, tileleft-gap(), worldbottom);

            pane.repaint();
        }
    }
}
