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
import java.awt.Dimension;
import java.util.Enumeration;
import java.awt.Graphics;
import java.util.Hashtable;
import javax.swing.JPanel;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseEvent;
import java.awt.Point;
import java.awt.Rectangle;
import javax.swing.Scrollable;

/*
        A basic Jape proof canvas.
*/

public class ProofCanvas extends JPanel implements Scrollable {

    public Dimension getPreferredScrollableViewportSize() {
        return getPreferredSize();
    }

    public int getScrollableUnitIncrement(Rectangle visibleRect,
                                          int orientation,
                                          int direction) {
        return 10; // for now
    }

    public int getScrollableBlockIncrement(Rectangle visibleRect,
                                           int orientation,
                                           int direction) {
        return 100; // for now
    }

    public boolean getScrollableTracksViewportWidth() {
        return false;
    }

    public boolean getScrollableTracksViewportHeight() {
        return false;
    }
    
    /*
        Assignment of buttons to functions.
     */
    static final int HitButton        = 1;
    static final int TextSelectButton = 2;

    public ProofCanvas() { 
    addMouseListener( 
        new MouseAdapter() { 
            public void mousePressed(MouseEvent e) { locate(e); Press(); }
            public void mouseReleased(MouseEvent e) { locate(e); Release(); }
        });
    addMouseMotionListener( 
        new MouseMotionAdapter() {
            public void mouseDragged(java.awt.event.MouseEvent e) { locate(e); Drag(); }
            public void mouseMoved(java.awt.event.MouseEvent e) { locate(e); Move(); }
        });              
    } // ProofCanvas()

//*************** Event-handling mediation between AWT and Items
    
    Point      lastPos;        // location of the last event
    MouseEvent lastEvent;      // the last event itself
    int        lastButton;     // the last (virtual) button (in 1, 2, 3)
    CanvasItem lastItem,       // item (if any) on which the last event happened
               focussedItem;   // item with the focus (if any)

    /** Find (if any) the item containing the given event and
        switch (if necessary) the focus from the focussed item
        to the containing item.
    */
    protected void locate(MouseEvent e) { 
        Point p    = e.getPoint();
        lastPos    = p;
        lastEvent  = e;

        /*
            This next spasm copes with the fact that Macs have 1-button
            mice, and that (in order to cope with this) Java AWT
            reports button 2 and button 3 presses AS IF they were
            presses with one of Meta or Alt pressed. I simply don't know
            whether the getButton field of an event on the Mac always
            says 1, or whether the lowlevel AWTery reports virtual
            buttons. Until I find out, I'm assuming the former, and using
            the Alt and Meta fields to give an indication of the button
            that was actually pressed (on nonMacs) or the (virtual) button
            that a MacIsta indicated that she wanted to press.
            
            Beyond here we're simply pretending we have a 3-button mouse.
        */
        
        // Assign the right virtual button for all but a move
        lastButton = e.isAltDown()  ? 2 :
                     e.isMetaDown() ? 3 :
                                      1 ;
        
        /*
            The item in whose bbox the mouse pointer is sitting 
            has the focus. All normal events are reported to the focussed item.
            Enter/Leave events are generated when the focus
            switches between items.
    
            This ``sharp focus'' model might be a little
            un-nerving for folks doing character selection. It
            can easily be changed so that items can CLAIM
            the focus, but first I want to see how it plays
            in Peoria.                
        */
        
        if (focussedItem != null && focussedItem.contains(p))
            lastItem = focussedItem;
        else {
            lastItem=null;
            for (Enumeration is = items.elements(); is.hasMoreElements(); ) { 
                CanvasItem i = (CanvasItem)is.nextElement();
                if (i.contains(p)) {
                    lastItem = i; break;
                }
            }
        }
        
        // generate Enter/Leave
        if (lastItem!=focussedItem) {  
            if (focussedItem!=null)
                focussedItem.Leave(lastPos, lastButton);
            focussedItem = lastItem;
            if (focussedItem!=null)
                focussedItem.Enter(lastPos, lastButton);
        }
    }

    protected void Press() { 
        if (Debugging.canvas_events) Report("press");
        if (focussedItem!=null) 
            focussedItem.Press(lastPos, lastButton);
    }
    
    protected void Release() {
        if (lastEvent.getClickCount()>1) Hit();
        else {  
            if (Debugging.canvas_events) Report("release");
            if (focussedItem!=null) 
                focussedItem.Release(lastPos, lastButton);
            else {  
                // Usually means clear selections.....
            }
        }
    }
    
    protected void Drag() {
        if (Debugging.canvas_events) Report("drag");
        if (focussedItem!=null) 
           focussedItem.Drag(lastPos, lastButton);
        else {  
            // in some interfaces this means drag the proof around on the canvas
        }
    }

    /** Report a move -- a no-op, pro-tem. */
    protected void Move() {  
        lastButton = 0; // Correct the virtual button
        if (Debugging.canvas_events) Report("move");
    }
    
    protected void Hit() { 
        if (Debugging.canvas_events) Report("hit");
        if (focussedItem!=null) 
           focussedItem.Hit(lastPos, lastButton);
    }

    protected void Report(String event) {  
        System.err.println(lastEvent);
        System.err.println(event+" "+lastButton +" @"+lastPos+" on "+lastItem+" with focus "+focussedItem);
    }

    /**
    <code>
    items represents 
            mapping: Position -> CanvasItem
    where
            forall pos in dom(mapping) 
                mapping(pos).position = pos
    </code>
    */

    protected Hashtable items = new Hashtable();

    public void add(CanvasItem c) {
        super.add(c);
        items.put(c.position, c);
    }
    
    //*************** Painting Interface with the AWT
    
    
    /** Repaint the canvas. */
    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        System.err.println("paintComponent in proofCanvas "+g+",clipBounds="+g.getClipBounds()+",bounds="+getBounds());
        for (Enumeration is = items.elements(); is.hasMoreElements(); ) { 
            CanvasItem i = (CanvasItem) is.nextElement();
            System.err.println("looking at "+i);
            if (i.intersects(g)) {
                System.err.println("painting "+i);
                i.paint(g);
            } 
        }
        if (Debugging.canvas_bbox) {  
            Rectangle bbox = computeBounds();
            g.setColor(Color.yellow);
            g.drawRect(bbox.x, bbox.y, bbox.width-1, bbox.height-1);
        }
    }

    /** 
        Invite the AWT to ask us to paint ourselves.
        
        AWT is based on model-view. Calling repaint tells AWT that the
        model within a certain bounding box has changed. AWT accumulates
        such notifications and eventually (or immediately, depending on
        how busy things are) gives the model a bit of real-estate and
        tells it to paint a view on it.
    */
    public void repaint() {  
        Rectangle bbox = computeBounds();
        super.repaint(bbox.x, bbox.y, bbox.width, bbox.height);
    }

    /** Compute the bounding box of the proof that's being shown. */
    public Rectangle computeBounds() { 
        Rectangle r = null;
        if (items!=null) // this can be called during initialisation
            for (Enumeration is = items.elements(); is.hasMoreElements(); ) { 
                CanvasItem i = (CanvasItem) is.nextElement();
                Rectangle bbox = new Rectangle(i.position.x, i.position.y, i.bounds.width, i.bounds.height);
                System.err.println("adding item "+i+" = "+bbox);
                if (r==null) r=bbox; else r.add(bbox);
            }
        System.err.println("ProofCanvas.computeBounds: "+items+"; "+r);
        return r==null ? new Rectangle() : r;
    }

    //*************** Interface with both clients and components
    
    public TextDimension stringSize(String s, int fontnum) { 
        TextDimension d = fonts[fontnum].stringSize(s);
        d.height  += 2*textInset.height;
        d.descent += textInset.height;
        d.width   += 2*textInset.width;
        return d;
    }

    //*************** Interface with components

    /** Right edge co-ordinates of the characters in the string. */
    public int[] getBoundaries(String text, int fontnum) { 
        int[] boundaries = fonts[fontnum].getBoundaries(text);
        for (int i=0; i<boundaries.length; i++) boundaries[i]+=textInset.width;
        return boundaries;
    }
    
    // Eventually these colours will be set as parameters.
    private Color darkergrey = Color.lightGray.darker();

    public Color getGreyedColour() { 
        return darkergrey; 
    }
    
    public Color getSelectedColour() { 
        return Color.red; 
    }
    
    public Color getTextSelectedBackgroundColour() { 
        return Color.yellow; 
    }
    
    public Color getNormalColour() { 
        return Color.black; 
    }

    // Eventually these fonts will be set as parameters.
    public Fontoid  menuFont     = Fontoid.decode("sanserif-plain-18"), 
                    commentFont  = Fontoid.decode("sanserif-plain-18"), 
                    proofFont    = Fontoid.decode("sanserif-plain-18");

    // this array is invariantly a dispatcher on the 3 canonical fonts
    public Fontoid[] fonts = new Fontoid[]{menuFont, commentFont, proofFont}; 

    // Size of the frame around text items. */
    public Dimension textInset = new Dimension(3, 3);

    public String toString() {
        return "ProofCanvas ["+super.toString()+";lastPos="+lastPos+",lastEvent="+lastEvent+",lastButton="+lastButton+",lastItem="+lastItem+",focussedItem="+focussedItem+",items="+items+",menuFont="+menuFont+",commentFont="+commentFont+"proofFont="+proofFont+",fonts="+fonts+",textInset="+textInset+"]";
    }
    
}
