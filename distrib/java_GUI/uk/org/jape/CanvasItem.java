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

import java.awt.*;
import java.io.*;

//*********************************************************************8<

/**     

        Base class for all canvas items.
*/

abstract class CanvasItem {
    public Point         position;       // Used as the key
    public TextDimension bounds;         // Bounding box size: computed lazily
    
    protected ProofCanvas canvas;        // The canvas that this is on        
    
    public CanvasItem(ProofCanvas canvas, Point position) { 
        this.canvas   = canvas; this.position = position;
    }
    
    /** 
            Compute the bounds of the item. 
    */
    protected abstract void computeBounds(); 
    
    /** 
            Paint the item.
            Assuming: the origin in g points to the origin of the host canvas.
    */
    public    abstract void paint(Graphics g); 
    
    
    /** 
            Is this point within our bbox? 
    */
    public boolean contains(Point p) {
        return position.x <= p.x && p.x <= position.x+bounds.width &&
               position.y <= p.y && p.y <= position.y+bounds.height; 
    }
    
    /** 
            Do we intersect the clip rectangle of this graphics?
    */
    public boolean intersects(Graphics g) {
        System.err.println("intersects trying "+position.x+", "+position.y+", "+bounds.width+", "+bounds.height);
        return g.hitClip(position.x, position.y, bounds.width, bounds.height); 
    }
    
    /** 
            Invite the canvas to ask the AWT to ask us to paint ourselves.
    */
    public void repaint() {
        System.err.print("repaint "+this+" in "+canvas.getGraphics());
        canvas.repaint(position.x, position.y, bounds.width, bounds.height);
        System.err.println(" => "+canvas.getGraphics());
    }
    
    
    /**  
            Print the item as postscript.
            This method must be overridden in concrete CanvasItems.
    */
    public void print(PrintStream os) {
        os.println(getClass().getName()+" ("+bounds+") @"+position);
    }
    
    public  Dimension getBounds() { 
        if (bounds==null) computeBounds();
        return bounds;
    }
    
    public String toString() { 
        return getClass().getName()+" ("+bounds+") @"+position;
    }
    
    /* 
            Events
    
            These are effectively no-ops in the base class, but (security
            blanket) we make them report what's happening if the debugger
            is at work.
            
    */
    public void Enter(Point position, int button) { 
        if (Debugging.canvas_itemevents) Report("enter");
    }
    
    public void Leave(Point position, int button) {
        if (Debugging.canvas_itemevents) Report("leave");
    }
    
    public void Press(Point position, int button) {
        if (Debugging.canvas_itemevents) Report("press");
    }
    
    public void Release(Point position, int button) {
    }
    
    public void Drag(Point position, int button) {
        if (Debugging.canvas_itemevents) Report("drag");
    }
    
    public void Hit(Point position, int button) {
        if (Debugging.canvas_itemevents) Report("hit");
    }
    
    public void Report(String event) {
        if (Debugging.canvas_itemevents) System.err.println(event + " at " + this);
    }
}

