// 
// $Id$
//
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
//

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
        return g.hitClip(position.x, position.y, bounds.width, bounds.height); 
    }
    
    /** 
            Invite the canvas to ask the AWT to ask us to paint ourselves.
    */
    public void repaint() {  
        canvas.repaint(position.x, position.y, bounds.width, bounds.height);
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
        if (Debugging.itemevents) Report("enter");
    }
    
    public void Leave(Point position, int button) {
        if (Debugging.itemevents) Report("leave");
    }
    
    public void Press(Point position, int button) {
        if (Debugging.itemevents) Report("press");
    }
    
    public void Release(Point position, int button) {
    }
    
    public void Drag(Point position, int button) {
        if (Debugging.itemevents) Report("drag");
    }
    
    public void Hit(Point position, int button) {
        if (Debugging.itemevents) Report("hit");
    }
    
    public void Report(String event) {
        if (Debugging.itemevents) System.err.println(event + " at " + this);
    }
}

