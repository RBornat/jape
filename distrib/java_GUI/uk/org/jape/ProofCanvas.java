import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.io.*;

/**
        Controls the inclusion of various debugging featurettes.
*/
class Debugging
{
        static final public boolean bbox          = false;
        static final public boolean baselines     = false;
        static final public boolean events        = false;
        static final public boolean itemevents    = false;
}

/**
        A basic Jape proof canvas.
*/

public class ProofCanvas extends Canvas
{    /*
        Assignment of buttons to functions.
     */
     static  int HitButton        = 1;
     static  int TextSelectButton = 2;

     public ProofCanvas()
     { addMouseListener
       ( new MouseAdapter()
         { 
            public void mousePressed(MouseEvent e)
            {
              locate(e);
              Press(); 
            }
   
            public void mouseReleased(MouseEvent e)
            {
              locate(e);
              Release();
            }
          }
       );
       addMouseMotionListener
       ( new MouseMotionAdapter()
         {
           public void mouseDragged(java.awt.event.MouseEvent e)
           {  locate(e);
              Drag();
           }
           
           public void mouseMoved(java.awt.event.MouseEvent e)
           {  locate(e);
              Move();
           }
         }
       );              
     }

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
     protected void locate(MouseEvent e)
     { Point p    = e.getPoint();
       lastPos    = p;
       lastEvent  = e;

       /*
          This next spasm copes with the fact that Macs have 1-button
          mice, and that (in ordert to cope with this) Java AWT
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
       // Assigns the right virtual button for all but a move
       lastButton = 1;
       if (e.isAltDown())  lastButton=2;
       else
       if (e.isMetaDown()) lastButton=3;

       /*
                The item in whose bbox the mouse pointer is sitting 
                has the focus.

                All normal events are reported to the focussed item.

                Enter/Leave events are generated when the focus
                switches between items.

                This ``sharp focus'' model might be a little
                un-nerving for folks doing character selection. It
                can easily be changed so that items can CLAIM
                the focus, but first I want to see how it plays
                in Peoria.                
       */
       if (focussedItem != null && focussedItem.contains(p))
       {
          lastItem = focussedItem;
       }
       else
       {
          Enumeration is = items.elements();
          lastItem = null;
          while (is.hasMoreElements() && lastItem==null)
          { CanvasItem i = (CanvasItem) is.nextElement();
            if (i.contains(p))
            { lastItem = i;
            }
          }
       }

       // Change the focus, if necessary
       if (lastItem!=focussedItem)
       {  if (focussedItem!=null) focussedItem.Leave(lastPos, lastButton);
          focussedItem = lastItem;
          if (focussedItem!=null) 
          { focussedItem.Enter(lastPos, lastButton);
          }
       }
     }

     protected void Press()
     { if (Debugging.events) Report("press");
       if (focussedItem!=null) focussedItem.Press(lastPos, lastButton);
     }
     
     protected void Release()
     {
       if (lastEvent.getClickCount()>1) Hit();
       else
       {  if (Debugging.events) Report("release");
          if (focussedItem!=null) 
             focussedItem.Release(lastPos, lastButton);
          else
          {  // Usually means clear selections.....
          }
       }
     }
     
     protected void Drag()
     {
        if (Debugging.events) Report("drag");
        if (focussedItem!=null) 
           focussedItem.Drag(lastPos, lastButton);
        else
        {  // in some interfaces this means drag the proof around on the canvas
        }
     }

     /** Report a move -- a no-op, pro-tem. */
     protected void Move()
     {  lastButton = 0; // Correct the virtual button
        if (Debugging.events) Report("move");
     }
     
     protected void Hit()
     { 
        if (Debugging.events) Report("hit");
        if (focussedItem!=null) 
           focussedItem.Hit(lastPos, lastButton);
     }

     protected void Report(String event)
     {  System.err.println(lastEvent);
        System.err.println(event + " "+lastButton + " @" + lastPos + " on " + lastItem + " with focus "+focussedItem);
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
     public void registerItem(CanvasItem c) { items.put(c.position, c); }

     //*************** Painting Interface with the AWT


     /** Repaint the canvas. */
     public void paint(Graphics g)
     {  
        Enumeration is = items.elements();
        while (is.hasMoreElements())
        { CanvasItem i = (CanvasItem) is.nextElement();
          if (i.intersects(g)) i.paint(g);
        }
        if (Debugging.bbox)
        {  Rectangle bbox = computeBounds();
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
     public void repaint()
     {  Rectangle bbox = computeBounds();
        repaint(bbox.x, bbox.y, bbox.width, bbox.height);
     }

     /** Compute the bounding box of the proof that's being shown. */
     public Rectangle computeBounds()
     { Rectangle r = null;
       Enumeration is = items.elements();
        while (is.hasMoreElements())
        { CanvasItem i = (CanvasItem) is.nextElement();
          Rectangle bbox = new Rectangle(i.position.x, i.position.y, i.bounds.width, i.bounds.height);
          if (r==null) r=bbox; else r.add(bbox);
        } 
        return r==null?new Rectangle():r;
     }

     

     //*************** Interface with both clients and components
     
     public TextDimension stringSize(String s, int fontnum)
     { TextDimension d = font[fontnum].stringSize(s);
       d.height  += 2*textInset.height;
       d.descent += textInset.height;
       d.width   += 2*textInset.width;
       return d;
     }

     //*************** Interface with components

     /** Right edge co-ordinates of the characters in the string. */
     public int[] getBoundaries(String text, int fontnum)
     { int[] boundaries = font[fontnum].getBoundaries(text);
       for (int i=0; i<boundaries.length; i++) boundaries[i]+=textInset.width;
       return boundaries;
     }
     
     // Eventually these colours will be set as parameters.
     private Color darkergrey = Color.lightGray.darker();

     public Color getGreyedColour()
     { return darkergrey; }
     
     public Color getSelectedColour()
     { return Color.red; }
     
     public Color getTextSelectedBackgroundColour()
     { return Color.yellow; }
     
     public Color getNormalColour()
     { return Color.black; }

     // Eventually these fonts will be set as parameters.
     public Fontoid     menuFont        = Fontoid.decode("sanserif-plain-18"), 
                        commentFont     = Fontoid.decode("sanserif-plain-18"), 
                        proofFont       = Fontoid.decode("sanserif-plain-18");

     // this array is invariantly a dispatcher on the 3 canonical fonts
     public Fontoid[] font = new Fontoid[]{menuFont, commentFont, proofFont}; 

     // Size of the frame around text items. */
     public Dimension textInset = new Dimension(3, 3);


}


//*********************************************************************8<

/**     

        Base class for all canvas items.
*/

abstract class CanvasItem 
{
   public Point         position;       // Used as the key
   public TextDimension bounds;         // Bounding box size: computed lazily

   protected ProofCanvas canvas;        // The canvas that this is on        

   public CanvasItem(ProofCanvas canvas, Point position)
   { this.canvas   = canvas;
     this.position = position;
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
   public boolean contains(Point p)
   {
     return position.x <= p.x && p.x <= position.x+bounds.width &&
            position.y <= p.y && p.y <= position.y+bounds.height; 
   }

   /** 
        Do we intersect the clip rectangle of this graphics?
   */
   public boolean intersects(Graphics g)
   {
     return g.hitClip(position.x, position.y, bounds.width, bounds.height); 
   }

   /** 
        Invite the canvas to ask the AWT to ask us to paint ourselves.
   */
   public void repaint()
   {  
      canvas.repaint(position.x, position.y, bounds.width, bounds.height);
   }


   /**  
        Print the item as postscript.
        This method must be overridden in concrete CanvasItems.
   */
   public void print(PrintStream os)       // Print (Postscript) itself
   {
      os.println(getClass().getName()+" ("+bounds+") @"+position);
   }

   public  Dimension getBounds()
   { if (bounds==null) computeBounds();
     return bounds;
   }

   public String toString()
   { 
        return getClass().getName()+" ("+bounds+") @"+position;
   }

   /* 
        Events

        These are effectively no-ops in the base class, but (security
        blanket) we make them report what's happening if the debugger
        is at work.
        
   */
   public void Enter(Point position, int button)
   { 
        if (Debugging.itemevents) Report("enter");
   }

   public void Leave(Point position, int button)
   {
        if (Debugging.itemevents) Report("leave");
   }

   public void Press(Point position, int button)
   {
        if (Debugging.itemevents) Report("press");
   }

   public void Release(Point position, int button)
   {
   }

   public void Drag(Point position, int button)
   {
        if (Debugging.itemevents) Report("drag");
   }
   
   public void Hit(Point position, int button)
   {
        if (Debugging.itemevents) Report("hit");
   }
   
   public void Report(String event)
   {
        if (Debugging.itemevents) System.err.println(event + " at " + this);
   }

  
}

class TextItem extends CanvasItem
{ 

        protected String  text;
        protected int     fontnum;
        
        protected boolean selected, 
                          greyed;
                
        protected int[]   boundaries;  // actual x cooordinates of character edges.
        protected Set     marked;      // the selected character positions.
        
        public TextItem(ProofCanvas canvas, Point position, String text, int fontnum)
        { super(canvas, position);
          this.text = text;
          this.fontnum = fontnum;
          computeBounds();
        }

        protected void computeBounds()
        {
          bounds = canvas.stringSize(text, fontnum);
          boundaries = canvas.getBoundaries(text, fontnum);
          for (int i=0; i<boundaries.length; i++) boundaries[i]+=position.x;
          marked = new Set(text.length()+1);
        }

        /** 
                index of the closest character boundary to the right of x 
        */
        protected int charAt(int x)
        { // poor-man's binary search!
          for (int i=0; i<boundaries.length; i++)
          { if (x<boundaries[i]) return i;
          }
          return -1;
        }

        /**
                state variables used when dragging
        */
        int     firstx,         // x-coord of position at which we started the drag
                lastx,          // most recent dragged co-ordinate
                firstpos;       // character position at which we started the drag      
                
        boolean dragging;       // we're dragging

        public void Press(Point position, int button)
        {
             if (Debugging.itemevents) Report("press" + charAt(position.x));
             dragging = false;

             // Debugging
             if (button==2) 
             { marked.clear();
               repaint();
             }
        }
     
        public void Release(Point position, int button)
        {
             if (Debugging.itemevents) Report("release" + charAt(position.x));
             dragging  = false;
        }
     
        public void Leave(Point position, int button)
        {
             if (Debugging.itemevents) Report("leave");
             dragging = false;
        }
     
        public void Drag(Point position, int button)
        {    
        
             if (Debugging.itemevents) Report("drag" + charAt(position.x));
             if (button != canvas.TextSelectButton) return;
             
             int currentpos = charAt(position.x);
             if (dragging && currentpos>=0)
             {
                int lastcount = marked.count();
                if (firstx<=position.x) 
                { // Selecting rightwards
                  if (lastx>=position.x)
                     // changed direction means undo
                     marked.rem(currentpos-1,  currentpos);
                  else
                     marked.add(Math.max(firstpos-1, 0), currentpos);
                }
                else
                { // Selecting leftwards
                  int leftpos  = Math.max(currentpos-1, 0); 
                  if (lastx<position.x)
                     // changed direction means undo
                     marked.rem(leftpos, leftpos+1);
                  else
                     marked.add(leftpos, firstpos);
                }
                lastx = position.x;
                if (marked.count()!=lastcount) repaint(); 
             }
             else
             { if (currentpos>=0)
               { dragging = true;
                 lastx = firstx = position.x;
                 firstpos = currentpos;
               }
             }
        }
        
        /** Repaint this text. */

        public void paint(Graphics g)
        { 
           canvas.font[fontnum].setGraphics(g);
           // Background painting
           { int[]   sel    = marked.runs();
             boolean normal = true;
             int     here   = position.x+canvas.textInset.width;
             int     there;
             g.setColor(Color.green);
             for (int i=0; i<sel.length; i++)
             { there = boundaries[sel[i]];
               if (!normal) g.fillRect(here, position.y, there-here, bounds.height);
               normal = !normal;
               here = there;
             }
             
           }
           g.setColor(greyed?canvas.getGreyedColour():canvas.getNormalColour());
           g.drawString(text, position.x+canvas.textInset.width, position.y+bounds.height-bounds.descent);     
           if (Debugging.baselines)
              g.drawLine(position.x, position.y+bounds.height-bounds.descent, position.x+bounds.width-1, position.y+bounds.height-bounds.descent);
           if (selected)
           {  g.setColor(canvas.getSelectedColour());
              // java draws a w+1 x h+1 rectangle!
              g.drawRect(position.x, position.y, bounds.width-1, bounds.height-1);
              g.drawRect(position.x+1, position.y+1, bounds.width-3, bounds.height-3);
           }
        }

        public String toString()
        { return super.toString() + "["+text+"]"; }
        
}



