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

class TextItem extends CanvasItem { 
    protected String  text;
    protected int     fontnum;
    
    protected boolean selected, 
                      greyed;
            
    protected int[]   boundaries;  // actual x cooordinates of character edges.
    protected BitSet  marked;      // the selected character positions.
    
    public TextItem(ProofCanvas canvas, Point position, String text, int fontnum) { 
        super(canvas, position);
        this.text = text;
        this.fontnum = fontnum;
        computeBounds();
    }

    protected void computeBounds() {
        bounds = canvas.stringSize(text, fontnum);
        boundaries = canvas.getBoundaries(text, fontnum);
        for (int i=0; i<boundaries.length; i++) boundaries[i]+=position.x;
        marked = new BitSet(text.length()+1);
    }

    /** 
            index of the closest character boundary to the right of x 
    */
    protected int charAt(int x) { 
        // sequential search!
        for (int i=0; i<boundaries.length; i++)
            if (x<boundaries[i]) return i;
        return -1;
    }

    /**
            state variables used when dragging
    */
    int     firstx,         // x-coord of position at which we started the drag
            lastx,          // most recent dragged co-ordinate
            firstpos;       // character position at which we started the drag      
            
    boolean dragging;       // we're dragging

    public void Press(Point position, int button) {
        if (Debugging.canvas_itemevents) Report("press" + charAt(position.x));
        dragging = false;

        // Debugging
        if (button==2) { 
            marked.clear(); repaint();
        }
    }
    
    public void Release(Point position, int button) {
        if (Debugging.canvas_itemevents) Report("release" + charAt(position.x));
        dragging  = false;
    }
    
    public void Leave(Point position, int button) {
        if (Debugging.canvas_itemevents) Report("leave");
        dragging = false;
    }
    
    public void Drag(Point position, int button) {    
        if (Debugging.canvas_itemevents) Report("drag" + charAt(position.x));
        if (button==canvas.TextSelectButton) {
            int currentpos = charAt(position.x);
            if (dragging && currentpos>=0) {
                int lastcount = marked.count();
                if (firstx<=position.x) { 
                    // Selecting rightwards
                    if (lastx>=position.x)
                        // changed direction means undo
                        marked.rem(currentpos-1,  currentpos);
                    else
                        marked.add(Math.max(firstpos-1, 0), currentpos);
                }
                else { 
                    // Selecting leftwards
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
            if (currentpos>=0) { 
                dragging = true;
                lastx = firstx = position.x;
                firstpos = currentpos;
            }
        }
    }
    
    /** Repaint this text. */

    public void paint(Graphics g) { 
        canvas.fonts[fontnum].setGraphics(g);
        // Background painting
        int[]   sel    = marked.runs();
        boolean normal = true;
        int     here   = position.x+canvas.textInset.width;
        int     there;
        
        g.setColor(Color.green);
        for (int i=0; i<sel.length; i++) { 
            there = boundaries[sel[i]];
            if (!normal) g.fillRect(here, position.y, there-here, bounds.height);
            normal = !normal;
            here = there;
        }
            
        g.setColor(greyed?canvas.getGreyedColour():canvas.getNormalColour());
        g.drawString(text, position.x+canvas.textInset.width, position.y+bounds.height-bounds.descent);     
        if (Debugging.text_baselines)
            g.drawLine(position.x, position.y+bounds.height-bounds.descent, position.x+bounds.width-1, position.y+bounds.height-bounds.descent);
        if (selected) {
            g.setColor(canvas.getSelectedColour());
            // java draws a w+1 x h+1 rectangle!
            g.drawRect(position.x, position.y, bounds.width-1, bounds.height-1);
            g.drawRect(position.x+1, position.y+1, bounds.width-3, bounds.height-3);
        }
    }

    public String toString() { 
        return super.toString() + ": TextItem["+text+"]"; 
    }
}



