import java.awt.*; 

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



