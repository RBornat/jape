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
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.MouseEvent;
import java.awt.Rectangle;

public abstract class SelectableProofItem extends TextSelectableItem
                                          implements SelectableItem {

    public Color selectionColour = Color.red;
    protected SelectionRect selectionRect;

    protected final ProofCanvas canvas;

    public SelectableProofItem(ProofCanvas canvas, int x, int y, byte fontnum,
                               String annottext, String printtext) {
        super(canvas,x,y,fontnum,annottext,printtext);
        this.canvas = canvas;
        selectionRect = new SelectionRect();
        addMouseInteractionListener(new MouseInteractionAdapter() {
            public void clicked(byte eventKind, MouseEvent e) {
                SelectableProofItem.this.clicked(eventKind, e);
            }
        });
    }

    protected abstract void clicked(byte eventKind, MouseEvent e);
    
    protected class SelectionRect extends Component {
        public byte selkind;
        private final int left = 0, top = 0, right, bottom, selectionthickness;

        SelectionRect() {
            super();
            int inset = canvas.getSelectionHalo();
            Rectangle bounds = SelectableProofItem.this.getBounds();
            bounds.grow(inset,inset);
            setBounds(bounds);
            selectionthickness = canvas.linethickness;
            right = bounds.width-selectionthickness; bottom = bounds.height-selectionthickness;
            canvas.add(this);
        }

        // Java doesn't support wide lines, says the 1.1 docs ...
        protected void paintBox(Graphics g) {
            g.drawRect(left,top, right,bottom);
        }

        protected void paintSides(Graphics g) {
            g.drawLine(left,top, left,bottom); g.drawLine(right,top, right,bottom);
        }

        protected void paintHoriz(Graphics g, int y) {
            g.drawLine(left,y, right,y);
        }

        protected void paintDotted(Graphics g, int y) {
            int dashlength = 3*selectionthickness;
            for (int i=0; i<right; i+=dashlength) {
                if ((i/3)%2==0) g.drawLine(i,y, Math.min(right, i+dashlength-1),y);
            }
        }

        protected void paintHooks(Graphics g, int y) {
            int hooklength = 4*selectionthickness;
            int lefthook = hooklength-1, righthook = right-hooklength+1;
            g.drawLine(left,y, lefthook,y);
            g.drawLine(righthook,y, right,y);
        }

        public void paint(Graphics g) {
            /*  At present we have two selection styles.  Reasons, and all formulae in tree style,
            are selected by surrounding them with a box.  In box style hyps get a selection
            open at the bottom, concs get a selection open at the top.  Ambigs behave differently
            when clicked in different places: near the top you get a conc-style selection, near
            the bottom a hyp-style selection, but in each case the closed end of the box is a
            dotted line.
            */

            g.setColor(selectionColour);

            switch (selkind) {
                case SelectionConstants.NoSel:
                    break;

                case SelectionConstants.ReasonSel:
                    paintBox(g); break;

                case SelectionConstants.HypSel:
                    if (canvas.proofStyle==ProtocolConstants.BoxStyle) {
                        paintSides(g); paintHoriz(g, top); paintHooks(g, bottom);
                    }
                    else 
                        paintBox(g);
                    break;
                    
                case SelectionConstants.HypSel | SelectionConstants.AmbigSel:
                    paintSides(g); paintDotted(g, top); paintHooks(g, bottom); break;

                case SelectionConstants.ConcSel:
                    if (canvas.proofStyle==ProtocolConstants.BoxStyle) {
                        paintSides(g); paintHoriz(g, bottom); paintHooks(g, top);
                    }
                    else 
                        paintBox(g);
                    break;
                    
                case SelectionConstants.ConcSel | SelectionConstants.AmbigSel:
                    paintSides(g); paintDotted(g, bottom); paintHooks(g, top); break;
                        
                default:
                    Alert.abort("SelectableProofItem.SelectionRect selkind="+selkind);
            }
        }

        public void setSelkind(byte selkind) {
            this.selkind = selkind; repaint();
        }
        
        public byte getSelkind() {
            return (byte)(selkind&~SelectionConstants.AmbigSel);
        }
    }

    public byte getSelkind() {
        return (byte)(selectionRect.getSelkind()&~SelectionConstants.AmbigSel);
    }

    public boolean selkindOverlaps(byte selmask) {
        return (getSelkind()&selmask)!=0;
    }

    public void deselect() {
        selectionRect.setSelkind(SelectionConstants.NoSel);
    }
}
