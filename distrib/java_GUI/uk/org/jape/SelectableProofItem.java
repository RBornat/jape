/* 
    $Id$
    
    Copyright © 2003 Richard Bornat & Bernard Sufrin
        
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

public abstract class SelectableProofItem extends    TextSelectableProofItem
                                          implements SelectableItem,
                                                     ProtocolConstants {

    protected SelectionRect selectionRect;

    protected final ProofCanvas canvas;

    public SelectableProofItem(ProofCanvas canvas, int x, int y, byte fontnum,
                               String annottext) {
        super(canvas,x,y,fontnum,annottext);
        this.canvas = canvas;
        selectionRect = new SelectionRect(canvas.getSurroundGap(), getBounds());
        canvas.add(selectionRect);
        addJapeMouseListener(new JapeMouseTextAdapter() {
            public void clicked(byte eventKind, MouseEvent e) {
                SelectableProofItem.this.canvas.claimFocus();
                SelectableProofItem.this.clicked(eventKind, e);
            }
            public void doubleclicked(byte eventKind, MouseEvent e) {
                SelectableProofItem.this.doubleclicked(eventKind, e);
            }
        });
    }

    protected abstract void clicked(byte eventKind, MouseEvent e);

    protected void doubleclicked(byte eventKind, MouseEvent e) {
        canvas.notifyHit(this);
    }
    
    protected class SelectionRect extends RectItem {
        public byte selkind;

        SelectionRect(int halo, Rectangle bounds) {
            super(SelectableProofItem.this.canvas,
                  bounds.x-halo, bounds.y-halo, bounds.width+2*halo, bounds.height+2*halo);
            setForeground(Preferences.SelectionColour);
        }

        protected void paintDotted(Graphics g, int y) {
            int dashlength = 3*this.canvas.linethickness;
            for (int i=0; i<right; i+=dashlength) {
                if ((i/3)%2==0) g.drawLine(i,y, Math.min(right, i+dashlength-1),y);
            }
        }

        protected void paintHooks(Graphics g, int y) {
            int hooklength = Math.min(4*this.canvas.linethickness, getWidth()/6);
            if (hooklength>this.canvas.linethickness) {
                int lefthook = hooklength-1, righthook = right-hooklength+1;
                g.drawLine(left,y, lefthook,y);
                g.drawLine(righthook,y, right,y);
            }
        }

        public void paint(Graphics g) {
            /*  At present we have two selection styles.  Reasons, and all formulae in tree style,
            are selected by surrounding them with a box.  In box style hyps get a selection
            open at the bottom, concs get a selection open at the top.  Ambigs behave differently
            when clicked in different places: near the top you get a conc-style selection, near
            the bottom a hyp-style selection, but in each case the closed end of the box is a
            dotted line.
            */

            if (paint_tracing)
                Logger.log.println("painting proof item at "+getX()+","+getY());
            
            if (selkind!=NoSel) {
                prepaint(g);

                switch (selkind) {
                    case ReasonSel:
                        super.paint(g); break;

                    case HypSel:
                        if (SelectableProofItem.this.canvas.proofStyle==BoxStyle) {
                            paintSides(g); paintHorizEdge(g, top); paintHooks(g, bottom);
                        }
                        else
                            super.paint(g);
                        break;

                    case HypSel | AmbigSel:
                        paintSides(g); paintDotted(g, top); paintHooks(g, bottom); break;

                    case ConcSel:
                        if (SelectableProofItem.this.canvas.proofStyle==BoxStyle) {
                            paintSides(g); paintHorizEdge(g, bottom); paintHooks(g, top);
                        }
                        else
                            super.paint(g);
                        break;

                    case ConcSel | AmbigSel:
                        paintSides(g); paintDotted(g, bottom); paintHooks(g, top); break;

                    default:
                        Alert.abort("SelectableProofItem.SelectionRect selkind="+selkind);
                }
            }

        }

        public void setSelkind(byte selkind) {
            this.selkind = selkind; repaint();
        }
        
        public byte getSelkind() {
            return (byte)(selkind&~AmbigSel);
        }
    }

    public byte getSelkind() {
        return selectionRect.getSelkind();
    }

    public boolean selkindOverlaps(byte selmask) {
        return (getSelkind()&selmask)!=0;
    }

    public void select(byte selkind) { selectionRect.setSelkind(selkind); }

    public void deselect() { selectionRect.setSelkind(NoSel); }

    public void blacken() {
        setForeground(Preferences.TextColour); repaint();
    }

    public void greyen() {
        deselect(); deTextSelect();
        setForeground(Preferences.GreyTextColour); repaint();
    }
}
