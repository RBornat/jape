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
import java.awt.Graphics;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.Rectangle;

public class SelectableTextItem extends TextItem {
    protected final byte kind;
    protected boolean selected = false;
    protected byte proofstyle;
    protected int selectionthickness;
    
    public SelectableTextItem(int x, int y, byte fontnum, byte kind,
                              String annottext, String printtext,
                              byte proofstyle, int selectionthickness) {
        super(x,y,fontnum,annottext,printtext);
        this.kind=kind; this.proofstyle=proofstyle; this.selectionthickness=selectionthickness;
        inset = 2*selectionthickness;
        Rectangle r = getBounds();
        r.grow(inset,inset);
        setBounds(r);
        addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent e) {
                click(e);
            }
        });
    }

    /*  At present we have two selection styles.  Reasons, and all formulae in tree style,
        are selected by surrounding them with a box.  In box style hyps get a selection
        open at the bottom, concs get a selection open at the top.  Ambigs behave differently
        when clicked in different places: near the top you get a conc-style selection, near
        the bottom a hyp-style selection, but in each case the closed end of the box is a
        dotted line.
     */
    protected void click(MouseEvent e) {
        selected = !selected;
        repaint();
    }

    private void paintSelectionRect(Graphics g) {
        g.drawRect(0, 0, getWidth()-selectionthickness, getHeight()-selectionthickness);
    }

    public void paint(Graphics g) {
        g.setColor(selected ? Color.red : getBackground());
        if (proofstyle!=ProofWindow.boxStyle)
            paintSelectionRect(g);
        else
        switch (kind) {
            case ReasonKind:
                paintSelectionRect(g); break;
            and the rest ... 
        }
        super.paint(g);
    }
}
