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

import java.awt.Graphics;

public class TextSelectableProvisoItem extends TextSelectableItem {
    public TextSelectableProvisoItem(ProvisoCanvas canvas, int x, int y, String annottext) {
        super(canvas, x, y, ProtocolConstants.ProvisoFontNum, annottext);
    }

    public String getTextSelections() {
        if (textsels==null || textsels.size()==0)
            return null;
        else {
            StringBuffer b = new StringBuffer(printchars.length+2*textsels.size());
            int i = 0;
            for (int j=0; j<textsels.size(); j++) {
                TextSel t = getTextSel(j);
                if (b.length()!=0)
                    b.append(Reply.stringSep);
                b.append(printchars, t.start, t.end-t.start);
            }
            return b.toString();
        }
    }
}
