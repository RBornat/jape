/* 
    $Id$

    Copyright © 2003-4 Richard Bornat & Bernard Sufrin
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

    This file is part of the Jape GUI, which is part of Jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).
    
*/

package uk.org.jape;

import java.awt.Dimension;

public class TextDimension extends Dimension {
    int ascent, descent;

    public TextDimension(int width, int ascent, int descent) { 
        super(width, ascent+descent);
        this.ascent=ascent; this.descent = descent;
    }
    
    public String toString()  { 
        return "TextDimension[width="+width+",ascent="+ascent+",descent="+descent+"]";
    }
}

