// 
// $Id$
//
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
//

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

