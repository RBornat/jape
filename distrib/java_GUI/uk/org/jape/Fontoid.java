/**
        A Fontoid is a font equipped with a way of measuring text.

        $Id$

        I don't really like the use of a dummy component, but it looks
        like the only concise way of getting the job done.

        Later we will have to adapt this so that measurement is
        related to the device on which the Font will be rendered.
*/

import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.util.Hashtable;
import javax.swing.JLabel;
import java.awt.Label;

public class Fontoid 
{ public    Font         font;
  public    int          descent;
    
  private FontMetrics  metrics; 
  
  private Fontoid(String name) { font = Font.decode(name); }

  static private Component dummy = null;

  public TextDimension stringSize(String s)
  { if (metrics==null) 
    { if (dummy==null) dummy = new Label("");
      metrics = dummy.getFontMetrics(font);
      descent = metrics.getMaxDescent();
    }
      
    return new TextDimension(metrics.stringWidth(s), descent+metrics.getMaxAscent(), descent);
  }
    
  /** 
    Monotonically increasing array containing the left hand edge
    distance of each character in the given string from the origin of
    the string.
  */
  // this is the job of a TextLayout, I think
  public int[] getBoundaries(String chars)
  { int   l  = chars.length();
    int[] bs = new int[l+1];
    int b = 0;
    for (int i=0; i<l; i++)
    { 
      bs[i]=b;
      b+=metrics.charWidth(chars.charAt(i));
    }
    bs[l]=b;
    return bs;          
  }
  

  public void setGraphics(Graphics g)
  {
    g.setFont(font);
  }

  public String toString()
  { return font.getName() + "-"+encode(font.getStyle())+"-"+font.getSize();
  }

  private static String encode(int s)
  {
    if (s==Font.PLAIN) return "plain"; else
    if (s==Font.BOLD) return "bold"; else
    if (s==Font.BOLD+Font.ITALIC) return "bold-italic"; else
    if (s==Font.ITALIC) return "italic"; else
    return "";
  }
  
  static protected Hashtable fonts = new Hashtable();

  static public Fontoid decode(String name)
  { Fontoid r = (Fontoid) fonts.get(name);
    if (r==null)
    { r = new Fontoid(name);
      fonts.put(name, r);
    }
    return r;
  }
}










