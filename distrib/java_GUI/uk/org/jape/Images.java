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


import java.awt.Image;
import java.awt.Toolkit;
import java.awt.Component;
import java.awt.Label;

/**
        This class provides a canonical way of acquiring the images
        used in the GUI.

        It's insufficient to use filenames to identify images,
        for the image files may be tucked away inside a jar
        (if our code came from a jar) or may be in the
        filestore in the place whence our program was loaded
        (if we're running a java class at the top level).

        We provide a way of loading an image from the place
        whence the class of a given object was loaded, and
        a (simpler) way to load an image from the
        place whence THIS class was loaded.
*/

public class Images
{ private Images() {}
  private static Images surrogate = new Images();
  
  /** Use the place that THIS code was loaded from */
  public static Image getImage(String localname)
  {
        return getImage(surrogate, localname);
  }
  
  /** Use the place that the host was loaded from */
  public static Image getImage(Object host, String localname)
  {  Toolkit tk = Toolkit.getDefaultToolkit();
     return tk.getImage(host.getClass().getResource(localname));
  }
}

