/* 
    $Id$

    Copyright © 2003-5 Richard Bornat & Bernard Sufrin
     
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

import java.awt.Image;
import java.awt.Toolkit;
import java.awt.Component;
import java.awt.Label;

/*
    Because we now are a package, it's not a good idea to load our images
    relative to a class. So we load them from the top level of the jar,
    using ClassLoader.
*/

public class Images
{ private Images() {}
  private static Images surrogate = new Images();
  
  /** Use the place that THIS code was loaded from */
  public static Image getImage(String localname) {
	return getImage(surrogate, localname);
  }
  
  /** Use the place that the host was loaded from */
  public static Image getImage(Object host, String localname)
  {  Toolkit tk = Toolkit.getDefaultToolkit();
     return tk.getImage(java.lang.ClassLoader.getSystemClassLoader().getResource(localname));
  }
}

