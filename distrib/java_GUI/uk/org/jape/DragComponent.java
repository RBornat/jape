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

import java.awt.Component;

import java.util.Enumeration;
import java.util.Vector;

public abstract class DragComponent extends Component {
    private Vector friendv;
    
    public void moveBy(int deltax, int deltay) {
        repaint();
        movePosition(deltax, deltay);
        repaint();
        if (friendv!=null)
            for (int i=0; i<friendv.size(); i++)
                ((DragComponent)friendv.get(i)).moveBy(deltax, deltay);
    }

    protected abstract void movePosition(int deltax, int deltay);

    public void addFriend(DragComponent friend) {
        if (friendv==null)
            friendv = new Vector();
        if (friendv.indexOf(friend)==-1)
            friendv.add(friend);
    }

    public Enumeration friends() {
        return friendv==null ? new NoFriends() : friendv.elements();
    }

    private static class NoFriends implements Enumeration {
        public boolean hasMoreElements() { return false; }
        public Object nextElement() { return null; }
    }
}
