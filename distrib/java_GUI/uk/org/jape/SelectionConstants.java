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

public interface SelectionConstants {
    // bits for mouse events
    public static final byte PureSelMask     = 1<<0,
                             ExtendedSelMask = 1<<1,
                             DisjointSelMask = 1<<2,
                             TextSelMask     = 1<<3;
    
    public static final byte Selection                 = PureSelMask,
                             ExtendedSelection         = PureSelMask|ExtendedSelMask,
                             DisjointSelection         = PureSelMask|DisjointSelMask,
                             ExtendedDisjointSelection = PureSelMask|ExtendedSelMask|DisjointSelMask,
                        
                             TextSelection                 = TextSelMask,
                             ExtendedTextSelection         = TextSelMask|ExtendedSelMask,
                             DisjointTextSelection         = TextSelMask|DisjointSelMask,
                             ExtendedDisjointTextSelection = TextSelMask|ExtendedSelMask|DisjointSelMask;

    // bit selectors for recorded selections
    public static final byte NoSel        = 0,
                             HypSel       = 1<<0,
                             ConcSel      = 1<<1,
                             ReasonSel    = 1<<2,
                             AmbigSel     = 1<<3;

    public static final byte NewWorldDrag  = 1,
                             MoveWorldDrag = 2;
    
    public static final byte WorldClick    = 1,
                             WorldNoClick  = 2;
}
