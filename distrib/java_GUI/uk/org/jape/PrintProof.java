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

public class PrintProof {
    /* we have to mangle panes and origins so that the printed result
    is  set out like this (for example, with obvious modifications if
                           disproof+tiles and/or provisos are empty):
                               -------
     ----------------------   |       |
     |                      | |       |
     |      disproof        | | tiles |
     |                      | |       |
     ----------------------   -------
     ---------------------------
     |                           |
     |                           |
     |      proof                |
     |                           |
     |                           |
     ---------------------------
     ---------------------
     |                     |
     |    provisos         |
     |                     |
     ---------------------

     The origin we use is irrelevant; I hope that clipping is irrelevant;
     so I've decided that the local coordinates of the left-hand top corner
     of the proof rectangle is 0,0 and worked from that
     */
}
