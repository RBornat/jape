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

interface DebugConstants {
    static final public boolean anchoredpane_tracing     = false,
                                antialias_tracing        = false,
                                buttonlayout_tracing     = false,
                                colourseg_tracing        = false,
                                containerlayout_tracing  = false,
                                containerrepaint_tracing = false,
                                disprooflayout_tracing   = false,
                                drag_tracing             = false,
                                encoding_tracing         = false,
                                fontDebug                = false,
                                geometry_tracing         = false,
                                image_tracing            = false,
                                makeMessage_tracing      = false,
                                measure_debug            = false,
                                menubar_tracing          = false,
                                osDebug                  = false,
                                paint_tracing            = false,
                                panelempty_tracing       = false,
                                panellayout_tracing      = false,
                                panellist_tracing        = false,
                                preference_tracing       = false,
								printlayout_tracing      = false,
                                worldpaint_tracing       = false;

    static final public boolean notice_Linux             = true,
                                notice_MacOSX            = true,
                                notice_Solaris           = true,
                                notice_Windows           = true;
	
	static final public boolean englyph_prep = true;
}

