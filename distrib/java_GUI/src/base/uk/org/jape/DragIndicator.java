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

import java.awt.Graphics;
import java.awt.Rectangle;

@SuppressWarnings("serial")
public class DragIndicator extends RectItem {

	public DragIndicator(SelectableProofItem item) {
		this(item, item.canvas, item.getBounds(), item.canvas.getSelectionGap()-1);
	}

	public final SelectableProofItem item;

	private DragIndicator(SelectableProofItem item, JapeCanvas canvas, Rectangle bounds, int gap) {
		super(canvas, bounds.x-gap, bounds.y-gap, bounds.width+2*gap, bounds.height+2*gap);
		this.item = item;
	}

	private boolean showing;

	public void wake(int dragNum) {
		showing = dragNum==-1 ? item.getDragNum()!=-1 : item.isTarget(dragNum);
		repaint();
	}

	protected boolean getIndicating() {
		return showing;
	}

	public void paint(Graphics g) {
		if (showing) {
			setForeground(JapePrefs.FormulaDragHighlightColour);
			super.paint(g);
		}
	}
}
