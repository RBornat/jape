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

/* This entire file is required to override two bloody silly casual assignments in scrollRectVisible:
   it simply decides that nobody could want to scroll to a point where a view isn't entirely
   enclosed in the viewport.  Ugh!

   Because I had to copy so much private code; because I had to copy so much of scrollRectVisible;
   I have to check this stuff every time the Java version changes ...
 */

import java.applet.Applet;
import javax.swing.CellRendererPane;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import javax.swing.JComponent;
import javax.swing.JViewport;
import java.awt.Point;
import java.awt.Rectangle;
import javax.swing.RepaintManager;
import java.awt.Window;

public class KViewport extends JViewport {
    public KViewport() {
        super();
    }

    private void my_validateView() {
        Component validateRoot = null;

        /* Find the first JComponent ancestor of this component whose
        * isValidateRoot() method returns true.
        */
        for(Component c = this; c != null; c = c.getParent()) {
            if ((c instanceof CellRendererPane) ||
                !c.isDisplayable() /* I think (c.getPeer() == null) */) {
                return;
            }
            if ((c instanceof JComponent) &&
                (((JComponent)c).isValidateRoot())) {
                validateRoot = c;
                break;
            }
        }

        // If no validateRoot, nothing to validate from.
        if (validateRoot == null) {
            return;
        }

        // Make sure all ancestors are visible.
        Component root = null;

        for(Component c = validateRoot; c != null; c = c.getParent()) {
            if (!c.isVisible() ||
                !c.isDisplayable() /* I think (c.getPeer() == null) */) {
                return;
            }
            if ((c instanceof Window) || (c instanceof Applet)) {
                root = c;
                break;
            }
        }

        // Make sure there is a Window ancestor.
        if (root == null) {
            return;
        }

        // Validate the root.
        validateRoot.validate();

        // And let the RepaintManager it does not have to validate from
        // validateRoot anymore.
        RepaintManager rm = RepaintManager.currentManager(this);

        if (rm != null) {
            rm.removeInvalidComponent((JComponent)validateRoot);
        }
    }

    /*  This method is used by the scrollToRect method to determine the
    *  proper direction and amount to move by. The integer variables are named
    *  width, but this method is applicable to height also. The code assumes that
    *  parentWidth/childWidth are positive and childAt can be negative.
    */
    private int my_positionAdjustment(int parentWidth, int childWidth, int childAt)    {

        //   +-----+
        //   | --- |     No Change
        //   +-----+
        if (childAt >= 0 && childWidth + childAt <= parentWidth)    {
            return 0;
        }

        //   +-----+
        //  ---------   No Change
        //   +-----+
        if (childAt <= 0 && childWidth + childAt >= parentWidth) {
            return 0;
        }

        //   +-----+          +-----+
        //   |   ----    ->   | ----|
        //   +-----+          +-----+
        if (childAt > 0 && childWidth <= parentWidth)    {
            return -childAt + parentWidth - childWidth;
        }

        //   +-----+             +-----+
        //   |  --------  ->     |--------
        //   +-----+             +-----+
        if (childAt >= 0 && childWidth >= parentWidth)   {
            return -childAt;
        }

        //   +-----+          +-----+
        // ----    |     ->   |---- |
        //   +-----+          +-----+
        if (childAt <= 0 && childWidth <= parentWidth)   {
            return -childAt;
        }

        //   +-----+             +-----+
        //-------- |      ->   --------|
        //   +-----+             +-----+
        if (childAt < 0 && childWidth >= parentWidth)    {
            return -childAt + parentWidth - childWidth;
        }

        return 0;
    }


    /**
    * Overridden to scroll the view so that <code>Rectangle</code>
     * within the view becomes visible.
     *
     * @param contentRect the <code>Rectangle</code> to display
     */
    public void scrollRectToVisible(Rectangle contentRect) {
        System.err.println("in scrollRectToVisible "+contentRect);
        Component view = getView();

        if (view == null) {
            return;
        } else {
            if (!view.isValid()) {
                // If the view is not valid, validate. scrollRectToVisible
                // may fail if the view is not valid first, contentRect
                // could be bigger than invalid size.
                my_validateView();
            }
            int     dx = 0, dy = 0;

            dx = my_positionAdjustment(getWidth(), contentRect.width, contentRect.x);
            dy = my_positionAdjustment(getHeight(), contentRect.height, contentRect.y);

            if (dx != 0 || dy != 0) {
                Point viewPosition = getViewPosition();
                Dimension viewSize = view.getSize();
                int startX = viewPosition.x;
                int startY = viewPosition.y;
                Dimension extent = getExtentSize();

                viewPosition.x -= dx;
                /* this is the first offending bit
                if (viewPosition.x + extent.width > viewSize.width) {
                    viewPosition.x = Math.max(0,viewSize.width - extent.width);
                }
                else if (viewPosition.x < 0) {
                    viewPosition.x = 0;
                }
                */
                viewPosition.y -= dy;
                /* and this is the second
                if (viewPosition.y + extent.height > viewSize.height) {
                    viewPosition.y = Math.max(0, viewSize.height -
                                              extent.height);
                }
                else if (viewPosition.y < 0) {
                    viewPosition.y = 0;
                }
                */
                
                if (viewPosition.x != startX || viewPosition.y != startY) {
                    setViewPosition(viewPosition);
                    // NOTE: How JViewport currently works with the
                    // backing store is not foolproof. The sequence of
                    // events when setViewPosition
                    // (scrollRectToVisible) is called is to reset the
                    // views bounds, which causes a repaint on the
                    // visible region and sets an ivar indicating
                    // scrolling (scrollUnderway). When
                    // JViewport.paint is invoked if scrollUnderway is
                    // true, the backing store is blitted.  This fails
                    // if between the time setViewPosition is invoked
                    // and paint is received another repaint is queued
                    // indicating part of the view is invalid. There
                    // is no way for JViewport to notice another
                    // repaint has occured and it ends up blitting
                    // what is now a dirty region and the repaint is
                    // never delivered.
                    // It just so happens JTable encounters this
                    // behavior by way of scrollRectToVisible, for
                    // this reason scrollUnderway is set to false
                    // here, which effectively disables the backing
                    // store.
                    scrollUnderway = false;
                }
            }
        }
    }

}
