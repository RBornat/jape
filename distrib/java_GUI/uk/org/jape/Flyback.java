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
import java.awt.Point;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Timer;

public class Flyback implements ActionListener, MiscellaneousConstants {
    Component c;
    Timer timer;

    int lastx, lasty, destx, desty;
    
    public Flyback(Component c, Point start, Point dest) {
        this.c = c;
        this.lastx = start.x; this.lasty = start.y;
        this.destx = dest.x; this.desty = dest.y;
        
        timer = new Timer(1000/FlybackFramesPerSecond, this);
        timer.setInitialDelay(0);
        timer.setCoalesce(true);

        timer.start();
    }

    private int delta(int last, int dest) {
        if (last<=dest)
            return Math.min(dest-last, FlybackDelta);
        else
            return Math.max(dest-last, -FlybackDelta);
    }
    
    public void actionPerformed(ActionEvent e) {
        if (lastx==destx && lasty==desty) {
            timer.stop();
            finishFlyback();
        }
        else {
            int nextx = lastx+delta(lastx, destx), nexty = lasty+delta(lasty, desty);
            c.repaint();
            c.setLocation(nextx, nexty);
            c.repaint();
            lastx = nextx; lasty = nexty;
        }
    }

    protected void finishFlyback() { }
}
