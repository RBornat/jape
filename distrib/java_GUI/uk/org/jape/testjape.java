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

import java.awt.BorderLayout;
import java.awt.Point;
import javax.swing.JFrame;

public class testjape { 
    JFrame       frame;
    ProofCanvas proof;
    
    public static void main(String[] args) { 
        testjape jape = new testjape();
    }
    
    public testjape() { 
        frame = new JFrame("testjape");
        proof = new ProofCanvas();

        TextItem t = new TextItem(proof, new Point(50, 50), "foobaz", 2);
        t.selected=true;
        proof.add(t);
        t = new TextItem(proof, new Point(50, 100), "is best for you", 2);
        t.selected=true;
        t.greyed=true;
        proof.add(t);
        t = new TextItem(proof, new Point(50, 150), "on \u22d6 April \u22d7 Thursdays", 2);
        proof.add(t);

        frame.getContentPane().setLayout(new BorderLayout());
        frame.getContentPane().add(proof, "Center");
        proof.revalidate();
        frame.setVisible(true);
    }
    
}


