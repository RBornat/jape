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

import java.awt.print.PageFormat;
import java.awt.print.PrinterJob;

public class PrintProof {
    static PageFormat defaultPage;

    private static void ensureDefaultPage(PrinterJob job) {
        if (defaultPage==null)
            defaultPage = job.defaultPage();
    }
    
    // run a Page Setup dialogue from the menu
    static void pageSetup() {
        PrinterJob job = PrinterJob.getPrinterJob();
        ensureDefaultPage(job);
        defaultPage = job.pageDialog(defaultPage);
    }

    static void printProof() {
        ProofWindow w;
        try {
            w = ProofWindow.getFocussedProofWindow(true);
        } catch (ProtocolError e) {
            Alert.abort("PrintProof.printProof no focussed window");
            w = null; // shut up compiler
        }
        PrinterJob job = PrinterJob.getPrinterJob();
        ensureDefaultPage(job);
        job.setPrintable(w, defaultPage);
        if (job.printDialog()) {
            try {
                job.print();
            } catch (Exception ex) {
                ex.printStackTrace();
                Alert.abort("print job failed -- see console");
            }
        }
    }
}
