/* 
    $Id$

    Copyright © 2003 Richard Bornat & Bernard Sufrin
     
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

import java.awt.print.PageFormat;
import java.awt.print.Paper;
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

    static final int PROOF = 1, DISPROOF = 2, BOTH = 3;
    
    static void printProof(ProofWindow w) {
        PrinterJob job = PrinterJob.getPrinterJob();
        ensureDefaultPage(job);
        w.cockeyed = false;
        w.whattoprint = BOTH;
        job.setPrintable(w, defaultPage);
        if (job.printDialog()) {
            try {
                job.print();
            } catch (Exception ex) {
                ex.printStackTrace();
                Alert.showAlert(Alert.Warning, "print job failed -- see log");
            }
        }
    }
    
    static void printTichy(ProofWindow w, int what) {
        PrinterJob job = PrinterJob.getPrinterJob();
        PageFormat page = new PageFormat();
        Paper paper = new Paper();

        w.whattoprint = what;
        ProofWindow.PrintSize printSize = w.getPrintSize();

        if ((w.cockeyed = printSize.printWidth>printSize.printHeight)) {
            paper.setSize((double)printSize.printHeight, (double)printSize.printWidth);
            paper.setImageableArea((double)0.0, (double)0.0,
                                   (double)printSize.printHeight, (double)printSize.printWidth);
        } else {
            paper.setSize((double)printSize.printWidth, (double)printSize.printHeight);
            paper.setImageableArea((double)0.0, (double)0.0,
                                   (double)printSize.printWidth, (double)printSize.printHeight);
        }
        page.setPaper(paper);

        if (w.cockeyed) {
            page.setOrientation(page.LANDSCAPE); // I think this is a bug ...
        } 
        
        job.setPrintable(w, page);
        if (job.printDialog()) {
            try {
                job.print();
            } catch (Exception ex) {
                ex.printStackTrace();
                Alert.showAlert(Alert.Warning, "print job failed -- see log");
            }
        }
    }
}
