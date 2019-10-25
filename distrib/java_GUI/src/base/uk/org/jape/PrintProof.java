/* 
        Copyright © 2003-19 Richard Bornat & Bernard Sufrin
     
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

import java.awt.print.PageFormat;
import java.awt.print.Paper;
import java.awt.print.PrinterJob;

import javax.swing.SwingUtilities;

public class PrintProof {
    static PageFormat defaultPage;

    private static void ensureDefaultPage(PrinterJob job) {
	if (defaultPage==null)
	    defaultPage = job.defaultPage();
    }
    
    // run a Page Setup dialogue from the menu
    static void pageSetup() {
	final PrinterJob job = PrinterJob.getPrinterJob();
	ensureDefaultPage(job);
	// See comment about invokeLater below (weehah! look at the raciness!!). RB 28.09.2004
	SwingUtilities.invokeLater(
	    new Runnable() {
	       public void run() {
		   defaultPage = job.pageDialog(defaultPage);
	       }
	    });
    }

    static final int PROOF = 1, DISPROOF = 2, BOTH = 3;
    
    static void printProof(ProofWindow w) {
	final PrinterJob job = PrinterJob.getPrinterJob();
        if (DebugVars.printdialog_tracing)
            Logger.log.println("printProof PrinterJob succeeded "+job);
	ensureDefaultPage(job);
	w.whattoprint = BOTH;
	job.setPrintable(w, defaultPage);
	// see comment about invokeLater below. RB 28.09.2004
	SwingUtilities.invokeLater(
	    new Runnable() {
	       public void run() {
		   try {
                       if (job.printDialog()) {
                           if (DebugVars.printdialog_tracing)
                               Logger.log.println("printProof setPrintable done -- starting printDialog");
                           job.print();
                           if (DebugVars.printdialog_tracing)
                               Logger.log.println("printProof job.print done");
                       }
		   } catch (Exception ex) {
		       ex.printStackTrace();
		       Alert.showAlert(Alert.Warning, "printProof print job failed -- see log window");
		   }
	       }
	    });
    }
    
    static void printTichy(ProofWindow w, int what) {
	final PrinterJob job = PrinterJob.getPrinterJob();
	if (DebugVars.printdialog_tracing)
	    Logger.log.println("printTichy PrinterJob constructed "+job);
	PageFormat page = new PageFormat();
	if (DebugVars.printdialog_tracing)
	    Logger.log.println("printTichy PageFormat constructed "+page);
	Paper paper = new Paper();
	if (DebugVars.printdialog_tracing)
	    Logger.log.println("printTichy Paper constructed "+paper);

	w.whattoprint = what;
	ProofWindow.PrintSize printSize = w.getPrintSize();
	if (DebugVars.printdialog_tracing)
	    Logger.log.println("printTichy got printSize "+printSize);
	@SuppressWarnings("unused")
        boolean cockeyed = printSize.printWidth>printSize.printHeight;

	/* if (cockeyed) {
	    paper.setSize((double)printSize.printHeight, (double)printSize.printWidth);
	    paper.setImageableArea((double)0.0, (double)0.0,
				   (double)printSize.printHeight, (double)printSize.printWidth);
	} else */ {
	    paper.setSize((double)printSize.printWidth, (double)printSize.printHeight);
	    paper.setImageableArea((double)0.0, (double)0.0,
				   (double)printSize.printWidth, (double)printSize.printHeight);
	}
	if (DebugVars.printdialog_tracing)
	    Logger.log.println("printTichy paper sizes set");
	page.setPaper(paper);
	if (DebugVars.printdialog_tracing)
	    Logger.log.println("printTichy setPaper succeeded");

	/* if (cockeyed) {
	    if (DebugVars.printdialog_tracing)
	        Logger.log.println("printTichy LANDSCAPE set (cockeyed)");
	    page.setOrientation(PageFormat.LANDSCAPE); // I think this is a bug ... and I have to do it here
	} */
	
	job.setPrintable(w, page);
	if (DebugVars.printdialog_tracing)
	    Logger.log.println("setPrintable done -- starting printDialog");
	
	/* apparently modal dialog(ue)s can cause a hang, it seems ... and don't I know it. 
	   invokeLater may sort it out. RB 28.09.2004 (It did. RB iii.2005)
	 */
	SwingUtilities.invokeLater(
	    new Runnable() {
	       public void run() {
		   try {
		       if (job.printDialog()) {
			   if (DebugVars.printdialog_tracing)
			       Logger.log.println("printTichy setPrintable done -- starting printDialog");
			   job.print();
			   if (DebugVars.printdialog_tracing)
			       Logger.log.println("printTichy job.print done");
		       }
		   } catch (Exception ex) {
		       ex.printStackTrace(Logger.log);
		       Alert.showAlert(Alert.Warning, "printTichy print job failed -- see log");
		   }
		   // desperate attempt to stop print dialogs doing stupid things
		   // job.setPrintable(null,null); 
		   if (DebugVars.printdialog_tracing)
			Logger.log.println("printTichy printing done");				       
	       }
	    });
    }
}
