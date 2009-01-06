/*
    $Id$

    Copyright © 2003-8 Richard Bornat & Bernard Sufrin

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

import java.io.FileOutputStream;
import java.io.IOException;

import javax.print.Doc;
import javax.print.DocFlavor;
import javax.print.DocPrintJob;
import javax.print.PrintException;
import javax.print.SimpleDoc;
import javax.print.StreamPrintService;
import javax.print.StreamPrintServiceFactory;

import javax.print.attribute.PrintRequestAttributeSet;
import javax.print.attribute.HashPrintRequestAttributeSet;

import javax.swing.JFrame;

public class Export {
    static String[]  buttons = new String [] { "pdf", "ps" };

    public static void export(ProofWindow w, int what) {
        /* improved hack to use OS X print dialog */
        if (Jape.onMacOSX) {
            boolean usePrintDialog = true;
            int q = Alert.myShowOptionDialog(w, buttons, Alert.Info,
                    "By default, "+
                    (what == PrintProof.BOTH ? JapeMenu.EXPORT :
                        what == PrintProof.PROOF ? JapeMenu.EXPORT_PROOF :
                            JapeMenu.EXPORT_DISPROOF)+
                            " produces PostScript (ps) files.  The OS X Print dialog can produce a pdf (using 'Save as PDF' or 'Preview').",
                            0);
            if (q==0) {
                PrintProof.printTichy(w, what);
                return;
            }
        } 

        /* Default: use the pre-defined flavor for a Printable from an InputStream */
        DocFlavor flavor = DocFlavor.SERVICE_FORMATTED.PRINTABLE;
        /* Specify the type of the output stream */
        String psMimeType = (DocFlavor.BYTE_ARRAY.POSTSCRIPT).getMimeType();
        /* Locate factory which can export a GIF image stream as Postscript */
        StreamPrintServiceFactory[] factories =
            StreamPrintServiceFactory.lookupStreamPrintServiceFactories(flavor, psMimeType);
        if (factories.length == 0) {
            Alert.showAlert("your Java system doesn't seem able to produce PostScript");
        } else
            try {
                /* Create a file for the exported postscript */
                String outputFileName = FileChooser.newSaveDialog(
                        "Save "+(what == PrintProof.BOTH  ? "image"  :
                            what == PrintProof.PROOF ? "proof image" :
                        "disproof image"),
                        ((JFrame)w).getTitle(),
                        "ps");	  
                if (outputFileName.equals(""))
                    return;

                FileOutputStream fos = new FileOutputStream(outputFileName);
                /* Create a Stream printer for Postscript */
                StreamPrintService sps = factories[0].getPrintService(fos);
                /* Create and call a Print Job */
                DocPrintJob pj = sps.createPrintJob();
                PrintRequestAttributeSet aset = new HashPrintRequestAttributeSet();
                w.whattoprint = what;
                Doc doc = new SimpleDoc(w, flavor, null);
                pj.print(doc, aset);
                fos.close();
            } catch (PrintException pe) {
                Alert.showAlert("ExportPic "+pe);
            } catch (IOException ie) {
                Alert.showAlert("ExportPic "+ie);
            }
    }
}











