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

import javax.print.DocFlavor;
import javax.print.StreamPrintServiceFactory;

public class ExportPic /* implements Printable */ {
    private static void findStreams() {
        DocFlavor flavor = DocFlavor.SERVICE_FORMATTED.PRINTABLE;
        showStreams(flavor, DocFlavor.BYTE_ARRAY.AUTOSENSE.getMimeType(), "autosense (????)");
        showStreams(flavor, DocFlavor.BYTE_ARRAY.GIF.getMimeType(), "gif (.gif)");
        showStreams(flavor, DocFlavor.BYTE_ARRAY.JPEG.getMimeType(), "jpeg (.jpeg)");
        showStreams(flavor, DocFlavor.BYTE_ARRAY.PDF.getMimeType(), "pdf (.pdf)");
        showStreams(flavor, DocFlavor.BYTE_ARRAY.PNG.getMimeType(), "png (.png)");
        showStreams(flavor, DocFlavor.BYTE_ARRAY.POSTSCRIPT.getMimeType(), "postscript (.ps)");
    }

    private static void showStreams(DocFlavor flavor, String outputMimeType, String kind) {
        StreamPrintServiceFactory[] factories =
            StreamPrintServiceFactory.lookupStreamPrintServiceFactories(flavor, outputMimeType);
        Alert.showAlert("found "+factories.length+" "+kind+" factories");
    }

    public static void exportPic(ProofWindow w) {
        findStreams();
    }
}











