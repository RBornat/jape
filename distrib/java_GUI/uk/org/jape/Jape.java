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

import java.awt.Component;
import java.awt.Container;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Rectangle;
import java.awt.Toolkit;

import java.io.File;

import java.util.Vector;

public class Jape implements DebugConstants {

    private static boolean tracing = false;
    
    public static void handleAbout() {
        AboutBox.showAboutBox();
    }

    private static boolean quitsent=false;
    
    public static void handleQuit() {
        if (!quitsent) {
            quitsent = true;
            Reply.sendCOMMAND("quit");
        }
        else
            Alert.showErrorAlert("The engine isn't responding!");
    }

    public static void dontQuit() {
        quitsent = false;
    }

    public static void handlePrefs() {
        Alert.showAlert(Alert.Info, "Preferences item selected in menu");
    }
    
    public static boolean onMacOS, onLinux, onSolaris, onWindows;
    public static Rectangle screenBounds;
    public static String defaultUnixEnginePath    = "./jape_engine";
    public static String defaultWindowsEnginePath = ".\\jape.exe" ;
    
    public static void main(String args[]) {
        // since platform independence seems not yet to have been achieved ...
        String osName = System.getProperty("os.name");
        
        if (!((onMacOS = notice_MacOSX && System.getProperty("mrj.version")!=null) 
            ||(onLinux = notice_Linux && osName.equals("Linux")) 
            ||(onSolaris = notice_Solaris && osName.equals("SunOS")) 
            ||(onWindows = osName.startsWith("Windows")))
           ) {
            Logger.log.println("Jape.main doesn't recognise OS\n"+
                               "os.name="+System.getProperty("os.name")+
                               "\nos.arch="+System.getProperty("os.arch")+
                               "\nos.version="+System.getProperty("os.version"));
        }

        if (onMacOS) { // deal with the double-bounce menu checkbox bug
            String s = System.getProperty("com.apple.macos.useScreenMenuBar");
            JapeMenu.checkboxDoubleBounce = s!=null && s.equals("true");
        }
        else
            JapeMenu.checkboxDoubleBounce = false; 

        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice gd = ge.getDefaultScreenDevice();
        GraphicsConfiguration[] gc = gd.getConfigurations();
        if (gc.length>=1) {
            screenBounds = gc[0].getBounds(); // I hope and truly believe that 0 is the default
        }
        else
            Alert.abort("no GraphicsConfiguration!");
        
        JapeMenu.initMenuBar();

        LocalSettings l = new LocalSettings();
        Vector engineCmd = new Vector();
        engineCmd.add(onWindows ? defaultWindowsEnginePath : defaultUnixEnginePath);

        // all args (except for -engine <path>) sent to engine.
        
        for (int i=0; i<args.length; i++) {
           if (args[i].equals("-engine")) {
               i++;
               if (i<args.length)
                   engineCmd.setElementAt(args[i],0);
               else
                   Alert.abort("-engine switch needs path argument");
            }
            else
                engineCmd.add(args[i]);
       }

        new Engine((String[])engineCmd.toArray(new String[engineCmd.size()]));

        Logger.init();
        
        if (tracing)
            Logger.log.println("GUI initialised");
    }
}





