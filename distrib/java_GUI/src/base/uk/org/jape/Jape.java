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

import java.awt.Color;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Taskbar;
import java.io.File;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Vector;

import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

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
            crash("The engine isn't responding!");
    }

    public static void dontQuit() {
        quitsent = false;
    }

    public static void crash(String message) {
        Logger.crash(message,2);
    }
    
    public static boolean onMacOSX, onLinux, onWindows, onUnix;
    public static Rectangle screenBounds;

    public static File appDir, picsDir, engineDir, resourceDir;

    public static String defaultUnixEnginePath    = "./jape_engine";
    public static String defaultWindowsEnginePath = ".\\jape.exe"  ;
        
    private static void propslist(Properties p, PrintWriter out) {
        out.println("-- listing properties --");
        Map<String, Object> h = new HashMap<>();
        for (Map.Entry<Object, Object> e : p.entrySet()) {
            String key = (String)e.getKey();
            h.put(key, e.getValue());
        }
        for (Map.Entry<String, Object> e : h.entrySet()) {
            String key = e.getKey();
            String val = (String)e.getValue();
            out.println(key + "=" + val);
        }
    }

    public static boolean taskbarIconSet = false;
    
    public static void main(String args[]) {
        // since platform independence seems not yet to have been achieved ...
        String osName = System.getProperty("os.name");
        
        onMacOSX = notice_MacOSX && osName.equals("Mac OS X"); // checked by RB 2019/10/25
        onLinux = notice_Linux && osName.equals("Linux");
        onWindows = osName.startsWith("Windows");
        
        onUnix = onMacOSX || onLinux || onWindows;
 
        appDir = new File (onMacOSX  ? System.getProperty("uk.org.jape.AppPackage") : ".");
        engineDir = onMacOSX ? new File (appDir, "Contents/Engine") : appDir;
        picsDir = onMacOSX ? new File (appDir, "Contents/Resources/Pics") : new File ("./Pics");
        resourceDir = onMacOSX ? new File (appDir, "Contents/Resources") : new File ("./Resources");
        
        if (!(onMacOSX || onLinux  || onWindows)) {
            Logger.log.println("Jape.main doesn't recognise OS\n"+
                               "os.name=\""+System.getProperty("os.name")+"\""+
                               "\nos.arch="+System.getProperty("os.arch")+
                               "\nos.version="+System.getProperty("os.version"));
        }

        if (osDebug)
            Logger.log.println("onMacOSX="+onMacOSX+"; onLinux="+onLinux+
                               "; onWindows="+onWindows+
                               "\nos.name="+System.getProperty("os.name")+
                               "\nos.arch="+System.getProperty("os.arch")+
                               "\nos.version="+System.getProperty("os.version"));

        if (onMacOSX && System.getProperty("java.vm.version").startsWith("1.3.")) {
            // deal with the double-bounce menu checkbox bug
            String s = System.getProperty("apple.laf.useScreenMenuBar");
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
            Alert.guiAbort("no GraphicsConfiguration!");
        
        JapeMenu.initMenuBar();

        new LocalSettings();
        final Vector<String> engineCmd = new Vector<String>();
        engineCmd.add(onWindows ? defaultWindowsEnginePath : 
                      onLinux   ? defaultUnixEnginePath    :
                      /* macOS */ (new File (engineDir, "jape_engine")).toString());

        // all args (except for -engine <path>) sent to engine.
        
        for (int i=0; i<args.length; i++) {
           if (args[i].equals("-engine")) {
               i++;
               if (i<args.length)
                   engineCmd.setElementAt(args[i],0);
               else
                   Alert.guiAbort("-engine switch needs path argument");
            }
            else
                engineCmd.add(args[i]);
        }
   
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
               new Engine((String[])engineCmd.toArray(new String[engineCmd.size()]));

                Logger.init();
                
                if (tracing)
                    Logger.log.println("GUI initialised");
                
                propslist(System.getProperties(), Logger.log);

                if (Taskbar.isTaskbarSupported()) {
                    final Taskbar taskbar = Taskbar.getTaskbar();

                    if (taskbar.isSupported(Taskbar.Feature.ICON_IMAGE)) {
                        try {
                            //set icon for mac os (and other systems which do support this method)
                            Image icon = Images.getPicsImage("japeicon.png");
                            taskbar.setIconImage(icon);
                            taskbarIconSet = true;
                        } catch (final UnsupportedOperationException e) {
                            System.out.println("The os does not support: 'taskbar.setIconImage' -- ");
                        } catch (final SecurityException e) {
                            System.out.println("There was a security exception for: 'taskbar.setIconImage' -- ");
                        }
                    }
                    else
                        Logger.log.println("no setIconImage on this machine");
                }
                else
                    Logger.log.println("no TaskBar on this machine");

                if (onMacOSX) {
                    try {
                        UIManager.setLookAndFeel("com.apple.laf.AquaLookAndFeel");
                        UIManager.put("TabbedPane.foreground", Color.BLACK); // temporary fix (up to Java 16 at least) on Apple Silicon Big Sur
                    }
                    catch (ClassNotFoundException | InstantiationException
                            | IllegalAccessException
                            | UnsupportedLookAndFeelException e) {
                        System.out.println("some exception e");
                        e.printStackTrace();
                    }
                }

            }
        });

    }
 
    /* *********************************** orphaned drags cause all sorts of problems ***********************************************/
    
    // deal with orphaned drags, which otherwise muck things up proper
    // on Mac OS X you get an orphaned drag if you touch the scroll ball during a drag. V annoying.
    
    private static RegisteredDrag dragee = null;
    
    public static RegisteredDrag getRegisteredDrag() {
        return dragee;
    }
        
    public static void registerDrag(RegisteredDrag rd) {
        dragee = rd;
    }
    
    public static void deregisterDrag(RegisteredDrag rd) {
        if (dragee!=rd)
            Alert.showErrorAlert("deregisterDrag from "+rd+" when dragee "+dragee);
        dragee = null;
    }
    
   /* ************************************************* howto do stuff messages ****************************************************/
    
    static String howTo(String what) {
        if (what.equals("FormulaSelect"))
            return LocalSettings.howToFormulaSelect +
                   "\n\n"+ howToFormulaSelect;
        else
        if (what.equals("TextSelect"))
            return LocalSettings.howToTextSelect +
                   "\n\n" + howToTextSelect;
        else
        if (what.equals("DragFormulae"))
            return howToDragFormulaeA + 
                   "\n\n" + LocalSettings.howToDragFormulae +
                   "\n\n" + howToDragFormulaeB;
        else
        if (what.equals("DragDisproofStuff"))
            return howToDragDisproofStuffA + 
                   "\n\n" + LocalSettings.howToDragDisproofStuff +
                   "\n\n" + howToDragDisproofStuffB;
        else
            return "";
    }

    static String howToFormulaSelect =
        "Every selection indicates a hypothesis (antecedent) or conclusion " +
        "(consequent) formula. " +
        "You can select more than one hypothesis with shift-click (press the " +
        "shift key while you click). Shift-click can also be used to cancel " +
        "selections."+
        "\n\n"+
        "In tree proofs, a formula selection is shown with an enclosing red box. " +
        "In box-and-line proofs, you get a box open at the bottom to show a " + 
        "hypothesis selection, or open at the top to show a conclusion " +
        "selection. Sometimes the " +
        "other side of the box is a dotted line: then by clicking in the top half " +
        "of the formula you can make a conclusion selection, open at the top; " +
        "clicking in the bottom half makes a hypothesis selection, open at the " +
        "bottom.";  
    
    static String howToTextSelect = 
        "(The same gestures can be used for token selection -- selecting a sequence of " +
        "tokens that aren't necessarily a subformula -- provided the menus allow you " +
        "to switch.)" +
        "\n\n" +
        "Subformula/token selections are shown by a yellow background behind the " +
        "selected. If one selection overlaps another, one or the other will be " +
        "cancelled. Be careful if you make adjacent subformula selections: there will " +
        "be a narrow white gap between the two.";
    
    static String howToDragFormulaeA =
        "In a multiplicative logic, the left context and/or the right context " +
        "can be split by a multiplicative rule. In such a case two things will happen: " +
        "(1) UNIFIESWITH provisos will appear below the proof, detailing the " +
        "split and how it must be resolved by dragging formulae into context " +
        "variables; (2) formulae that can be dragged into " +
        "context variables will be marked with a blue box.";
    
    static String howToDragFormulaeB =
        "The formula you are dragging will show up as a transparent copy, and " +
        "the original will remain in place. During the drag, " +
        "the blue boxes round draggable formulae will " +
        "disappear, to be replaced by blue boxes round the available destinations " +
        "(context variables which are able to accept the dragged formula). " +
        "If you move the mouse pointer over a destination, still dragging your formula, " +
        "the destination will light up (go solid blue) and you can drop your formula " +
        "into it. Jape will make the corresponding unification, and show you the result." +
        "\n\n" +
        "If you drop your formula anywhere but into a receptive destination, it will fly " +
        "back to the place it started from.";
    
    static String howToDragDisproofStuffA =
        "In a disproof diagram, everything is draggable: worlds (blobs), connections " +
        "(lines) and labels (formulae). You can drag things from one place to another; " +
        "you can drag them into the trash can (bottom right of the disproof pane); " +
        "you can duplicate them and drag the copy elsewhere.";
    
    static String howToDragDisproofStuffB =
        "During a drag, Jape shows you the diagram as it would be if you dropped the " +
        "thing you are dragging immediately. If you pass the mouse over a destination " +
        "the thing will drop into, the destination lights up (changes colour) to show " +
        "that it will accept the drop." +
        "\n\n" +
        "Worlds can be dropped anywhere, but also can be dropped onto lines. " +
        "Lines and labels can only be dropped into worlds or in the trash can. " +
        "If you drop your thing where it can't go, it will fly back " +
        "to base.";
}






