//
//  japeserver.java
//	For information on setting Java configuration information, including setting
//	Java properties, refer to the documentation at
//		http://developer.apple.com/techpubs/java/java.html
//
//  Copyright (c) 2002 Richard Bornat. All rights reserved (for the moment, till I get it copylefted).
//

import java.awt.Color;
import java.io.File;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import javax.swing.JFrame;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.util.Vector;

import com.apple.mrj.*;

public class japeserver extends JFrame
                      implements  MRJAboutHandler,
                                  MRJQuitHandler
{
    static final String message = "Hello World!";
    private Font font = new Font("serif", Font.ITALIC+Font.BOLD, 36);

    protected AboutBox aboutBox;
    
    private JapeMenu menus;

    private static boolean tracing = true;

    private Vector operators;
    
    public void setoperators(Vector list) {
        operators=list;
    }
    
    private char onbra, onket, offbra, offket, outbra, outket, lockbra, lockket;
    
    public void setinvischars(char onbra, char onket, char offbra, char offket, 
        char outbra, char outket, char lockbra, char lockket) {
        this.onbra=onbra; this.onket=onket;
        this.offbra=offbra; this.offket=offket;
        this.outbra=outbra; this.outket=outket;
        this.lockbra=lockbra; this.lockket=lockket;
    }
    
    public japeserver() {
        super("japeserver");
        this.getContentPane().setLayout(null);
        menus = new JapeMenu();
        menus.addStdMenus(this);

        aboutBox = new AboutBox();
        Toolkit.getDefaultToolkit();
        MRJApplicationUtils.registerAboutHandler(this);
        MRJApplicationUtils.registerQuitHandler(this);

        setVisible(true);
        
        operators = new Vector();
        new Dispatcher(this, aboutBox, menus).start();
        
        if (tracing)
            System.err.println("japeserver initialised");
    }

    public void paint(Graphics g) {
        super.paint(g);
        g.setColor(Color.blue);
        g.setFont (font);
        g.drawString(message, 40, 80);
    }

    public void handleAbout() {
        aboutBox.setResizable(false);
        aboutBox.setVisible(true);
        aboutBox.show();
    }

    private static boolean quitsent=false;
    
    public void handleQuit() {
        if (!quitsent) {
            quitsent = true;
            Reply.sendCOMMAND("quit");
        }
        else {
            Alert.showInfoMessage(Alert.Warning, "Quit twice, this time I'm doing it ...");
            System.exit(0); // should be an alert about this ...
        }
    }

    private static String theorypath, theory, ext, title, iconname,
        viewpath, pviewpath, proofpath;

    public static boolean onMacOS;
    public static Rectangle screenBounds;
    
    public static void main(String args[]) {
        // since platform independence seems not yet to have been achieved ...
        onMacOS = (System.getProperty("mrj.version")!=null);
        
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice gd = ge.getDefaultScreenDevice();
        GraphicsConfiguration[] gc = gd.getConfigurations();
        if (gc.length==1)
            screenBounds = gc[0].getBounds();
        else {
            System.err.println("don't know how to deal with multiple GraphicsConfiguration!");
            System.exit(2);
        }
        
        // #
        // # Set up various paths
        // # 
        // # Assumption is:
        // #   
        // #   args=[path to server, ... command invoking server ..., --, first arg of command]
        // #
        if (args.length>=4 && args[args.length-2].equals("--") && args[args.length-1].charAt(0)!='-') {
            theorypath = args[args.length-1];
            File theoryfile = new File(theorypath);
            // there must be a way of doing this in the library ..
            String name = theoryfile.getName();
            int i;
            for (i = name.length()-1; i>=0; i--) 
                if (name.charAt(i)=='.') 
                    break;
            if (i<0) {
                theory = name; ext=null;
            }
            else
            if (i==0) {
                // assume it was a directory
                // this.theorypath=os.path.join(this.theorypath, "")
                // this.title(this.appname+' in '+this.theorypath)
                // this.iconname(this.appname+' in '+this.theorypath)
                theory = ext = null;
            }
            else {
                theory = name.substring(0,i-1); ext = name.substring(i+1, name.length()-1);
                // if ext in ['.j', '.jt', '.jape']:
                //     this.wm_update(this.theorypath)
            }
        }
        
        viewpath       = null;
        pviewpath      = null;
        proofpath      = null;
        theory         = null;
        
        new japeserver();
    }

}
