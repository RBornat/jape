//
//  japeserver.java
//	For information on setting Java configuration information, including setting
//	Java properties, refer to the documentation at
//		http://developer.apple.com/techpubs/java/java.html
//
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
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

public class japeserver extends JFrame
{
    static final String message = "Hello World!";
    private Font font = new Font("serif", Font.ITALIC+Font.BOLD, 36);

    protected static AboutBox aboutBox;
    
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
        LocalSettings l = new LocalSettings();
        
        Toolkit.getDefaultToolkit();
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

    public static void handleAbout() {
        aboutBox.setResizable(false);
        aboutBox.setVisible(true);
        aboutBox.show();
    }

    private static boolean quitsent=false;
    
    public static void handleQuit() {
        if (!quitsent) {
            quitsent = true;
            Reply.sendCOMMAND("quit");
        }
        else {
            Alert.showAlert(Alert.Warning, "Quit twice, this time I'm doing it ...");
            System.exit(0); // should be an alert about this ...
        }
    }

    public static void handlePrefs() {
        Alert.showAlert(Alert.Info, "Preferences item selected in menu");
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
