//
//  japeserver.java
//	For information on setting Java configuration information, including setting
//	Java properties, refer to the documentation at
//		http://developer.apple.com/techpubs/java/java.html
//
//  Copyright (c) 2002 Richard Bornat. All rights reserved (for the moment, till I get it copylefted).
//

import java.awt.*;
import java.awt.event.*;
import com.apple.mrj.*;
import javax.swing.*;
import java.io.*;
import java.util.*;

public class japeserver extends JFrame
                      implements  ActionListener,
                                  MRJAboutHandler,
                                  MRJQuitHandler
{

    static final String message = "Hello World!";
    private Font font = new Font("serif", Font.ITALIC+Font.BOLD, 36);

    protected AboutBox aboutBox;
    
    // Declarations for menus
    static final JMenuBar mainMenuBar = new JMenuBar();
	
    static final JMenu fileMenu = new JMenu("File");
    protected JMenuItem miNew;
    protected JMenuItem miOpen;
    protected JMenuItem miClose;
    protected JMenuItem miSave;
    protected JMenuItem miSaveAs;
	
    static final JMenu editMenu = new JMenu("Edit");
    protected JMenuItem miUndo;
    protected JMenuItem miCut;
    protected JMenuItem miCopy;
    protected JMenuItem miPaste;
    protected JMenuItem miClear;
    protected JMenuItem miSelectAll;
	
    public void addFileMenuItems() {
        miNew = new JMenuItem ("New");
        miNew.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_N, java.awt.Event.META_MASK));
        fileMenu.add(miNew).setEnabled(true);
        miNew.addActionListener(this);

        miOpen = new JMenuItem ("Open...");
        miOpen.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_O, java.awt.Event.META_MASK));
        fileMenu.add(miOpen).setEnabled(true);
        miOpen.addActionListener(this);
		
        miClose = new JMenuItem ("Close");
        miClose.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_W, java.awt.Event.META_MASK));
        fileMenu.add(miClose).setEnabled(true);
        miClose.addActionListener(this);
		
        miSave = new JMenuItem ("Save");
        miSave.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_S, java.awt.Event.META_MASK));
        fileMenu.add(miSave).setEnabled(true);
        miSave.addActionListener(this);
		
        miSaveAs = new JMenuItem ("Save As...");
        fileMenu.add(miSaveAs).setEnabled(true);
        miSaveAs.addActionListener(this);
		
        mainMenuBar.add(fileMenu);
    }
	
	
    public void addEditMenuItems() {
        miUndo = new JMenuItem("Undo");
        miUndo.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_Z, java.awt.Event.META_MASK));
        editMenu.add(miUndo).setEnabled(true);
        miUndo.addActionListener(this);
        editMenu.addSeparator();

        miCut = new JMenuItem("Cut");
        miCut.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_X, java.awt.Event.META_MASK));
        editMenu.add(miCut).setEnabled(true);
        miCut.addActionListener(this);

        miCopy = new JMenuItem("Copy");
        miCopy.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_C, java.awt.Event.META_MASK));
        editMenu.add(miCopy).setEnabled(true);
        miCopy.addActionListener(this);

        miPaste = new JMenuItem("Paste");
        miPaste.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_V, java.awt.Event.META_MASK));
        editMenu.add(miPaste).setEnabled(true);
        miPaste.addActionListener(this);

        miClear = new JMenuItem("Clear");
        editMenu.add(miClear).setEnabled(true);
        miClear.addActionListener(this);
        editMenu.addSeparator();

        miSelectAll = new JMenuItem("Select All");
        miSelectAll.setAccelerator(KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_A, java.awt.Event.META_MASK));
        editMenu.add(miSelectAll).setEnabled(true);
        miSelectAll.addActionListener(this);

        mainMenuBar.add(editMenu);
    }
	
    public void addMenus() {
        addFileMenuItems();
        addEditMenuItems();
        setJMenuBar (mainMenuBar);
    }

    private static boolean tracing = true;

    private Vector operators;
    
    // this really needs a list construct in the interface ... easy to do.
    public void operatorsbegin() {
        operators = new Vector();
    }
    public void addoperator(String op) {
        operators.add(op);
    }
    public void operatorsend() {
        return;
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
        addMenus();

        aboutBox = new AboutBox();
        Toolkit.getDefaultToolkit();
        MRJApplicationUtils.registerAboutHandler(this);
        MRJApplicationUtils.registerQuitHandler(this);

        setVisible(true);
        
        operators = new Vector();
        new Dispatcher(this, aboutBox).start();
        
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

    public void handleQuit() {	
        System.exit(0);
    }

    // ActionListener interface (for menus)
    public void actionPerformed(ActionEvent newEvent) {
        if (newEvent.getActionCommand().equals(miNew.getActionCommand())) doNew();
        else if (newEvent.getActionCommand().equals(miOpen.getActionCommand())) doOpen();
        else if (newEvent.getActionCommand().equals(miClose.getActionCommand())) doClose();
        else if (newEvent.getActionCommand().equals(miSave.getActionCommand())) doSave();
        else if (newEvent.getActionCommand().equals(miSaveAs.getActionCommand())) doSaveAs();
        else if (newEvent.getActionCommand().equals(miUndo.getActionCommand())) doUndo();
        else if (newEvent.getActionCommand().equals(miCut.getActionCommand())) doCut();
        else if (newEvent.getActionCommand().equals(miCopy.getActionCommand())) doCopy();
        else if (newEvent.getActionCommand().equals(miPaste.getActionCommand())) doPaste();
        else if (newEvent.getActionCommand().equals(miClear.getActionCommand())) doClear();
        else if (newEvent.getActionCommand().equals(miSelectAll.getActionCommand())) doSelectAll();
    }

    public void doNew() {}
	
    public void doOpen() {}
	
    public void doClose() {}
	
    public void doSave() {}
	
    public void doSaveAs() {}
	
    public void doUndo() {}
	
    public void doCut() {}
	
    public void doCopy() {}
	
    public void doPaste() {}
	
    public void doClear() {}
	
    public void doSelectAll() {}
    
    static String theorypath, theory, ext, title, iconname,
        viewpath, pviewpath, proofpath;

    public static void main(String args[]) {
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
