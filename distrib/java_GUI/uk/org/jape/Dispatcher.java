//
//  dispatcher.java
//  japeserver
//
//  Created by Richard Bornat on Fri Aug 09 2002.
//  Copyright (c) 2002 Richard Bornat. All rights reserved (for the moment, till I get it copylefted).
//

import java.io.*;
import java.util.*;

public class Dispatcher extends Thread {

    protected BufferedReader in;
    boolean tracing = false;
    AboutBox aboutbox;
    japeserver boss;
    JapeMenu menus;
    
    public Dispatcher(japeserver boss, AboutBox aboutbox, JapeMenu menus) {
        super("Dispatcher");
        in = new BufferedReader(new InputStreamReader(System.in));
        this.boss = boss;
        this.aboutbox = aboutbox;
        this.menus = menus;
        if (tracing)
            System.err.println("dispatcher initialised");
    }

    private void showcommand(String m, String[] command) {
        System.err.print(m+" [");
        for (int i=0; i<command.length-1; i++)
            System.err.print("\""+command[i]+"\" ");
        System.err.println("\""+command[command.length-1]+"\"]");
    }
    
    public void run() {
        Vector list = new Vector(); // initialisation to shut up the compiler
        try {
            while (true) {
                String line = in.readLine();
                if (tracing)
                    System.err.println("dispatcher reads "+line);
                String[] command = japesplit(line);
                if (tracing) {
                    showcommand("which is, being translated,", command);
                }
                
                try {
                    if (command.length!=0) {
                        String p = command[0];
                        int len = command.length;
                        
                    // GET means client is listening
                        if (p.equals("GET")&&len==1)
                            Reply.openchannel();
                        else
                    
                    // ASK is for alerts
                        if (p.equals("ASKSTART")&&len==1)
                            list = new Vector();
                        else
                        if (p.equals("ASKBUTTON")&&len==2)
                            list.add(command[1]);
                        else
                        if (p.equals("ASKNOW")&&len==4)
                            Alert.newAlert(list, toInt(command[1]), command[2], toInt(command[3]));
                        else
                    
                    // file choosing
                        if (p.equals("READFILENAME")&&len==3)
                            Reply.reply(FileChooser.newOpenDialog(command[1], command[2]));
                        else
                        
                    // font setting
                        if (p.equals("SETFONTS")&&len==2)
                            JapeFont.setfont(command[1]);
                        else
                        
                    // INVISCHARS are the way we describe syntactic structure
                        if(p.equals("SETINVISCHARS")&&len==9)
                            boss.setinvischars(
                                toChar(command[1]),toChar(command[2]),
                                toChar(command[3]),toChar(command[4]),
                                toChar(command[5]),toChar(command[6]),
                                toChar(command[7]),toChar(command[8]));
                        else
                    
                    // MENU commands
                        if (p.equals("NEWMENU")&&len==2)
                            menus.newMenu(command[1]);
                        else
                        if (p.equals("MENUITEM")&&len==5)
                            menus.newMenuItem(command[1], JapeFont.toUnicode(command[2]), command[3], command[4]);
                        else
                        if (p.equals("MAKEMENUSVISIBLE")&&len==1) {} // doesn't seem necessary
                        else
                        if (p.equals("MENUSEP")&&len==2)
                            menus.menusep(command[1]);
                        else
                        if (p.equals("ENABLEMENUITEM")&&len==4)
                            menus.enablemenuitem(command[1], command[2], toBool(command[3]));
                        else
                    
                    // OPERATOR .. deal with the keyboard in some dialogue boxes
                        if (p.equals("OPERATORSBEGIN")&&len==1)
                            list=new Vector();
                        else
                        if (p.equals("OPERATOR")&&len==2)
                            list.add(command[1]);
                        else
                        if (p.equals("OPERATORSEND")&&len==1)
                            boss.setoperators(list);
                        else
                    
                    // miscellaneous
                        if (p.equals("QUIT")&&len==1) { 
                            System.exit(0);
                        }
                        else
                        if (p.equals("SETTEXTSELECTIONMODE")&&len==2)
                            { } // for now
                        else
                        if (p.equals("VERSION")&&len==2)
                            aboutbox.setVersion(command[1]);
                        else
                            Alert.errorAlert("dispatcher doesn't understand ("+len+") "+JapeFont.toUnicode(line));
                    } // if (command.length!=0)
                } catch (ProtocolError e) {
                    System.err.println("protocol error in "+line+":: "+e.getMessage());
                    System.exit(2);
                }
            } // while
        } catch (IOException e) {
            System.err.println("japeserver crash: dispatcher fails with IOException "+e);
            System.exit(2);
        }
    }

/* 
./jape.opt
Jape proof engine: release 5.0b7 [/Users/richard/cvsdownloads/jape/jape/java_japeserver/MacOSX/build/japeserver.app/Contents/MacOS/japeserver]

japeserver initialised
ENABLEMENUITEM "Edit" "Disprove" false failed
kCGErrorFailure : CGTranslateCTM is obsolete; use CGContextTranslateCTM instead.
[OPENING "/Users/richard/cvsdownloads/jape/jape/examples/natural_deduction/I2L.jt"]
[OPENING "/Users/richard/cvsdownloads/jape/jape/examples/natural_deduction/I2L_syntax.j"]
[CLOSING "/Users/richard/cvsdownloads/jape/jape/examples/natural_deduction/I2L_syntax.j"]
[OPENING "/Users/richard/cvsdownloads/jape/jape/examples/natural_deduction/I2L_rules.j"]
[CLOSING "/Users/richard/cvsdownloads/jape/jape/examples/natural_deduction/I2L_rules.j"]
[OPENING "/Users/richard/cvsdownloads/jape/jape/examples/natural_deduction/I2L_menus.j"]
[CLOSING "/Users/richard/cvsdownloads/jape/jape/examples/natural_deduction/I2L_menus.j"]
[OPENING "/Users/richard/cvsdownloads/jape/jape/examples/natural_deduction/I2L_problems.j"]
[CLOSING "/Users/richard/cvsdownloads/jape/jape/examples/natural_deduction/I2L_problems.j"]
[OPENING "/Users/richard/cvsdownloads/jape/jape/examples/natural_deduction/I2L_disproof.j"]
[CLOSING "/Users/richard/cvsdownloads/jape/jape/examples/natural_deduction/I2L_disproof.j"]
[CLOSING "/Users/richard/cvsdownloads/jape/jape/examples/natural_deduction/I2L.jt"]
**** dispatcher doesn't understand (1) EMPTYMENUSANDPANELS
**** dispatcher doesn't understand (3) NEWMENU Backward
**** dispatcher doesn't understand (3) NEWPANEL Invalid\040conjectures 1
**** dispatcher doesn't understand (4) PANELENTRY Invalid\040conjectures E�(F�G)\040�\040(E�F)�G \"E�(F�G)\040�\040(E�F)�G\"
**** dispatcher doesn't understand (4) PANELBUTTON Invalid\040conjectures Apply \040apply\040TheoremForwardOrBackward\040%-%COMMAND%-%

*/
    private String[] japesplit(String line) {
        // split a line encoded in the japeserver obol form.
        Vector result = new Vector();
        StringBuffer buf   = new StringBuffer();
        int i = 0;
        boolean quote = false;
        boolean dbquote=false;
        int len = line.length();
        
        while (i<len) {
            char c=line.charAt(i);

            i=i+1;
            switch (c) {
                case '{': {
                    if (quote) {
                        result.add(buf.toString());
                        buf = new StringBuffer();
                        quote = false;
                    }
                    else
                        quote = true;
                    break;
                }
                case '"': {
                    dbquote = !dbquote;
                    break;
                }
                case ' ': {
                    if (!quote) {
                        if (buf.length()!=0)
                        result.add(buf.toString());
                        buf = new StringBuffer();
                    }
                    else
                        buf.append(c);
                    break;
                }
                case '\\': {
                    c=line.charAt(i);
                    i=i+1;
                    if (c=='n')
                        buf.append('\n');
                    else
                    if ('0' <= c && c <= '8') { 
                        int n=((c-'0')*8+line.charAt(i)-'0')*8+line.charAt(i+1)-'0';
                        i=i+2;
                        buf.append((char)n);
                    }
                    else
                        buf.append(c);
                    break;
                }
                default: {
                    buf.append(c);
                    break;
                }
            }
        }
        
        if (buf.length()!=0) {
            result.add(buf.toString());
        }
            
        return ((String[])result.toArray(new String[result.size()]));
    }
    
    private int toInt(String s) throws ProtocolError {
        try {
            return Integer.parseInt(s);
        } catch (Exception e) {
            throw (new ProtocolError ("\""+s+"\" can't be read as an integer"));
        }
    }
    
    private boolean toBool(String s) throws ProtocolError {
        try {
            int j = toInt(s);
            if (j==1) return true; else
            if (j==0) return false; // else fall through
        } catch (Exception e) { }
        throw (new ProtocolError ("\""+s+"\" is neither \"1\" nor \"0\""));
    }
    
    private char toChar(String s) throws ProtocolError {
        if (s.length()==1)
            return s.charAt(0);
        throw (new ProtocolError ("\""+s+"\" is not a single char string"));
    }
}
