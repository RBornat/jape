/* 
    $Id$

    Copyright © 2002 Richard Bornat & Bernard Sufrin
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

    This file is part of japeserver, which is part of jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).
    
*/

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.Vector;

public class Dispatcher extends Thread implements DebugConstants {

    protected BufferedReader in;
    
    public Dispatcher() {
        super("Dispatcher");
        in = new BufferedReader(new InputStreamReader(System.in));
        if (protocol_tracing)
            System.err.println("dispatcher initialised");
    }

    private void showcommand(String m, String[] cmd) {
        System.err.print(m+" [");
        for (int i=0; i<cmd.length; i++) {
            System.err.print("\""+cmd[i]+"\"");
            if (i<cmd.length-1)
                System.err.print(" ");
        }
        System.err.println("]");
    }

    public void run() {
        Vector list = new Vector(); 
        try {
            while (true) {
                String line = in.readLine();
                try {
                    String[] cmd = japesplit(line);
                    if (protocol_tracing) {
                        showcommand("dispatcher reads ("+cmd.length+") ", cmd);
                    }
                
                    if (cmd.length!=0) {
                        String p = cmd[0];
                        int len = cmd.length;
                        
                    // GET means client is listening
                        if (p.equals("GET")&&len==1) {
                            JapeWindow.ensureMenusAvailable();
                            ProofWindow.makeReady();
                            Reply.openchannel();
                        }
                        else

                    // string passing happens a lot, so put it early
                        if (p.equals("STRINGSIZE")&&len==3)
                            Reply.reply(JapeFont.checkedMeasure(toUnicode(cmd[2]), toByte(cmd[1])));
                        else
                        if (p.equals("STRINGSIZE")&&len==2) // this can happen ... ask a stupid question
                            Reply.reply(JapeFont.checkedMeasure("", toByte(cmd[1])));
                        else
                        if (p.equals("DRAWSTRING")&&len==7)
                            ProofWindow.drawstring(toInt(cmd[1]), toInt(cmd[2]),    // x, y
                                                    toByte(cmd[3]), toByte(cmd[4]), // font, kind
                                                    toUnicode(cmd[5]),                  // annottext
                                                    toUnicode(cmd[6]));                 // printtext
                        else
                        if (p.equals("DRAWRECT")&&len==5)
                            ProofWindow.drawRect(toInt(cmd[1]), toInt(cmd[2]), // x, y
                                                 toInt(cmd[3]), toInt(cmd[4])); // w, h
                        else
                        if (p.equals("DRAWLINE")&&len==5)
                            ProofWindow.drawLine(toInt(cmd[1]), toInt(cmd[2]), // x, y
                                                 toInt(cmd[3]), toInt(cmd[4])); // w, h
                        else
                        if (p.equals("BLACKEN")&&len==3)
                            ProofWindow.blacken(toInt(cmd[1]), toInt(cmd[2]));
                        else
                        if (p.equals("GREYEN")&&len==3)
                            ProofWindow.greyen(toInt(cmd[1]), toInt(cmd[2]));
                        else
                        if (p.equals("HIGHLIGHT")&&len==4)
                            ProofWindow.highlight(toInt(cmd[1]), toInt(cmd[2]), toByte(cmd[3]));
                        else
                        if (p.equals("UNHIGHLIGHT")&&len==3)
                            ProofWindow.unhighlight(toInt(cmd[1]), toInt(cmd[2]));
                        else
                            
                    // FONTINFO not very often
                        if (p.equals("FONTINFO")&&len==2)
                            Reply.reply(JapeFont.checkedFontMetrics(toByte(cmd[1])));
                        else
                        if (p.equals("FONTNAMES")&&len==1)
                            Reply.reply(JapeFont.getFontNames(Reply.stringSep));
                        else

                    // ASK is for alerts
                        if (p.equals("ASKSTART")&&len==1)
                            list.removeAllElements();
                        else
                        if (p.equals("ASKBUTTON")&&len==2)
                            list.add(toUnicode(cmd[1]));
                        else
                        if (p.equals("ASKNOW")&&len==4)
                            Reply.reply(Alert.ask(((String[])list.toArray(new String[list.size()])),
                                                  toInt(cmd[1]), toUnicode(cmd[2]), toInt(cmd[3])));
                        else
                        if (p.equals("ASKDANGEROUSLY")&&len==4)
                            Reply.reply(Alert.askDangerously(toUnicode(cmd[1]),
                                                             toUnicode(cmd[2]),
                                                             toUnicode(cmd[3])));
                        else
                    
                    // file choosing
                        if (p.equals("READFILENAME")&&len==3)
                            Reply.reply(FileChooser.newOpenDialog(toUnicode(cmd[1]), toUnicode(cmd[2])));
                        else
                        if (p.equals("WRITEFILENAME")&&len==3)
                            Reply.reply(FileChooser.newSaveDialog(toUnicode(cmd[1]), toUnicode(cmd[2])));
                        else
                            
                    // font setting
                        if (p.equals("SETFONTS")&&len==2)
                            JapeFont.setSubstituteFont(cmd[1]);
                        else
                        
                    // INVISCHARS are the way we describe syntactic structure
                        if(p.equals("SETINVISCHARS")&&len==9)
                            TextItem.setinvischars(
                                toChar(cmd[1]),toChar(cmd[2]),
                                toChar(cmd[3]),toChar(cmd[4]),
                                toChar(cmd[5]),toChar(cmd[6]),
                                toChar(cmd[7]),toChar(cmd[8]));
                        else
                    
                    // MENU commands
                        if (p.equals("NEWMENU")&&len==3)
                            JapeMenu.newMenu(toBool(cmd[1]), toUnicode(cmd[2]));
                        else
                        if (p.equals("MENUITEM")&&len==5)
                            JapeMenu.addItem(toUnicode(cmd[1]), toUnicode(cmd[2]), toUnicode(cmd[3]), cmd[4]);
                        else
                        if (p.equals("MAKEMENUSVISIBLE")&&len==1) {
                            JapeMenu.makeMenusVisible();
                            PanelWindowData.makePanelsVisible();
                        } 
                        else
                        if (p.equals("MENUSEP")&&len==2)
                            JapeMenu.addSeparator(cmd[1]);
                        else
                        if (p.equals("ENABLEMENUITEM")&&len==4) {
                            // showcommand("", cmd);
                            // see comment in japeserver.mli
                            JapeMenu.enableItem(false, toUnicode(cmd[1]), toUnicode(cmd[2]), toBool(cmd[3]));
                        }
                        else
                        if (p.equals("MENURADIOBUTTON")&&len==1)
                            list.removeAllElements();
                        else
                        if (p.equals("MENURADIOBUTTONPART")&&len==3)
                            list.add(new String[] { toUnicode(cmd[1]), cmd[2]});
                        else
                        if (p.equals("MENURADIOBUTTONEND")&&len==2)
                            JapeMenu.addRadioButtonGroup(toUnicode(cmd[1]),
                                                         (String[][])list.toArray(new String[list.size()][]));
                        else
                        if (p.equals("MENUCHECKBOX")&&len==4)
                            JapeMenu.addCheckBox(toUnicode(cmd[1]), toUnicode(cmd[2]), cmd[3]);
                        else
                        if (p.equals("TICKMENUITEM")&&len==4) {
                            // showcommand("",cmd);
                            // see comment in japeserver.mli
                            JapeMenu.tickItem(false, toUnicode(cmd[1]), toUnicode(cmd[2]), toBool(cmd[3]));
                        }
                        else
                    
                    // PANEL commands
                        if (p.equals("NEWPANEL")&&len==3)
                            PanelWindowData.spawn(JapeFont.toUnicodeTitle(cmd[1]), toInt(cmd[2]));
                        else
                        if (p.equals("PANELENTRY")&&len==4)
                            PanelWindowData.addEntry(JapeFont.toUnicodeTitle(cmd[1]), toUnicode(cmd[2]), cmd[3]);
                        else
                        if (p.equals("PANELBUTTON")&&len==3) {
                            list.removeAllElements();
                            list.add(JapeFont.toUnicodeTitle(cmd[1]));
                            list.add(toUnicode(cmd[2]));
                        }
                        else
                        if (p.equals("PANELBUTTONINSERT")&&len==3) {
                            switch (toInt(cmd[1])) {
                              case 0: list.add(new PanelWindowData.StringInsert(cmd[2])); break;
                              case 1: list.add(new PanelWindowData.LabelInsert()); break;
                              case 2: list.add(new PanelWindowData.CommandInsert()); break;
                              default: throw new ProtocolError(toInt(cmd[1])+" should be 0, 1 or 2");
                            }
                        }
                        else
                        if (p.equals("PANELBUTTONEND")&&len==1) {
                            String panel = (String)list.remove(0);
                            String entry = (String)list.remove(0);
                            PanelWindowData.addButton(panel, entry, 
                                ((PanelWindowData.Insert[])list.toArray(new PanelWindowData.Insert[list.size()])));
                        }
                        else
                        if (p.equals("MARKPANELENTRY")&&len==5)
                            PanelWindowData.markEntry(toUnicode(cmd[1]), cmd[2], toBool(cmd[3]), toBool(cmd[4]));
                        else
                    
                    // proof window commands
                        if (p.equals("OPENPROOF")&&len==3)
                            ProofWindow.spawn(JapeFont.toUnicodeTitle(cmd[1]), toInt(cmd[2]));
                        else
                        if (p.equals("CLOSEPROOF")&&len==2)
                            ProofWindow.closeproof(toInt(cmd[1]));
                        else
                        if (p.equals("PANEGEOMETRY")&&len==2)
                        Reply.reply(ProofWindow.getPaneGeometry(toByte(cmd[1])));
                        else
                        if (p.equals("SETPROOFPARAMS")&&len==3)
                            ProofWindow.setProofParams(toByte(cmd[1]), toInt(cmd[2]));
                        else
                        if (p.equals("CLEARPANE")&&len==2)
                            ProofWindow.clearPane(toByte(cmd[1]));
                        else
                        if (p.equals("DRAWINPANE")&&len==2)
                            ProofWindow.drawInPane(toByte(cmd[1]));
                        else

                    // disproof
                        if (p.equals("SEQBOX")&&len==4)
                            ProofWindow.setSequentBox(toInt(cmd[1]), toInt(cmd[2]), toInt(cmd[3]));
                        else
                        if (p.equals("EMPHASISE")&&len==4)
                            ProofWindow.emphasise(toInt(cmd[1]), toInt(cmd[2]), toBool(cmd[3]));
                        else
                        if (p.equals("TILESSTART")&&len==1)
                            list.removeAllElements();
                        else
                        if (p.equals("TILE")&&len==2)
                            list.add(toUnicode(cmd[1]));
                        else
                        if (p.equals("TILESEND")&&len==1)
                            ProofWindow.setDisproofTiles((String[])list.toArray(new String[list.size()]));
                        else
                        if (p.equals("WORLDSSTART")&&len==1)
                            ProofWindow.worldsStart();
                        else
                        if (p.equals("WORLD")&&len==3)
                            ProofWindow.addWorld(toInt(cmd[1]), toInt(cmd[2]));
                        else
                        if (p.equals("WORLDLABEL")&&len==4)
                            ProofWindow.addWorldLabel(toInt(cmd[1]), toInt(cmd[2]), toUnicode(cmd[3]));
                        else
                        if (p.equals("WORLDCHILD")&&len==5)
                            ProofWindow.addChildWorld(toInt(cmd[1]), toInt(cmd[2]), toInt(cmd[3]), toInt(cmd[4]));
                        else
                        if (p.equals("WORLDSELECT")&&len==3)
                            ProofWindow.selectWorld(toInt(cmd[1]), toInt(cmd[2]), true);
                        else
                            
                    // provisos and givens
                        if (p.equals("CLEARGIVENS")&&len==1)
                        list.removeAllElements();
                        else
                        if (p.equals("GIVEN")&&len==3) {
                            int i = toInt(cmd[1]);
                            if (i!=list.size())
                                throw new ProtocolError("given "+i+" shouldabeen "+list.size());
                            list.add(toUnicode(cmd[2]));
                        }
                        else
                        if (p.equals("SETGIVENS")&&len==1)
                            ProofWindow.setGivens((String[])list.toArray(new String[list.size()]));
                        else
                        if (p.equals("CLEARPROVISOVIEW")&&len==1)
                            ProofWindow.clearProvisoView();
                        else

                    // selections of various kinds
                        if (p.equals("GETSELECTIONS")&&len==1)
                            Reply.reply(ProofWindow.getSelections());
                        else
                        if (p.equals("GETTEXTSELECTIONS")&&len==1)
                            Reply.reply(ProofWindow.getTextSelections());
                        else
                        if (p.equals("GETGIVENTEXTSELECTIONS")&&len==1)
                            Reply.reply(ProofWindow.getGivenTextSelections());
                        else
                        
                    // OPERATOR .. deal with the keyboard in some dialogue boxes
                        if (p.equals("OPERATORSBEGIN")&&len==1)
                            list.removeAllElements();
                        else
                        if (p.equals("OPERATOR")&&len==2)
                            list.add(cmd[1]);
                        else
                        if (p.equals("OPERATORSEND")&&len==1)
                            japeserver.setoperators((String[])list.toArray(new String[list.size()]));
                        else
                    
                    // miscellaneous
                        if (p.equals("QUIT")&&len==1) { 
                            System.exit(0);
                        }
                        else
                        if (p.equals("DONTQUIT")&&len==1)
                            japeserver.dontQuit();
                        else
                        if (p.equals("SETTEXTSELECTIONMODE")&&len==2)
                            ProofWindow.setTextSelectionMode(toByte(cmd[1]));
                        else
                        if (p.equals("VERSION")&&len==2)
                            AboutBox.setVersion(cmd[1]);
                        else
                            /*Alert.showErrorAlert*/System.err.println("dispatcher doesn't understand ("+len+") "+
                                                                       toUnicode(line));
                    } // if (cmd.length!=0)
                } catch (ProtocolError e) {
                    Alert.showErrorAlert("protocol error in "+line+":: "+e.getMessage());
                }
            } // while
        } catch (IOException e) {
            System.err.println("japeserver crash: dispatcher fails with IOException "+e);
            System.exit(2);
        }
    }

    private String[] japesplit(String line) throws ProtocolError {
        // split a line encoded in the japeserver obol form.
        Vector result = new Vector();
        StringBuffer buf = new StringBuffer();
        boolean dbquote=false;
        int len = line.length();

        for (int i=0; i<len; ) {
            char c=line.charAt(i++);

            switch (c) {
                case '"':
                    dbquote = !dbquote; // string quotes ignored ...
                    break;

                case ' ':
                    if (buf.length()!=0) {
                        result.add(buf.toString());
                        buf = new StringBuffer();
                    }
                    break;

                case '\\':
                    c=line.charAt(i++);
                    if (c=='n')
                        buf.append('\n');
                    else
                    if ('0'<=c && c<='8') { 
                        int n=((c-'0')*8+line.charAt(i++)-'0')*8+line.charAt(i++)-'0';
                        buf.append((char)n);
                    }
                    else
                        buf.append(c);
                    break;

                default:
                    buf.append(c);
                    break;
            }
        }
        
        if (buf.length()!=0) {
            result.add(buf.toString());
        }

        if (dbquote)
            throw new ProtocolError("unmatched string quote");
        else
            return ((String[])result.toArray(new String[result.size()]));
    }
    
    private int toInt(String s) throws ProtocolError {
        try {
            return Integer.parseInt(s);
        } catch (Exception e) {
            throw new ProtocolError ("\""+s+"\" can't be read as an integer");
        }
    }

    private short toShort(String s) throws ProtocolError {
        int i = toInt(s);
        if (Short.MIN_VALUE<=i && i<=Short.MAX_VALUE)
            return (short)i;
        else
            throw new ProtocolError ("\""+s+"\" outside range of Java short");
    }

    private byte toByte(String s) throws ProtocolError {
        int i = toInt(s);
        if (Byte.MIN_VALUE<=i && i<=Byte.MAX_VALUE)
            return (byte)i;
        else
            throw new ProtocolError ("\""+s+"\" outside range of Java byte");
    }

    private boolean toBool(String s) throws ProtocolError {
        if (s.equals("T")) 
            return true; 
        else
        if (s.equals("F")) 
            return false; 
        else
            throw new ProtocolError ("\""+s+"\" can't be read as a boolean "+
                                      "(it's neither \"T\" nor \"F\")");
    }
    
    private char toChar(String s) throws ProtocolError {
        if (s.length()==1)
            return s.charAt(0);
        throw new ProtocolError ("\""+s+"\" is not a single char string");
    }

    private String toUnicode(String s) {
        return JapeFont.toUnicode(s);
    }
}
