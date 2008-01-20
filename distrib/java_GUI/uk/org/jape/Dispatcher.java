/* 
    $Id$

	Version modified by Stefano Mannino (Stefano.Mannino@scienze.studenti.unige.it) 
	on 2006/02/01 in order to fix the bug detailed in the "SETCHOICE bug" mail sent on 2006/02/01
	to bugs@jape.org.uk

    Copyright © 2003-5 Richard Bornat & Bernard Sufrin
     
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

import java.awt.Point;
import java.io.IOException;
import java.util.Vector;

import uk.org.jape.MiscellaneousConstants.IntString;
import uk.org.jape.PanelWindowData.Insert;

public class Dispatcher extends Thread implements DebugConstants {

	//[Mannino]: added variables currentChoice and currentChoiceText
	//[Mannino]: it may be also possible to define these variables in the
	//[Mannino]: ChoiceDialog.Choice class but it requires a deeper code modify.
	private int currentChoice;
	private String currentChoiceText;

    public Dispatcher() {
	super("Dispatcher");
	if (DebugVars.protocol_tracing)
	    Logger.log.println("dispatcher initialised");
    }

    private void showSplitLine(String m, String[] cmd) {
	Logger.log.print(m+" [");
	for (int i=0; i<cmd.length; i++) {
	    Logger.log.print(JapeUtils.enQuote(cmd[i]));
	    if (i<cmd.length-1)
		Logger.log.print(" ");
	}
	Logger.log.println("]");
    }

    public void run() {
	Vector<String> stringlist = new Vector<String>();
	Vector<IntString> givenlist = new Vector<IntString>();
	Vector<Insert> insertlist = new Vector<Insert>();
        Vector<String[]> radiobuttonlist = new Vector<String[]>();
	try {
	    while (true) {
		String line = Engine.fromEngine().readLine();
		try {
		    String[] cmd = japesplit(line);
		    if (DebugVars.protocol_tracing) {
			showSplitLine("dispatcher reads ("+cmd.length+") ", cmd);
		    }
		
		    if (cmd.length!=0) {
			String p = cmd[0].intern();
			int len = cmd.length;
			
			// GET means client is listening
			if (p=="GET"&&len==1) {
			    if (Reply.openchannel()) {
				JapeWindow.ensureMenusAvailable();
				ProofWindow.makeReady();
			    }
			}
			else

		    // string passing happens a lot, so put it early
			if (p=="STRINGSIZE"&&len==3)
			    Reply.reply(JapeFont.measure(cmd[2],
					    JapeFont.checkInterfaceFontnum(toByte(cmd[1]))));
			else
			if (p=="STRINGSIZE"&&len==2) // this can happen ... ask a stupid question
			    Reply.reply(JapeFont.measure("", 
					      JapeFont.checkInterfaceFontnum(toByte(cmd[1]))));
			else
			if (p=="PROCRUSTES"&&len==5)
			    Reply.reply(JapeFont.procrustes(toInt(cmd[1]), cmd[2], cmd[3],
					      JapeFont.checkInterfaceFontnum(toByte(cmd[4]))));
			else
			if (p=="DRAWSTRING"&&len==6)
			    ProofWindow.getFocussedWindow().drawstring(
				toInt(cmd[1]), toInt(cmd[2]),       // x, y
				toByte(cmd[3]), toByte(cmd[4]),     // font, kind
				cmd[5]);			    // annottext
			else
			if (p=="DRAWMEASUREDTEXT"&&len==5)
			    ProofWindow.getFocussedWindow().startMeasuredText(
				toInt(cmd[1]), toInt(cmd[2]),       // x, y
				toByte(cmd[3]), toInt(cmd[4]));      // kind, length
			else
			if (p=="DRAWMT"&&len==5)
			    ProofWindow.getFocussedWindow().continueMeasuredText(
				 toInt(cmd[1]), toInt(cmd[2]),      // x, y
				 toByte(cmd[3]), cmd[4]);	    // font, annottext
			else
			if (p=="DRAWMEASUREDTEXTEND"&&len==1)
			    ProofWindow.getFocussedWindow().endMeasuredText();
			else
			if (p=="DRAWRECT"&&len==5)
			    ProofWindow.getFocussedWindow().drawRect(toInt(cmd[1]), toInt(cmd[2]), // x, y
						 toInt(cmd[3]), toInt(cmd[4])); // w, h
			else
			if (p=="DRAWLINE"&&len==5)
			    ProofWindow.getFocussedWindow().drawLine(toInt(cmd[1]), toInt(cmd[2]), // x, y
						 toInt(cmd[3]), toInt(cmd[4])); // w, h
			else
			if (p=="BLACKEN"&&len==3)
			    ProofWindow.getFocussedWindow().blacken(toInt(cmd[1]), toInt(cmd[2]));
			else
			if (p=="GREYEN"&&len==3)
			    ProofWindow.getFocussedWindow().greyen(toInt(cmd[1]), toInt(cmd[2]));
			else
			if (p=="HIGHLIGHT"&&len==4)
			    ProofWindow.getFocussedWindow().highlight(toInt(cmd[1]), toInt(cmd[2]), toByte(cmd[3]));
			else
			if (p=="UNHIGHLIGHT"&&len==3)
			    ProofWindow.getFocussedWindow().unhighlight(toInt(cmd[1]), toInt(cmd[2]));
			else
			    
		    // DRAGINFO once at most in each proof step
			if (p=="DRAGSOURCES"&&len==2) {
			    int slen = toInt(cmd[1]);
			    Point[][] ss = new Point[slen][];
			    for (int i=0; i<slen; i++)
				ss[i] = readPoints();
			    SelectableProofItem.setDragSources(ss);
			    ProofWindow.wakeDragSourceIndicators();
			}
			else
			if (p=="DRAGTARGETS"&&len==2) {
			    int tlen = toInt(cmd[1]);
			    Point[][] ts = new Point[tlen][];
			    for (int i=0; i<tlen; i++)
				ts[i] = readPoints();
			    SelectableProofItem.setDragTargets(ts);
			}
			else
			if (p=="DRAGMAP"&&len==2) {
			    int mlen = toInt(cmd[1]);
			    int[][] map = new int[2*mlen][];
			    for (int i=0; i<mlen; i++) {
				map[2*i] = readInts(); map[2*i+1] = readInts();
			    }
			    SelectableProofItem.setDragMap(map);
			}
			else
			    
			// FONTINFO not very often
			if (p=="FONTINFO"&&len==2)
			    Reply.reply(JapeFont.getFontMetrics(JapeFont.checkInterfaceFontnum(toByte(cmd[1]))));
			else
			if (p=="FONTNAMES"&&len==1)
			    Reply.reply(JapeFont.getFontNames(Reply.stringSep));
			else

		    // ASK is for alerts
			if (p=="ASKSTART"&&len==1)
			    stringlist.removeAllElements();
			else
			if (p=="ASKBUTTON"&&len==2)
			    stringlist.add(cmd[1]);
			else
			if (p=="ASKNOW"&&len==4)
			    Reply.reply(Alert.ask(((String[])stringlist.toArray(new String[stringlist.size()])),
						  toInt(cmd[1]), cmd[2], toInt(cmd[3])));
			else
			if (p=="ASKDANGEROUSLY"&&len==4)
			    Reply.reply(Alert.askDangerously(cmd[1], cmd[2], cmd[3]));
			else
			if (p=="CLEARCHOICES"&&len==1)
			    ChoiceDialog.clearChoices();
			else
			if (p=="SETCHOICE"&&len==3) {
				//[Mannino]: modified the handling of the "SETCHOICE len=3" case
				if (0!=currentChoice)ChoiceDialog.setChoice(currentChoiceText,currentChoice);
				currentChoice=toInt(cmd[2]); currentChoiceText=cmd[1];
				}
			else
			if (p=="SETCHOICE"&&len==2) //[Mannino]: added the handling of the "SETCHOICE len=2" case
				currentChoiceText+=cmd[1];
			else
			if (p=="MAKECHOICE"&&len==2){
				//[Mannino]: modified the handling of the "MAKECHOICE len=2" case
				if (0!=currentChoice)ChoiceDialog.setChoice(currentChoiceText,currentChoice);
			    Reply.reply(ChoiceDialog.makeChoice(cmd[1]));
				currentChoice=0;
			}
			else
			if (p=="SETALERT"&&len==2)
			    Alert.showErrorAlert(cmd[1]);
			else
			if (p=="ASKUNIFY"&&len==2)
			    TextDialog.runUnifyDialog(cmd[1]);
			else
			if (p=="ASKLEMMA"&&len==5) {
			    String thmString = cmd[1];
			    String druleString = cmd[2];
			    int panelCount = toInt(cmd[3]);
			    int provisoCount = toInt(cmd[4]);
			    String[] panels = readLines(panelCount);
			    String[] provisos = readLines(provisoCount);
			    Reply.reply(LemmaDialog.runLemmaDialog(thmString, druleString,
								   panels, provisos));
			}
			else
		    
		    // file choosing
			if (p=="READFILENAME"&&len==3)
			    Reply.reply(FileChooser.newOpenDialog(cmd[1], cmd[2]));
			else
			if (p=="WRITEFILENAME"&&len==3)
			    Reply.reply(FileChooser.newSaveDialog(cmd[1], cmd[2]));
			else
			    
		    // font setting
			if (p=="SETFONTS"&&len==2) {
			    Alert.showAlert("The FONTS directive is out of date.\n\n"+
					    "Please delete it from your source files");
			}
			else
			
		    // INVISCHARS are the way we describe syntactic structure
			if(p=="SETINVISCHARS"&&len==9)
			    AnnotatedTextComponent.setinvischars(
				toChar(cmd[1]),toChar(cmd[2]),
				toChar(cmd[3]),toChar(cmd[4]),
				toChar(cmd[5]),toChar(cmd[6]),
				toChar(cmd[7]),toChar(cmd[8]));
			else
		    
		    // MENU commands
			if (p=="NEWMENU"&&len==3)
			    JapeMenu.newMenu(cmd[1], toInt(cmd[2]));
			else
			if (p=="MENUITEM"&&len==6)
			    JapeMenu.addItem(cmd[1], cmd[2], cmd[3], cmd[4], toInt(cmd[5]));
			else
			    if (p=="MAKEMENUSVISIBLE"&&len==1) {
			    JapeMenu.makeMenusVisible();
			    PanelWindowData.makePanelsVisible();
			}
			else
			if (p=="MENUSEP"&&len==3)
			    JapeMenu.addSeparator(cmd[1], toInt(cmd[2]));
			else
			if (p=="ENABLEMENUITEM"&&len==5) {
			    // showSplitLine("", cmd);
			    // see comment in japeserver.mli
			    JapeMenu.enableItem(false, cmd[1], cmd[2], toInt(cmd[3]), 
						toBool(cmd[4]));
			}
			else
			if (p=="MENURADIOBUTTON"&&len==1)
			    radiobuttonlist.removeAllElements();
			else
			if (p=="MENURADIOBUTTONPART"&&len==3)
			    radiobuttonlist.add(new String[] { cmd[1], cmd[2] });
			else
			if (p=="MENURADIOBUTTONEND"&&len==3)
			    JapeMenu.addRadioButtonGroup(cmd[1], 
				 (String[][])radiobuttonlist.toArray(new String[radiobuttonlist.size()][]),
				 toInt(cmd[2]));
			else
			if (p=="MENUCHECKBOX"&&len==5)
			    JapeMenu.addCheckBox(cmd[1], cmd[2], cmd[3], toInt(cmd[4]));
			else
			if (p=="TICKMENUITEM"&&len==5) {
			// showSplitLine("",cmd);
			// see comment in japeserver.mli
			JapeMenu.tickItem(false, cmd[1], cmd[2], toInt(cmd[3]), toBool(cmd[4]));
			}
			else
		    
		    // PANEL commands
			if (p=="NEWPANEL"&&len==3)
			    PanelWindowData.spawn(cmd[1], toInt(cmd[2]));
			else
			if (p=="PANELENTRY"&&len==4)
			    PanelWindowData.addEntry(cmd[1], cmd[2], cmd[3]);
			else
			if (p=="PANELBUTTON"&&len==3) {
			    stringlist.removeAllElements();
			    stringlist.add(cmd[1]);
			    stringlist.add(cmd[2]);
                            insertlist.removeAllElements();
			}
			else
			if (p=="PANELBUTTONINSERT"&&len==3) {
			    switch (toInt(cmd[1])) {
			      case 0: insertlist.add(new PanelWindowData.StringInsert(cmd[2])); break;
			      case 1: insertlist.add(new PanelWindowData.LabelInsert()); break;
			      case 2: insertlist.add(new PanelWindowData.CommandInsert()); break;
			      default: throw new ProtocolError(toInt(cmd[1])+" should be 0, 1 or 2");
			    }
			}
			else
			if (p=="PANELBUTTONEND"&&len==1) {
			    String panel = (String)stringlist.remove(0);
			    String entry = (String)stringlist.remove(0);
			    PanelWindowData.addButton(panel, entry, 
				(insertlist.toArray(new PanelWindowData.Insert[insertlist.size()])));
			}
			else
			if (p=="MARKPANELENTRY"&&len==5)
			    PanelWindowData.markEntry(cmd[1], cmd[2], toBool(cmd[3]), toBool(cmd[4]));
			else
			if (p=="SELECTPANELENTRY"&&len==3)
			    PanelWindowData.selectEntry(cmd[1], cmd[2]);
			else
		    
		    // proof window commands
			if (p=="OPENPROOF"&&len==3)
			    ProofWindow.spawn(cmd[1], toInt(cmd[2]));
			else
			if (p=="CLOSEPROOF"&&len==3)
			    ProofWindow.closeproof(toInt(cmd[1]), toBool(cmd[2]));
			else
			if (p=="PANEGEOMETRY"&&len==2)
			Reply.reply(ProofWindow.getFocussedWindow().getPaneGeometry(toByte(cmd[1])));
			else
			if (p=="SETPROOFPARAMS"&&len==3)
			    ProofWindow.getFocussedWindow().setProofParams(toByte(cmd[1]), toInt(cmd[2]));
			else
			if (p=="CLEARPANE"&&len==2)
			    ProofWindow.getFocussedWindow().clearPane(toByte(cmd[1]));
			else
			if (p=="DRAWINPANE"&&len==2)
			    ProofWindow.getFocussedWindow().drawInPane(toByte(cmd[1]));
			else

		    // disproof
			if (p=="SEQBOX"&&len==4)
			    ProofWindow.getFocussedWindow().setSequentBox(toInt(cmd[1]), toInt(cmd[2]), toInt(cmd[3]));
			else
			if (p=="EMPHASISE"&&len==4)
			    ProofWindow.getFocussedWindow().emphasise(toInt(cmd[1]), toInt(cmd[2]), toBool(cmd[3]));
			else
			if (p=="TILESSTART"&&len==1)
			    stringlist.removeAllElements();
			else
			if (p=="TILE"&&len==2)
			    stringlist.add(cmd[1]);
			else
			if (p=="TILESEND"&&len==1)
			    ProofWindow.getFocussedWindow().setDisproofTiles((String[])stringlist.toArray(new String[stringlist.size()]));
			else
			if (p=="WORLDSSTART"&&len==1)
			    ProofWindow.getFocussedWindow().worldsStart();
			else
			if (p=="WORLD"&&len==4)
			    ProofWindow.getFocussedWindow().addWorld(toInt(cmd[1]), toInt(cmd[2]), toBool(cmd[3]));
			else
			if (p=="WORLDLABEL"&&len==5)
			    ProofWindow.getFocussedWindow().addWorldLabel(toInt(cmd[1]), toInt(cmd[2]), toBool(cmd[3]), cmd[4]);
			else
			if (p=="WORLDCHILD"&&len==5)
			    ProofWindow.getFocussedWindow().addChildWorld(toInt(cmd[1]), toInt(cmd[2]), toInt(cmd[3]), toInt(cmd[4]));
			else
			if (p=="WORLDSELECT"&&len==3)
			    ProofWindow.getFocussedWindow().selectWorld(toInt(cmd[1]), toInt(cmd[2]), true);
			else
			if (p=="DISPROOFSELECT"&&len==3)
			    ProofWindow.getFocussedWindow().forceDisproofSelect(toInt(cmd[1]), toInt(cmd[2]));
			else
			if (p=="DISPROOFTEXTSELPOS"&&len==3) {
			    stringlist.removeAllElements();
			    stringlist.add(cmd[1]); stringlist.add(cmd[2]); 
			}
			else
			if (p=="DISPROOFTEXTSEL"&&len==2)
			    stringlist.add(cmd[1]);
			else
			if (p=="DISPROOFTEXTSEL"&&len==1)
			    stringlist.add("");
			else
			if (p=="DISPROOFTEXTSELDONE"&&len==1) {
			    int posX = toInt((String)stringlist.remove(0));
			    int posY = toInt((String)stringlist.remove(0));
			    ProofWindow.getFocussedWindow().forceDisproofTextSelection(
				posX, posY, (String[])stringlist.toArray(new String[stringlist.size()]));
			}
			else
			    
		    // provisos and givens
			if (p=="CLEARPROVISOVIEW"&&len==1)
			    ProofWindow.getFocussedWindow().clearProvisoView();
			else
			if (p=="SHOWPROVISOLINE"&&len==2)
			    ProofWindow.getFocussedWindow().showProvisoLine(cmd[1]);
			else
			if (p=="CLEARGIVENS"&&len==1)
			    givenlist.removeAllElements();
			else
			if (p=="GIVEN"&&len==3)
			    givenlist.add(new MiscellaneousConstants.IntString(toInt(cmd[1]), cmd[2]));
			else
			if (p=="SETGIVENS"&&len==1)
			    ProofWindow.getFocussedWindow().setGivens(
				givenlist.toArray(new  MiscellaneousConstants.IntString[givenlist.size()]));
			else

		    // selections of various kinds
			if (p=="GETPROOFSELECTIONS"&&len==1)
			    Reply.reply(ProofWindow.getFocussedWindow().getProofSelections());
			else
			if (p=="GETPROOFTEXTSELECTIONS"&&len==1)
			    Reply.reply(ProofWindow.getFocussedWindow().getProofTextSelections());
			else
			if (p=="GETDISPROOFSELECTIONS"&&len==1)
			    Reply.reply(ProofWindow.getFocussedWindow().getDisproofSelections());
			else
			if (p=="GETDISPROOFTEXTSELECTIONS"&&len==1)
			    Reply.reply(ProofWindow.getFocussedWindow().getDisproofTextSelections());
			else
			    if (p=="GETGIVENTEXTSELECTIONS"&&len==1)
			    Reply.reply(ProofWindow.getFocussedWindow().getGivenTextSelections());
			else
			
		    // OPERATOR .. deal with the keyboard in some dialogue boxes
			if (p=="OPERATORSBEGIN"&&len==1)
			    stringlist.removeAllElements();
			else
			if (p=="OPERATOR"&&len==2)
			    stringlist.add(cmd[1]);
			else
			if (p=="OPERATORSEND"&&len==1)
			    TextDialog.setOperators((String[])stringlist.toArray(new String[stringlist.size()]));
			else
		    
		    // miscellaneous
			if (p=="RESETTHEORY"&&len==1) {
			    PanelWindowData.cancelPanels();
			    JapeMenu.cancelMenus();
			    JapeWindow.resetNextPos();
			}
			else
			if (p=="EMPTYMENUSANDPANELS"&&len==2) {
			    JapeMenu.emptyMenus(toBool(cmd[1]));
			    PanelWindowData.emptyPanels();
			}
			else
			if (p=="QUIT"&&len==1) {
			    System.exit(0);
			}
			else
			if (p=="DONTQUIT"&&len==1)
			    Jape.dontQuit();
			else
			if (p=="SETTEXTSELECTIONMODE"&&len==2)
			    ProofWindow.setTextSelectionMode(toByte(cmd[1]));
			else
			if (p=="VERSION"&&len==2)
			    AboutBox.setVersion(cmd[1]);
			else
			if (p=="HOWTOTEXT"&&len==2)
			    Reply.reply(multiLine(Jape.howTo(cmd[1])));
			else
			    Alert.showErrorAlert("dispatcher doesn't understand ("+len+") "+line);
		    } // if (cmd.length!=0)
		} catch (ProtocolError e) {
		    Alert.showErrorAlert("protocol error in "+line+":: "+e.getMessage());
		}
	    } // while
	} catch (IOException e) {
	    Logger.crash("GUI crash: dispatcher fails with IOException "+e, 2);
	}
    }

    private String[] readLines(int n) {
	String[] res = new String[n];
	for (int i=0; i<n; i++) {
	    try {
		res[i] = Engine.fromEngine().readLine();
	    } catch (IOException e) {
		Logger.crash("GUI crash: dispatcher fails during readLines("+n+") at line "+i+
			     " with IOException "+e, 2);
	    }
	}
	return res;
    }
    private String[] japesplit(String line) throws ProtocolError, IOException {
    if (line==null) {
	Alert.showErrorAlert("Dispatcher sees null line -- is jape_engine ok?");
	return japesplit(Engine.fromEngine().readLine());
    }
	// split a line encoded in the obol form.
	Vector<String> result = new Vector<String>();
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
	    throw new ProtocolError (JapeUtils.enQuote(s)+" can't be read as an integer");
	}
    }

    private byte toByte(String s) throws ProtocolError {
	int i = toInt(s);
	if (Byte.MIN_VALUE<=i && i<=Byte.MAX_VALUE)
	    return (byte)i;
	else
	    throw new ProtocolError (JapeUtils.enQuote(s)+" outside range of Java byte");
    }

    private boolean toBool(String s) throws ProtocolError {
	if (s.equals("T")) 
	    return true; 
	else
	if (s.equals("F")) 
	    return false; 
	else
	    throw new ProtocolError (JapeUtils.enQuote(s)+" can't be read as a boolean "+
				      "(it's neither \"T\" nor \"F\")");
    }
    
    private char toChar(String s) throws ProtocolError {
	if (s.length()==1)
	    return s.charAt(0);
	throw new ProtocolError (JapeUtils.enQuote(s)+" is not a single char string");
    }
    
    private Point[] readPoints() throws ProtocolError, IOException {
	String line = Engine.fromEngine().readLine();
	String[] ints = japesplit(line);
	if (DebugVars.protocol_tracing) {
	    showSplitLine("readPoints reads", ints);
	}
	int len;
	if (ints.length>0 && (len=toInt(ints[0]))*2+1==ints.length) {
	    Point[] ps = new Point[len];
	    for (int i=0; i<len; i++) {
		ps[i]=new Point(toInt(ints[2*i+1]),toInt(ints[2*i+2]));
	    }
	    return ps;
	}
	else  {
	    Alert.showErrorAlert("readInts can't see point array in "+line);
	    return new Point[0];
	}
    }

    private int[] readInts() throws ProtocolError, IOException {
	String line = Engine.fromEngine().readLine();
	String[] ints = japesplit(line);
	if (DebugVars.protocol_tracing) {
	    showSplitLine("readInts reads", ints);
	}
	int len;
	if (ints.length>0 && (len=toInt(ints[0]))+1==ints.length) {
	    int[] ns = new int[len];
	    for (int i=0; i<len; i++) {
		ns[i]=toInt(ints[i+1]);
	    }
	    return ns;
	}
	else {
	    Alert.showErrorAlert("readInts can't see int array in "+line);
	    return new int[0];
	}
    }
    
    // insert a count of number of newlines to interpolate
    public String multiLine(String s) {
	int i = 0;
	int count = 0;
	int j = 0;
	while ((j=s.indexOf('\n', i))!=-1) {
	    count++; i=j+1;
	}
	return count + "\n" + s;
    }
}

