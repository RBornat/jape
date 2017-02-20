/* 
        Copyright © 2003-17 Richard Bornat & Bernard Sufrin
     
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

import java.awt.Canvas;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.util.Vector;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

public class JapeFont implements DebugConstants, ProtocolConstants {

    private static Font deriveFont(Font f, int style, int size) {
	return f.deriveFont(style, (float)size);
    }

    public static final int MENUENTRY	= 100,
			    DIALOGLABEL = 201,
			    PROOFPANEL	= 300,
			    PANELENTRY	= 301,
			    PANELBUTTON = 302,
			    LOGWINDOW	= 400,
			    TEXTINPUT	= 500;

    public static byte
	FormulaFontSize	    = JapePrefs.getProp("FormulaFontSize",       LocalSettings.FormulaFontSize),
	ReasonFontSize	    = JapePrefs.getProp("ReasonFontSize",	 LocalSettings.NonFormulaFontSize),
	ProvisoFontSize	    = JapePrefs.getProp("ProvisoFontSize",	 ReasonFontSize),
	PanelButtonFontSize = JapePrefs.getProp("PanelButtonFontSize",   LocalSettings.NonFormulaFontSize),
	PanelEntryFontSize  = JapePrefs.getProp("PanelEntryFontSize",    PanelButtonFontSize),
	LogWindowFontSize   = JapePrefs.getProp("LogWindowFontSize",     LocalSettings.NonFormulaFontSize);

    public static class BaseFont {
        private static String FontFamily =
            JapePrefs.getProp("FontFamily", Jape.onMacOSX && JapeUtils.isIn("Lucida Sans Unicode", GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames()) ? "Lucida Sans Unicode" : "SansSerif");
        public static String getFontFamily() {
            return FontFamily;
        }static 
        public void setFontFamily(String family) {
            FontFamily = family;
            JapePrefs.putProp("FontFamily", family);
            BaseFont = newBaseFont(family);
        }
        private static Font BaseFont = newBaseFont(FontFamily);
        public static Font getBaseFont() {
            return BaseFont;
        }
        private static Font newBaseFont(String family) {
            return new Font(FontFamily, Font.PLAIN, 1);
        }
    }

    public static final int[] normalsizes = { 9, 10, 11, 12, 13, 14, 18, 24, 36, 48, 72 };

    private static Vector<Integer> initsizes(int size) {
	Vector<Integer> sizes = new Vector<Integer>(normalsizes.length);
	boolean included = false;
	for (int i=0; i<normalsizes.length; i++) {
	    if (!included && size<=normalsizes[i]) {
		if (size!=normalsizes[i])
		    sizes.add(new Integer(size));
		included = true;
	    }
	    sizes.add(new Integer(normalsizes[i]));
	}
	if (!included)
	    sizes.add(new Integer(size));
	return sizes;
    }

    private static class SizeSelector {
	final JLabel label; final int size;
	JComboBox<Integer> comboBox;
	SizeSelector(String label, int size) {
	    this.label = new JLabel(label); this.size = size;
	    Vector<Integer> sizes = initsizes(size);
	    comboBox = new JComboBox<Integer>(sizes);
	    for (int j=0; j<sizes.size(); j++)
		if (sizes.get(j).intValue()==size) {
		    comboBox.setSelectedIndex(j); break;
		}
	}
    }
	
    private static void addLabelledComboBox(JPanel panel, GridBagLayout gridbag, 
            JLabel label, GridBagConstraints labelconstraints, 
            JComboBox<?> comboBox, GridBagConstraints comboconstraints) {
        gridbag.setConstraints(label, labelconstraints);
        panel.add(label);
        gridbag.setConstraints(comboBox, comboconstraints);
        panel.add(comboBox);
    }
    
    public static void runFontSizesDialog() {
	SizeSelector [] fontSizes = {
	    new SizeSelector("Formula font size"       , FormulaFontSize   ), // 0
	    new SizeSelector("Reason/Proviso font size", ReasonFontSize	   ), // 1
	    new SizeSelector("Panel font size"	       , PanelEntryFontSize), // 2
	    new SizeSelector("Log window font size"    , LogWindowFontSize )  // 3
	};
	JPanel panel = new JPanel();
	GridBagLayout gridbag = new GridBagLayout();
	panel.setLayout(gridbag);

	GridBagConstraints labelconstraints = new GridBagConstraints(),
			   comboconstraints = new GridBagConstraints();

	labelconstraints.gridwidth = GridBagConstraints.RELATIVE; //next-to-last
	labelconstraints.fill = GridBagConstraints.NONE;      //reset to default
	labelconstraints.anchor = GridBagConstraints.EAST;
	labelconstraints.weightx = 0.0;			      //reset to default

	comboconstraints.gridwidth = GridBagConstraints.REMAINDER;     //end row
	comboconstraints.fill = GridBagConstraints.NONE;
	comboconstraints.weightx = 1.0;
	comboconstraints.anchor = GridBagConstraints.WEST;
	comboconstraints.insets = new Insets(0, 5, 5, 0);

	String [] fontFamilies = GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
	JComboBox<String> fontBox = new JComboBox<String>(fontFamilies);
	for (int i=0; i<fontFamilies.length; i++) 
	    if (fontFamilies[i].startsWith(BaseFont.getFontFamily())) {
	        fontBox.setSelectedIndex(i);
	        break;
	    }
	        
	addLabelledComboBox(panel, gridbag, new JLabel("Font name"), labelconstraints, fontBox, comboconstraints);
	
	for (int i=0; i<fontSizes.length; i++) {
	    addLabelledComboBox(panel, gridbag, fontSizes[i].label, labelconstraints, fontSizes[i].comboBox, comboconstraints);
	}
	
	int reply = JOptionPane.showConfirmDialog(JapeWindow.getTopWindow(), panel, 
	                                            "Font sizes", JOptionPane.OK_CANCEL_OPTION, 
	                                            JOptionPane.PLAIN_MESSAGE);

	if (reply==JOptionPane.OK_OPTION) {
	    boolean interfaceChanged = false;
	    String family = (String)fontBox.getSelectedItem();
	    if (family!=BaseFont.getFontFamily()) {
	        BaseFont.setFontFamily(family);
	        PanelWindowData.font_reset();
	        Logger.font_reset();
	        interfaceChanged = true;
	    }
	    for (int i=0; i<fontSizes.length; i++) {
	        int selectedvalue = ((Integer)fontSizes[i].comboBox.getSelectedItem()).intValue();
		if (selectedvalue!=fontSizes[i].size) {
		    switch (i) {
			case 0:
			    FormulaFontSize = (byte)selectedvalue;
			    if (FormulaFontSize==LocalSettings.FormulaFontSize)
				JapePrefs.putProp("FormulaFontSize", null);
			    else
				JapePrefs.putProp("FormulaFontSize", FormulaFontSize);
			    interfaceChanged = true; break;
			case 1:
			    ReasonFontSize = ProvisoFontSize = (byte)selectedvalue;
			    if (ReasonFontSize==LocalSettings.NonFormulaFontSize)
				JapePrefs.putProp("ReasonFontSize", null);
			    else
				JapePrefs.putProp("ReasonFontSize", ReasonFontSize);
			    interfaceChanged = true; break;
			case 2:
			    PanelButtonFontSize = PanelEntryFontSize = (byte)selectedvalue;
			    if (PanelButtonFontSize==LocalSettings.NonFormulaFontSize)
				JapePrefs.putProp("PanelButtonFontSize", null);
			    else
				JapePrefs.putProp("PanelButtonFontSize", PanelButtonFontSize);
			    PanelWindowData.font_reset(); break;
			case 3:
			    LogWindowFontSize = (byte)selectedvalue;
			    if (LogWindowFontSize==LocalSettings.NonFormulaFontSize)
				JapePrefs.putProp("LogWindowFontSize", null);
			    else
				JapePrefs.putProp("LogWindowFontSize", LogWindowFontSize);
			    Logger.font_reset(); break;
			default: Alert.abort("runFontSizesDialog switch sees "+i);
		    }
		    
		}
	    }
	    if (interfaceChanged) {
		interfaceFonts = null; interfaceMetrics = null;
		Reply.sendCOMMAND("fonts_reset");
	    }
	}
    }
    
    public static void setComponentFont(Component c, int kind) {
	switch (kind) {
	    case MENUENTRY  : 
	    case DIALOGLABEL:
		mimicFont(c); break;
	    case PANELENTRY :
		mimicFont(c, PanelEntryFontSize); break;
	    case PANELBUTTON:
		mimicFont(c, PanelButtonFontSize); break;
	    case PROOFPANEL:
		// don't know yet
		break;
	    case LOGWINDOW:
		mimicFont(c, LogWindowFontSize); break;
	    case TEXTINPUT:
		mimicFont(c, FormulaFontSize); break;
	    default:
		Alert.showErrorAlert("setComponentFont("+kind+","+c);
	}
    }

    private static void mimicFont(Component c) {
	// use size info from component itself
	mimicFont(c, c.getFont().getSize());
    }

    private static Font mimics[] = new Font[0];
	
    private static void mimicFont(Component c, int size) {
	Font f = c.getFont();
	if (!BaseFont.getFontFamily().equals(f.getName()) || f.getSize()!=size) {
	    if (size>mimics.length) {
		Font mimics1[] = new Font[size+1];
		for (int i = 0; i<mimics.length; i++)
		    mimics1[i] = mimics[i];
		for (int i = mimics.length; i<mimics1.length; i++)
		    mimics1[i] = null;
		mimics = mimics1;
	    }
	    if (mimics[size]==null 
	            || !mimics[size].getName().equals(BaseFont.getFontFamily()) 
	            || mimics[size].getStyle()!=f.getStyle())
	        mimics[size] = deriveFont(BaseFont.getBaseFont(), f.getStyle(), size);
	    c.setFont(mimics[size]);
	}
    }

    public static byte[] interfaceFontSizes;
    private static Font[] interfaceFonts;

    private static void initInterfaceFonts() {
	if (interfaceFonts==null) {
	    if (fontDebug) {
		GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();

		String ffns[] = ge.getAvailableFontFamilyNames();
		Logger.log.println("Font names:");
		for (int i=0; i<ffns.length; i++)
		    Logger.log.println(ffns[i]);
		Logger.log.println();

		Font fs[] = ge.getAllFonts();
		Logger.log.println("Fonts.getName");
		for (int i=0; i<fs.length; i++)
		    Logger.log.println(fs[i].getName());
		Logger.log.println();
	    }

	    if (fontDebug)
		Logger.log.println("base font "+BaseFont.getBaseFont());

	    setInterfaceFonts(BaseFont.getBaseFont());
	}
    }
    
    private static void setInterfaceFonts(Font base) {
	interfaceFonts = new Font[3];
	interfaceFontSizes = new byte[]{ FormulaFontSize, ReasonFontSize, ProvisoFontSize };
	for (int i=TermFontNum; i<=ProvisoFontNum; i++)
	    interfaceFonts[i] = deriveFont(base, Font.PLAIN, interfaceFontSizes[i]);
    }

    public static String getFontNames(String sep) {
	initInterfaceFonts();
	String s = null;
	for (int i=TermFontNum; i<=ProvisoFontNum; i++) {
	    Font f = interfaceFonts[i];
	    s = (s==null ? "" : s+sep)+f.getName()+","+f.getStyle()+","+f.getSize();
	}
	return s;
    }
    
    public static byte checkInterfaceFontnum(byte fontnum) throws ProtocolError {
	if (fontnum<TermFontNum || fontnum>ProvisoFontNum)
	    throw new ProtocolError("fontnum "+fontnum+" out of range");
	return fontnum;
    }

    private static FontMetrics[] interfaceMetrics;

    private static void initInterfaceMetrics() {
	if (interfaceMetrics==null) {
	    initInterfaceFonts();
	    interfaceMetrics = new FontMetrics[3];
	    JLabel l = new JLabel();
	    for (int i=TermFontNum; i<=ProvisoFontNum; i++) {
		interfaceMetrics[i] = l.getFontMetrics(interfaceFonts[i]);
	    }
	}
    }

    private static final Component dummyComponent = new Canvas();
    
    public static TextDimension measure(Font f, String s) {
	dummyComponent.setFont(f);
	return measure(dummyComponent, s);
    }

    public static TextDimension measure(Component c, String s) {
	FontMetrics m = c.getFontMetrics(c.getFont());
	return new TextDimension(m.stringWidth(s), m.getMaxAscent(), m.getMaxDescent());
    }

    public static TextDimension measure(String s, byte fontnum) {
	initInterfaceMetrics();
	if (DebugVars.measure_tracing)
	    Logger.log.println("measuring "+JapeUtils.enQuote(s)+"; ("+
			   interfaceMetrics[fontnum].stringWidth(s)+","+
			   interfaceMetrics[fontnum].getMaxAscent()+"["+
			   interfaceMetrics[fontnum].getAscent()+"],"+
			   interfaceMetrics[fontnum].getMaxDescent()+"["+
			   interfaceMetrics[fontnum].getDescent()+"]); "+
			   interfaceMetrics[fontnum].getLeading());
	return new TextDimension(interfaceMetrics[fontnum].stringWidth(s),
				 interfaceMetrics[fontnum].getMaxAscent(),
				 interfaceMetrics[fontnum].getMaxDescent());
    }

    public static String procrustes(int width, String s, String ellipsis, byte fontnum) {
	TextDimension se = measure(ellipsis, fontnum);
	
	/* find split by binary chop -- oh dear! */
	char[] cs = s.toCharArray();
	int i = 0, j = cs.length, w = width-se.width;
	
	while (i+1<j) {
	    int k = (i+j)/2;
	    int wk = charsWidth(cs, 0, k, fontnum);
	    if (wk<w) i=k;
	    else      j=k;
	}
	return (new String(cs, 0, i)+ellipsis);
    }
    
    public static int charsWidth(char[] cs, int off, int len, byte fontnum) {
	initInterfaceMetrics();
	return interfaceMetrics[fontnum].charsWidth(cs, off, len);
    }

    public static int stringWidth(Font f, String s) {
	dummyComponent.setFont(f);
	return stringWidth(dummyComponent, s);
    }

    public static int stringWidth(Component c, String s) {
	FontMetrics m = c.getFontMetrics(c.getFont());
	return m.stringWidth(s);
    }

    public static FontMetrics getFontMetrics(byte fontnum) {
	initInterfaceMetrics();
	return interfaceMetrics[fontnum];
    }

    public static Font getFont(byte fontnum) {
	initInterfaceFonts();
	return interfaceFonts[fontnum];
    }
}

