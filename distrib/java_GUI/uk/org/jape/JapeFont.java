/* 
    $Id$

    Copyright � 2003-4 Richard Bornat & Bernard Sufrin
     
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
import java.awt.Graphics;
import java.awt.GraphicsEnvironment;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import java.util.HashMap;
import java.util.Vector;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

public class JapeFont implements DebugConstants, ProtocolConstants {

    private static Font deriveFont(Font f, int style, int size) {
        return f.deriveFont(style, (float)size);
    }

    public static final int MENUENTRY   = 100,
                            DIALOGLABEL = 201,
                            PROOFPANEL  = 300,
                            PANELENTRY  = 301,
                            PANELBUTTON = 302,
                            LOGWINDOW   = 400,
                            TEXTINPUT   = 500;

    public static byte
        FormulaFontSize     = Preferences.getProp("font.formula.size",
                                                  LocalSettings.FormulaFontSize),
        NonFormulaFontSize  = Preferences.getProp("font.nonformula.size",
                                                  LocalSettings.NonFormulaFontSize),
        ReasonFontSize      = Preferences.getProp("font.reason.size",      NonFormulaFontSize),
        ProvisoFontSize     = Preferences.getProp("font.proviso.size",     NonFormulaFontSize),
        PanelButtonFontSize = Preferences.getProp("font.panelbutton.size", NonFormulaFontSize),
        PanelEntryFontSize  = Preferences.getProp("font.panelentry.size",  NonFormulaFontSize),
        LogWindowFontSize   = Preferences.getProp("font.logwindow.size",   NonFormulaFontSize);

    public static final String
        FontName =
            Preferences.getProp("fonts.family", Jape.onMacOS ? "LucidaSansUnicode" : "SansSerif");

    private static final Font baseFont = new Font(FontName, Font.PLAIN, 1);

    public static final int[] normalsizes = { 9, 10, 11, 12, 13, 14, 18, 24, 36, 48, 72 };

    private static Vector initsizes(int size) {
        Vector sizes = new Vector(normalsizes.length);
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

    private static class SI {
        final JLabel label; final int size;
        JComboBox comboBox;
        SI(String label, int size) {
            this.label = new JLabel(label); this.size = size;
            Vector sizes = initsizes(size);
            comboBox = new JComboBox(sizes);
            for (int j=0; j<sizes.size(); j++)
                if (((Integer)sizes.get(j)).intValue()==size) {
                    comboBox.setSelectedIndex(j); break;
                }
        }
    }
        
    public static void runFontSizesDialog() {
        String [] buttons = { "OK", "Cancel" };
        SI [] entries = {
            new SI("Formula font size"       , FormulaFontSize   ), // 0
            new SI("Reason/Proviso font size", ReasonFontSize    ), // 1
            new SI("Panel font size"         , PanelEntryFontSize), // 2
            new SI("Log window font size"    , LogWindowFontSize )  // 3
        };
        JPanel panel = new JPanel();
        GridBagLayout gridbag = new GridBagLayout();
        panel.setLayout(gridbag);

        GridBagConstraints labelconstraints = new GridBagConstraints(),
                           comboconstraints = new GridBagConstraints();

        labelconstraints.gridwidth = GridBagConstraints.RELATIVE; //next-to-last
        labelconstraints.fill = GridBagConstraints.NONE;      //reset to default
        labelconstraints.anchor = GridBagConstraints.EAST;
        labelconstraints.weightx = 0.0;                       //reset to default

        comboconstraints.gridwidth = GridBagConstraints.REMAINDER;     //end row
        comboconstraints.fill = GridBagConstraints.NONE;
        comboconstraints.weightx = 1.0;
        comboconstraints.anchor = GridBagConstraints.WEST;
        comboconstraints.insets = new Insets(0, 5, 5, 0);

        for (int i=0; i<entries.length; i++) {
            gridbag.setConstraints(entries[i].label, labelconstraints);
            panel.add(entries[i].label);
            gridbag.setConstraints(entries[i].comboBox, comboconstraints);
            panel.add(entries[i].comboBox);
        }
        
        int reply = JOptionPane.showOptionDialog(JapeWindow.getTopWindow(), panel,
                                                 "Font sizes", 0,
                                                 JOptionPane.PLAIN_MESSAGE,
                                                 null, buttons, buttons[0]);
        if (reply==0) {
            boolean interfaceChanged = false;
            for (int i=0; i<entries.length; i++) {
                int selectedvalue = ((Integer)entries[i].comboBox.getSelectedItem()).intValue();
                if (selectedvalue!=entries[i].size) {
                    switch (i) {
                        case 0:
                            FormulaFontSize = (byte)selectedvalue;
                            interfaceChanged = true; break;
                        case 1:
                            ReasonFontSize = ProvisoFontSize = (byte)selectedvalue;
                            interfaceChanged = true; break;
                        case 2:
                            PanelButtonFontSize = PanelEntryFontSize = (byte)selectedvalue;
                            PanelWindowData.font_reset(); break;
                        case 3:
                            LogWindowFontSize = (byte)selectedvalue;
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
        if (!FontName.equals(f.getName()) || f.getSize()!=size) {
            if (size>mimics.length) {
                Font mimics1[] = new Font[size+1];
                for (int i = 0; i<mimics.length; i++)
                    mimics1[i] = mimics[i];
                for (int i = mimics.length; i<mimics1.length; i++)
                    mimics1[i] = null;
                mimics = mimics1;
            }
            if (mimics[size]==null)
                mimics[size] = deriveFont(baseFont, f.getStyle(), size);
            c.setFont(mimics[size]);
        }
    }

    public static byte[] interfaceFontSizes;
    private static Font[] interfaceFonts;

    private static void initInterfaceFonts() {
        if (interfaceFonts==null) {
            codecDone = true;

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
                Logger.log.println("base font "+baseFont);

            setInterfaceFonts(baseFont);
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

    private static boolean codecDone;
    
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
        if (measure_debug)
            Logger.log.println("measuring \""+s+"\"; ("+
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

