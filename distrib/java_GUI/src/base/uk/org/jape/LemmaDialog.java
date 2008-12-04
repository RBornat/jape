/* 
    $Id$
     
     Copyright © 2003-8 Richard Bornat and Bernard Sufrin
     
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

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.Spring;
import javax.swing.SpringLayout;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;

public class LemmaDialog {
    
    private static void addLabel(Vector<JComponent> items, String label) {
	JLabel[] ms = ChoiceDialog.wrap(label);
	for (int i=0; i<ms.length; i++)
	    items.add(ms[i]);
    }
    
    @SuppressWarnings("serial")
    public static String runLemmaDialog(String druleString, String thmString,
				      String[] panels, String[] provisos) 
		throws ProtocolError {
	Vector<JComponent> items = new Vector<JComponent>();
	int labelcount = 0;
	
	/* *********************** theorem or derived rule **********************************/
	
	JRadioButton druleButton = null, thmButton = null;
	
	if (druleString.equals(" "))
	    addLabel(items, "Lemma is "+ singleLine(thmString));
	else {
	    labelcount++;
	    addLabel(items, labelcount+
		     ". Say whether to make a derived rule or a theorem (Jape recommends derived rule):");
	    
	    druleButton = new JRadioButton("Derived Rule");
	    druleButton.setActionCommand("Derived Rule");
	    druleButton.setSelected(true);
	    
	    thmButton = new JRadioButton("Theorem");
	    thmButton.setActionCommand("Derived Rule");
	    
	    ButtonGroup kindGroup = new ButtonGroup();
	    kindGroup.add(druleButton);
	    kindGroup.add(thmButton);
	    
	    ActionListener kindListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    // nothing yet
		}
	    };
	    
	    druleButton.addActionListener(kindListener);
	    thmButton.addActionListener(kindListener);
	    
	    /* now we have to work out whether to display the rule and theorem as a single line
	       or a multi-line panel
	     */
	    
	    JPanel kindPanel = new JPanel(new SpringLayout());
	    kindPanel.add(druleButton);
	    kindPanel.add(foldThm(druleString));
	    kindPanel.add(thmButton);
	    kindPanel.add(foldThm(thmString));
	    
	    makeCompactGrid(kindPanel,
			    2, 2, //rows, cols
			    6, 6,        //initX, initY
			    6, 6);       //xPad, yPad
	    
	    items.add(kindPanel);
	}
	
	/* ********************************* panel name ************************************ */
	
	labelcount++;
	addLabel(items, labelcount+
		 ". Select a conjecture panel to hold the lemma, or type the name of a new panel "+
		 "\n(the OK button is disabled until you've done this):");
	
	String[] panelshow = new String[panels.length+1];
	panelshow[0] = "";
	System.arraycopy(panels, 0, panelshow, 1, panels.length);
	
	final JComboBox panelList = new JComboBox(panelshow);
	panelList.setEditable(true);
	
	items.add(panelList);
	
	/* ********************************* lemma name ************************************ */
	
	labelcount++;
	addLabel(items, labelcount+". (optional) Type a name for the lemma:");
	
	final JTextField textField = new JTextField();
	JapeFont.setComponentFont(textField, JapeFont.TEXTINPUT);
	
	items.add(textField);
	
	/* ******************************** provisos ******************************** */
	
	JCheckBox[] provisochecks = new JCheckBox[provisos.length];
	
	if (provisos.length!=0) {
	    labelcount++;
	    addLabel(items, labelcount+
		     ". Tick the provisos you want to inherit from the parent theorem:");
	    
	    JPanel provisoPanel = new JPanel(new FlowLayout(FlowLayout.LEFT){
		public Dimension preferredLayoutSize(Container target) {
		    synchronized (target.getTreeLock()) {
			Dimension dim = new Dimension(0, 0);
			int rowwidth=0, rowheight=0, rowlim=Jape.screenBounds.width*6/10;
			int nmembers = target.getComponentCount();
			int hgap = getHgap(), vgap = getVgap();
			
			for (int i = 0 ; i < nmembers ; i++) {
			    Component m = target.getComponent(i);
			    if (m.isVisible()) {
				Dimension d = m.getPreferredSize();
				if (rowwidth==0 || d.width+hgap+rowwidth<=rowlim) {
				    rowheight = Math.max(rowheight, d.height);
				    if (rowwidth!=0) rowwidth+=hgap;
				    rowwidth += d.width;
				}
				else {
				    if (dim.height!=0) dim.height+=vgap;
				    dim.height+=rowheight;
				    dim.width=Math.max(rowwidth,dim.width);
				    rowwidth=d.width; rowheight=d.height;
				}
			    }
			}

			if (dim.height!=0) dim.height+=vgap;
			dim.height+=rowheight;
			dim.width=Math.max(rowwidth, dim.width);

			Insets insets = target.getInsets();
			dim.width += insets.left + insets.right + hgap*2;
			dim.height += insets.top + insets.bottom + vgap*2;
			return dim;
		    }
		}
	    });
	    
	    int i;
	    
	    for (i=0; i<provisos.length; i++) {
		JCheckBox box = new JCheckBox(provisos[i]);
		provisochecks[i] = box;
		box.setSelected(true);
		box.setActionCommand(provisos[i]);
		// Logger.log.println(provisos[i]+" = "+box.getWidth()+","+box.getHeight());
		provisoPanel.add(box);
	    }
	    
	    items.add(provisoPanel);
	}
	
	/* ********************************* run dialog ******************************* */
	
	String [] options = { "OK", "Cancel" };
	final JOptionPane pane = new JOptionPane(items.toArray(), JOptionPane.PLAIN_MESSAGE, 0,
					   null, options, options[0]);
	final JDialog dialog = pane.createDialog(JapeWindow.getTopWindow(), "Make a lemma");
	
	JapeMenu.setDialogMenuBar(JapeMenu.TEXTDIALOGWINDOW_BAR|JapeMenu.DIALOGWINDOW_BAR, 
				  dialog, "Make Lemma dialog");

	Component panelBlank = panelList.getEditor().getEditorComponent();
	if (panelBlank instanceof JTextComponent) {
	    
	    ((JTextComponent)panelBlank).getDocument().addDocumentListener(new DocumentListener(){
		private String string(DocumentEvent e) {
		    Document d = e.getDocument();
		    try {
			return d.getText(0,d.getLength());
		    } catch (Exception exn) {
			return "";
		    }
		}
		public void insertUpdate(DocumentEvent e) {
		    enableButtons(string(e), dialog);
		}
		public void removeUpdate(DocumentEvent e) {
		    enableButtons(string(e), dialog);
		}
		public void changedUpdate(DocumentEvent e) {
		    return;
		}
	    });

	    panelList.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		    enableButtons(panelList.getSelectedItem().toString(), dialog);
		}
	    });
	    enableButtons("",dialog);
	    dialog.setVisible(true);
	}
	else
	    while (true) {
		dialog.setVisible(true);
		Object selectedValue = pane.getValue();
		String panelName = panelList.getSelectedItem().toString();
		if (selectedValue!=null && options[0].equals(selectedValue) &&
		    panelName.equals(""))
		    Alert.showAlert("You must give a panel name (or press Cancel). Try again!\n\n"+
				    "(If I knew how to grey out the OK button till there's a panel name, I would do it.)");
		else
		    break;
	    }

	Object selectedValue = pane.getValue();
	/* int res = JOptionPane.showConfirmDialog(null, items.toArray(), "Make a lemma", 
						JOptionPane.OK_CANCEL_OPTION);
	 */
	
	String lemmaName = textField.getText();
	boolean isThm = thmButton==null || thmButton.isSelected();
	String panelName = panelList.getSelectedItem().toString();
	String lemmaProvisos = harvestProvisos(provisochecks, 0);
	
	boolean ok = selectedValue!=null && options[0].equals(selectedValue);
	
	JapeMenu.undoDialogMenuBar();
	dialog.dispose();
	
	return ok ? ((isThm ? 1 : 0) + Reply.stringSep +
		     (lemmaName==null ? "" : lemmaName) + Reply.stringSep +
		     panelName + Reply.stringSep +
		     lemmaProvisos)
		  : "";
    }
    
    private static String harvestProvisos(JCheckBox[] provisochecks, int i) {
	if (i==provisochecks.length)
	    return ""; // this gives endless trouble at the other end ...
	else {
	    String r = harvestProvisos(provisochecks, i+1);
	    return provisochecks[i].isSelected() 		  ? 
			provisochecks[i].getActionCommand()+
                        (r.equals("") ? "" : (Reply.stringSep+r)) : r;
	}
    }
    
    private static void enableButtons(String panelName, JDialog dialog) {
	int i, n=panelName.length();
	for (i=0; i<n && panelName.charAt(i)==' '; i++) ;
	boolean set = i<n;
	
	JButton ok = findButton(dialog, "OK");
	JButton cancel = findButton(dialog, "Cancel");
	
	if (ok!=null && cancel!=null) {
	    if (set) {
		ok.setEnabled(true);
		ok.setSelected(true);
		cancel.setSelected(false);
	    }
	    else {
		ok.setSelected(false);
		ok.setEnabled(false);
		cancel.setSelected(true);
	    }
	    ok.repaint(); cancel.repaint();
	}
    }
    
    private static JButton findButton(Object o, String title) {
	if (o instanceof JButton && ((JButton)o).getText().equals(title))
	    return (JButton) o;
	else
	if (o instanceof Container) {
	    Container c = (Container) o;
	    int i, n = c.getComponentCount();
	    for (i=0; i<n; i++) {
		JButton res;
		if ((res=findButton(c.getComponent(i), title))!=null)
		    return res;
	    }
	}
	return null;
    }
    
    static Component foldThm(String s) {
	JLabel line = Alert.makeLabel(singleLine(s));
	TextDimension m = JapeFont.measure(line, s);
	if (m.width>Jape.screenBounds.width*3/10) {
	    JLabel[] lines = ChoiceDialog.wrap(s, Jape.screenBounds.width*3/10);
	    JPanel panel = new JPanel(new GridLayout(0,1));
	    for (int i=0; i<lines.length; i++)
		panel.add(lines[i]);
	    return panel;
	}
	else
	    return line;
    }
    
    static String singleLine(String s) {
	/* replace nl by space */
	char[] cs = s.toCharArray();
	for (int i=0; i<cs.length; i++)
	    if (cs[i]=='\n') cs[i]=' ';
	return new String(cs, 0, cs.length);
    }
    
    /* This stuff taken from Sun's 1.4 documentation. What a palaver! */
    
    /* Used by makeCompactGrid. */
    private static SpringLayout.Constraints getConstraintsForCell(
								  int row, int col,
								  Container parent,
								  int cols) {
        SpringLayout layout = (SpringLayout) parent.getLayout();
        Component c = parent.getComponent(row * cols + col);
        return layout.getConstraints(c);
    }
    
    /**
    * Aligns the first <code>rows</code> * <code>cols</code>
     * components of <code>parent</code> in
     * a grid. Each component in a column is as wide as the maximum
     * preferred width of the components in that column;
     * height is similarly determined for each row.
     * The parent is made just big enough to fit them all.
     *
     * @param rows number of rows
     * @param cols number of columns
     * @param initialX x location to start the grid at
     * @param initialY y location to start the grid at
     * @param xPad x padding between cells
     * @param yPad y padding between cells
     */
    public static void makeCompactGrid(Container parent,
                                       int rows, int cols,
                                       int initialX, int initialY,
                                       int xPad, int yPad) {
        SpringLayout layout;
        try {
            layout = (SpringLayout)parent.getLayout();
        } catch (ClassCastException exc) {
            System.err.println("The first argument to makeCompactGrid must use SpringLayout.");
            return;
        }
	
        //Align all cells in each column and make them the same width.
        Spring x = Spring.constant(initialX);
        for (int c = 0; c < cols; c++) {
            Spring width = Spring.constant(0);
            for (int r = 0; r < rows; r++) {
                width = Spring.max(width,
                                   getConstraintsForCell(r, c, parent, cols).
				   getWidth());
            }
            for (int r = 0; r < rows; r++) {
                SpringLayout.Constraints constraints =
		getConstraintsForCell(r, c, parent, cols);
                constraints.setX(x);
                constraints.setWidth(width);
            }
            x = Spring.sum(x, Spring.sum(width, Spring.constant(xPad)));
        }
	
        //Align all cells in each row and make them the same height.
        Spring y = Spring.constant(initialY);
        for (int r = 0; r < rows; r++) {
            Spring height = Spring.constant(0);
            for (int c = 0; c < cols; c++) {
                height = Spring.max(height,
                                    getConstraintsForCell(r, c, parent, cols).
				    getHeight());
            }
            for (int c = 0; c < cols; c++) {
                SpringLayout.Constraints constraints =
		getConstraintsForCell(r, c, parent, cols);
                constraints.setY(y);
                constraints.setHeight(height);
            }
            y = Spring.sum(y, Spring.sum(height, Spring.constant(yPad)));
        }
	
        //Set the parent's size.
        SpringLayout.Constraints pCons = layout.getConstraints(parent);
        pCons.setConstraint(SpringLayout.SOUTH, y);
        pCons.setConstraint(SpringLayout.EAST, x);
    }
    
}
