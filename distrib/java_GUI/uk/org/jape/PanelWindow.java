//
//  JapePanel.java
//  japeserver
//
//  Created by Richard Bornat on Wed Sep 11 2002.
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
//

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.BorderLayout;
import javax.swing.DefaultListModel;
import java.util.Enumeration;
import java.awt.FlowLayout;
import java.awt.Font;
import java.util.Hashtable;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.ListSelectionModel;
import java.util.Vector;

public class PanelWindow extends JapeWindow implements ActionListener {
    protected int kind;
    
    final DefaultListModel model;
    JList entries;
    JScrollPane scrollPane;
    JPanel buttonPanel;
    Vector entryv, cmdv; // matches the entries line for line
    Vector buttonv;
    
    public PanelWindow(String title, int kind) {
        super(title);
        this.kind = kind;
        model = new DefaultListModel();
        entries = new JList(model);
        entries.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        entries.setSelectedIndex(0);
        scrollPane = new JScrollPane(entries);
        entryv = new Vector();
        cmdv = new Vector();
        buttonv = new Vector();

        this.getContentPane().setLayout(new BorderLayout()); 
        this.getContentPane().add(scrollPane, BorderLayout.CENTER);
        
        buttonPanel = new JPanel();
        buttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 5));
        this.getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        
        if (kind==1) { // i.e. a ConjecturePanel
            // has default buttons
            Insert[] cmd;
            cmd = new Insert[1]; cmd[0] = new StringInsert("New... button pressed");
            addButton("New...", cmd);
            cmd = new Insert[2]; cmd[0] = new StringInsert("prove"); cmd[1] = new CommandInsert();
            addButton("Prove", cmd);
            cmd = new Insert[2]; cmd[0] = new StringInsert("showproof"); cmd[1] = new CommandInsert();
            addButton("Show Proof", cmd);
            // and double-click means "prove this one"
            MouseListener m = new MouseAdapter () {
                public void mouseClicked(MouseEvent e) {
                    if (e.getClickCount()==2) {
                        int index = entries.locationToIndex(e.getPoint());
                        Reply.sendCOMMAND("prove "+cmdv.get(index));
                    }
                }
            };
            entries.addMouseListener(m);
        }
        
        JapeFont.setComponentFont(entries);
        if (japeserver.onMacOS && !LocalSettings.quitMenuItemNeeded)
            setJMenuBar(new JMenuBar()); // by experiment, seems to be necessary before setVisible
    }
    
    protected void addEntry(String entry, String cmd) throws ProtocolError {
        // check for duplicate entries?
        if (model.indexOf(entry)!=-1)
            throw new ProtocolError("there is already an entry "+entry+" in panel "+title);
        model.addElement(entry);
        entryv.add(entry);
        cmdv.add(cmd);
    }
    
    protected static class PanelButton extends JButton {
        String label;
        Insert[] cmd;
        PanelButton(String label, Insert[] cmd) { super(label); this.label=label; this.cmd=cmd; }
    }
    
    protected void addButton(String label, Insert[] cmd) {
        // if there already is such a button, just change its cmd
        for (int i=0; i<buttonv.size(); i++) {
            PanelButton b = (PanelButton)buttonv.get(i);
            if (b.label.equals(label))
                b.cmd = cmd;
        }
        // otherwise a new one
        PanelButton button = new PanelButton(label, cmd);
        JapeFont.setComponentFont(button);
        button.setActionCommand(label);
        button.addActionListener(this);
        buttonPanel.add(button);
        buttonv.add(button);
    }
    
    // ActionListener interface (for menus)
    public void actionPerformed(ActionEvent newEvent) {
        String key = newEvent.getActionCommand();
        for (int i=0; i<buttonv.size(); i++)  {
            PanelButton b = (PanelButton)buttonv.get(i);
            if (b.label.equals(key)) {
                int index = entries.getSelectedIndex();
                String res="";
                for (int ci=0; ci<b.cmd.length; ci++) {
                    if (ci!=0) res+=" ";
                    res += b.cmd[ci] instanceof CommandInsert ? (String)cmdv.get(index)                     :
                           b.cmd[ci] instanceof LabelInsert   ? JapeFont.toAscii((String)entryv.get(index)) :
                           ((StringInsert)b.cmd[ci]).s;
                }
                Reply.sendCOMMAND(res);
                return;
            }
        }
    }

    public static PanelWindow spawn (String title, int kind) throws ProtocolError {
        JapeWindow w = findWindow(title);
        if (w!=null) {
            if (!(w instanceof PanelWindow))
                throw new ProtocolError ("NEWPANEL \""+title+"\" "+kind+" -- non-panel exists with this title");
            else {
                PanelWindow p = (PanelWindow) w;
                if (p.kind==kind)
                    return p;
                else
                    throw new ProtocolError ("NEWPANEL \""+title+"\" "+kind+" -- panel exists with kind "+p.kind);
            }
        }
	else // it ain't there
            return new PanelWindow(title, kind);
    }
    
    public static Enumeration panels() {
        Vector v = new Vector();
        for (Enumeration e = JapeWindow.windows(); e.hasMoreElements(); ) {
            Object o = e.nextElement();
            if (o instanceof PanelWindow) v.add(o);
        }
        return v.elements();
     }
    
    public static void makePanelsVisible() {
        for (Enumeration e = panels(); e.hasMoreElements(); ) {
            PanelWindow w = (PanelWindow)e.nextElement();
            w.setVisible(true);
        }
    }
    
    public static abstract class Insert { }
    public static class StringInsert extends Insert {
        String s;
        StringInsert(String s) { this.s=s; }
    }
    public static class LabelInsert extends Insert { }
    public static class CommandInsert extends Insert { }
    
    public static void panelEntry(String panel, String entry, String cmd) throws ProtocolError {
        JapeWindow w = findWindow(panel);
        if (w!=null) {
            if (!(w instanceof PanelWindow))
                throw new ProtocolError ("PANELENTRY \""+panel+"\" \""+entry+"\" ... non-panel exists with this title");
            else
                ((PanelWindow)w).addEntry(entry,cmd);
        }
	else // it ain't there
            throw new ProtocolError ("PANELENTRY \""+panel+"\" \""+entry+"\" ... before NEWPANEL");
    }
    
    public static void panelButton(String panel, String button, Insert[] cmd) throws ProtocolError {
        JapeWindow w = findWindow(panel);
        if (w!=null) {
            if (!(w instanceof PanelWindow))
                throw new ProtocolError ("PANELBUTTON \""+panel+"\" \""+button+"\" ... non-panel exists with this title");
            else
                ((PanelWindow)w).addButton(button,cmd);
        }
	else // it ain't there
            throw new ProtocolError ("PANELBUTTON \""+panel+"\" \""+button+"\" ... before NEWPANEL");
    }
}
