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

/* This is now basically functional, but things to do:
   // 1. Arrange a layout so that buttons are always visible, scroll pane doesn't sometimes disappear.
   2. Disable action of close button (and one day, if poss, grey out close button).
   3. Make panels float (maybe, and perhaps only on MacOS X)
   4. Make panels look different -- different title style? smaller title bar?
 */
 
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import javax.swing.DefaultListModel;
import java.util.Enumeration;
import java.util.Hashtable;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JViewport;
import java.awt.LayoutManager;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.ListSelectionModel;
import java.util.Vector;

public class PanelWindow extends JapeWindow implements ActionListener, DebugConstants {
    protected int kind;
    
    final DefaultListModel model;
    JList list;
    JScrollPane scrollPane;
    Vector entryv, cmdv; // matches the list line for line
    Vector buttonv;

    
    public PanelWindow(String title, int kind) {
        super(title);
        this.kind = kind;
        model = new DefaultListModel();
        list = new JList(model);
        list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        list.setSelectedIndex(0);
        scrollPane = new JScrollPane(list);
        entryv = new Vector();
        cmdv = new Vector();
        buttonv = new Vector();

        Container contentPane = getContentPane();

        contentPane.setLayout(new PanelWindowLayout()); 
        contentPane.add(scrollPane);
        
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
                        int index = list.locationToIndex(e.getPoint());
                        Reply.sendCOMMAND("prove "+cmdv.get(index));
                    }
                }
            };
            list.addMouseListener(m);
        }

        JapeFont.setComponentFont(JapeFont.PANELENTRY, list);
        if (LocalSettings.panelWindowMenus)
            setBar(); // by experiment, seems to be necessary before setVisible
        
        pack(); // does that call buttonPanel.pack()??
    }

    protected void addEntry(String entry, String cmd) {
        int i = model.indexOf(entry);
        if (i==-1) {
            model.addElement(entry);
            entryv.add(entry);
            cmdv.add(cmd);
        }
        else {
            cmdv.setElementAt(cmd, i);
        }
    }
    
    protected class PanelButton extends JButton {
        String label;
        Insert[] cmd;
        PanelButton(String label, Insert[] cmd) {
            super(label);
            this.label=label; this.cmd=cmd;
            setActionCommand(label);
            addActionListener(PanelWindow.this);
            JapeFont.setComponentFont(JapeFont.PANELBUTTON, this);
        }
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
        getContentPane().add(button);
        buttonv.add(button);
    }

    // ActionListener interface for buttons

    public void actionPerformed(ActionEvent newEvent) {
        String key = newEvent.getActionCommand();
        for (int i=0; i<buttonv.size(); i++)  {
            PanelButton b = (PanelButton)buttonv.get(i);
            if (b.label.equals(key)) {
                int index = list.getSelectedIndex();
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

    /**********************************************************************************************

        Layout

     **********************************************************************************************/

    protected class PanelWindowLayout implements LayoutManager {

        /* Called by the Container add methods. Layout managers that don't associate
        * strings with their components generally do nothing in this method.
        */
        public void addLayoutComponent(String s, Component c) { }

        /* Called by the Container remove and removeAll methods. Many layout managers
        * do nothing in this method, relying instead on querying the container for its
        * components, using the Container getComponents method.
        */
        public void removeLayoutComponent(Component c) { }

        /* Called by the Container getPreferredSize method, which is itself called under
        * a variety of circumstances. This method should calculate and return the ideal
        * size of the container, assuming that the components it contains will be at or
        * above their preferred sizes. This method must take into account the container's
        * internal borders, which are returned by the getInsets method.
        */

        private int buttonpanelheight, buttonpanelwidth;
        private int leading(Dimension d) { return d.height/5; } // empirical
        
        private void preferredButtonPanelSize(Container pane) {
            buttonpanelwidth = 0;
            // three buttons across

            if (buttonv.size()==0) {
                if (panellayout_tracing)
                    System.err.println("preferredButtonPanelSize no buttons");
                PanelButton b = new PanelButton("Prove",  new Insert[0]);
                Dimension d = b.getPreferredSize();
                int buttonleading = leading(d);
                buttonpanelheight = 0;
                buttonpanelwidth = d.width*3+2*buttonleading;
            }
            else {
                Dimension d = 
                int buttonleading = ((PanelButton)buttonv.get(0)).getLeading();
                int buttonheight = ((PanelButton)buttonv.get(0)).getHeight();
                int threewidth = 0; // shut up compiler
                
                buttonpanelheight = buttonleading;
                
                for (int i=0; i<buttonv.size(); i++) {
                    PanelButton button = (PanelButton)buttonv.get(i);

                    if (panellayout_tracing)
                        System.err.println("preferredButtonPanelSize "+i+": "+button);

                    if (i%3==0) {
                        buttonpanelheight += buttonleading+buttonheight;
                        threewidth = button.getWidth();
                    }
                    else
                        threewidth += buttonleading+button.getWidth();

                    if ((i+1)%3==0)
                        buttonpanelwidth = Math.max(threewidth, buttonpanelwidth);
                }

                if (buttonv.size()<3)
                    buttonpanelwidth = (buttonpanelwidth-(buttonv.size()-1)*buttonleading)*3/buttonv.size()+
                        2*buttonleading;
            }
            if (panellayout_tracing)
                System.err.println("preferredButtonPanelSize = "+buttonpanelwidth+","+buttonpanelheight);
        }
        
        public Dimension preferredLayoutSize(Container pane) {
            preferredButtonPanelSize(pane);
            JViewport port = scrollPane.getViewport();
            return new Dimension(Math.max(buttonpanelwidth, port.getX()+list.getWidth()),
                                 buttonpanelwidth+buttonpanelheight);
        }

        /* Called by the Container getMinimumSize method, which is itself called under
        * a variety of circumstances. This method should calculate and return the minimum
        * size of the container, assuming that the components it contains will be at or
        * above their minimum sizes. This method must take into account the container's
        * internal borders, which are returned by the getInsets method.
        */
        
        public Dimension minimumLayoutSize(Container pane) {
            preferredButtonPanelSize(pane);
            return new Dimension(buttonpanelwidth, buttonpanelheight+buttonpanelwidth); 
        }

        /* Called when the container is first displayed, and each time its size changes.
        * A layout manager's layoutContainer method doesn't actually draw components.
        * It simply invokes each component's resize, move, and reshape methods to set
        * the component's size and position. This method must take into account the
        * container's internal borders, which are returned by the getInsets method.
        * You can't assume that the preferredLayoutSize or minimumLayoutSize method
        * will be called before layoutContainer is called.
        */
        public void layoutContainer(Container pane) {
            PanelButton[] buttons = (PanelButton[])buttonv.toArray(new PanelButton[buttonv.size()]);
            if (buttons.length==0)
                scrollPane.setBounds(pane.getBounds());
            else {
                // first layout the buttons
                int[] xs = new int[buttons.length], ys = new int[buttons.length];
                int leading = buttons[0].getLeading(), bh = buttons[0].getHeight();
                int width = getWidth(), height=getHeight();
                int y = height-leading-bh, packedwidth = 0;
                for (int i=0; i<buttons.length; i++) {
                    int bw = buttons[i].getWidth();
                    if (packedwidth==0 || packedwidth+leading+bw<=width) {
                        for (int j=0; j<i; j++)
                            if (ys[j]==y) xs[j]-=bw+leading;
                    }
                    else {
                        for (int j=0; j<i; j++) ys[j]-=bh+leading;
                        packedwidth = 0;
                    }
                    xs[i]=width-bw; ys[i]=y;
                    if (packedwidth==0)
                        packedwidth = bw;
                    else
                        packedwidth += bw+leading;
                }
                // give the buttons their due
                for (int i=0; i<buttons.length; i++)
                    buttons[i].setLocation(xs[i],ys[i]);
                // give the scroll pane the rest of the height
                scrollPane.setBounds(0, 0, width, Math.max(0, ys[0]-leading));
            }
        }
    }

    protected static PanelWindow findPanelWindow(String title, boolean musthave) throws ProtocolError {
        JapeWindow w = findWindow(title);
        if (w!=null) {
            if (!(w instanceof PanelWindow))
                throw new ProtocolError ("non-panel exists with this title");
        }
        else 
        if (musthave)
            throw new ProtocolError("before NEWPANEL");
            
        return (PanelWindow)w;
    }
    
    public static PanelWindow spawn (String title, int kind) throws ProtocolError {
        PanelWindow p = findPanelWindow(title, false);
        if (p!=null) {
            if (p.kind==kind)
                return p;
            else
                throw new ProtocolError ("panel exists with different kind");
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
        findPanelWindow(panel, true).addEntry(entry,cmd);
    }
    
    public static void panelButton(String panel, String button, Insert[] cmd) throws ProtocolError {
        findPanelWindow(panel,true).addButton(button,cmd);
    }
}
