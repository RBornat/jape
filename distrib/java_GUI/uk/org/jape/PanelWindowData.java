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

/* We mustn't create panel windows -- which means things like calculating
   preferred size and such -- until we have all the data we need.
 */
 
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import javax.swing.DefaultListModel;
import java.awt.Dimension;
import java.util.Enumeration;
import java.awt.Graphics;
import java.util.Hashtable;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JViewport;
import java.awt.LayoutManager2;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.Rectangle;
import java.util.Vector;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

public class PanelWindowData implements DebugConstants, ProtocolConstants {
    protected String title;
    protected int kind;
    PanelWindow window;
    
    Vector entryv, cmdv; // matches the list line for line
    Vector buttonv;
    
    public PanelWindowData(String title, int kind) {
        this.title = title; this.kind = kind;
        window = null;

        entryv = new Vector();
        cmdv = new Vector();
        buttonv = new Vector();

        if (kind==ConjecturePanelKind) { 
            // has default buttons
            addButton("New...", new Insert[] { new StringInsert("New... button pressed") });
            addButton("Prove", new Insert[] { new StringInsert("prove"), new CommandInsert() });
            addButton("Show Proof", new Insert[] { new StringInsert("showproof"), new CommandInsert() });
        }
    }

    protected void addEntry(String entry, String cmd) {
        int i = entryv.indexOf(entry);
        if (i==-1) {
            if (panellist_tracing)
                System.err.println("panel \""+title+"\" adding entry \""+entry+"\", cmd \""+cmd+"\"");
            entryv.add(entry);
            cmdv.add(cmd);
        }
        else {
            if (panellist_tracing)
                System.err.println("panel \""+title+"\" setting entry \""+entry+"\" to \""+cmd+"\"");
            cmdv.setElementAt(cmd, i);
        }
        if (window!=null)
            window.addEntry(entry);
    }

    protected void markEntry(String cmd, boolean proved, boolean disproved) throws ProtocolError {
        if (window==null)
            throw new ProtocolError("before MAKEPANELSVISIBLE");
        else {
            if (panellist_tracing)
                System.err.println("panel \""+title+"\" marking cmd \""+cmd+"\" "+proved+","+disproved);
            int i = cmdv.indexOf(cmd);
            if (i==-1)
                throw new ProtocolError("no such command");
            else
                window.markEntry(i, proved, disproved);
        }
    }

    public static abstract class Insert { }
    public static class StringInsert extends Insert {
        String s;
        StringInsert(String s) { this.s=s; }
    }
    public static class LabelInsert extends Insert { }
    public static class CommandInsert extends Insert { }

    protected static class PanelButton extends JButton {
        String label;
        Insert[] cmd;
        PanelButton(String label, Insert[] cmd) {
            super(label);
            this.label=label; this.cmd=cmd;
            setActionCommand(label);
            JapeFont.setComponentFont(this, JapeFont.PANELBUTTON);
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
        buttonv.add(button);
        if (window!=null) {
            if (panellayout_tracing)
                System.err.println("late PanelButton "+label);
            window.addButton(button); // I hope it never does ...
        }
    }

    public void closeWindow() {
        if (window!=null) {
            JapeWindow.closeWindow(window.title);
            window = null;
        }
    }

    public void emptyPanel() {
        window.setVisible(false);
        window.model.removeAllElements();
        Component[] cs = window.getContentPane().getComponents();
        for (int i=0; i<cs.length; i++)
            if (cs[i] instanceof PanelButton)
                window.getContentPane().remove(cs[i]);
        entryv.removeAllElements(); cmdv.removeAllElements(); buttonv.removeAllElements();
    }
    
    /* This is now basically functional, but things to do:
       1. Disable action of close button (and one day, if poss, grey out close button).
       2. Make panels float (maybe, and perhaps only on MacOS X)
       3. Make panels look different -- different title style? smaller title bar?
     */
 
    public class PanelWindow extends JapeWindow implements ActionListener {

        private final DefaultListModel model;
        private JList list;
        private JScrollPane scrollPane;

        private final int prefixw;
        
        public PanelWindow() {
            super(PanelWindowData.this.title);
    
            setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
            addWindowListener(new WindowAdapter() {
                public void windowClosing(WindowEvent e) {
                    Alert.showAlert(PanelWindow.this, Alert.Info,
                                    new String[] {
                                        "You can't close a Jape panel: it's needed!",
                                        "(and if only I knew how to grey out the close button ...)"
                                    });
                }
            });
    
            Container contentPane = getContentPane();
            contentPane.setLayout(new PanelWindowLayout());
    
            model = new DefaultListModel();
            list = new JList(model);
            list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
            list.setSelectedIndex(0);
            JapeFont.setComponentFont(list, JapeFont.PANELENTRY);
            list.setCellRenderer(new Renderer());

            prefixw = JapeFont.stringWidth(list, "YN ");
            
            for (int i=0; i<entryv.size(); i++)
                addEntry((String)entryv.get(i));
            
            if (kind==ConjecturePanelKind) { 
                // double-click means "prove this one"
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
    
            scrollPane = new JScrollPane(list, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
                                               JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
            contentPane.add(scrollPane);
    
            for (int i=0; i<buttonv.size(); i++) {
                addButton((PanelButton)buttonv.get(i));
            }
            
            if (LocalSettings.panelWindowMenus)
                setBar(); // by experiment, seems to be necessary before setVisible

            setSize(getPreferredSize());
            setLocation(nextPos());
            
            pack(); // necessary??
        }

        public void resetPanelLayout() {
            Container contentPane = getContentPane();
            contentPane.getLayout().layoutContainer(contentPane);
        }
        
        public boolean equals(Object o) {
            return o instanceof PanelWindow ? ((PanelWindow)o).title.equals(this.title) :
                                              super.equals(o);
        }

        protected class Entry extends Component {
            private String s;
            public String prefix;
            private final TextDimension td;
            private final Dimension size;
            public boolean selected, focussed;
            public Color innerBackground = Color.white;
            public Entry(String s) {
                super(); this.s = s; prefix = null; this.td = JapeFont.measure(list, s);
                this.size = new Dimension(prefixw+td.width, td.ascent+td.descent);
                setFont(list.getFont());
            }
            public Dimension getPreferredSize() {
                return size;
            }
            public void paint(Graphics g) {
                if (paint_tracing)
                    System.err.println("painting PanelWindow.Entry");
                g.setColor(getBackground()); g.fillRect(0, 0, getWidth(), getHeight());
                if (selected && !focussed) {
                    g.setColor(innerBackground); g.fillRect(2, 2, getWidth()-4, getHeight()-4);
                }
                g.setColor(getForeground()); g.setFont(getFont());
                if (prefix!=null) g.drawString(prefix, 0, td.ascent);
                g.drawString(s, prefixw, td.ascent);
            }
        }
        
        protected void addEntry(String entry) {
            model.addElement(new Entry(entry));
        }

        protected void markEntry(int i, boolean proved, boolean disproved) {
            Entry e = (Entry)model.elementAt(i);
            e.prefix = proved ? LocalSettings.tick+(disproved ? LocalSettings.cross : "") :
                       disproved ? " "+LocalSettings.cross :
                       null;
        }

        class Renderer implements ListCellRenderer {
            public Component getListCellRendererComponent(JList list, Object value, int index,
                                                          boolean isSelected, boolean cellHasFocus) {
                Entry e = (Entry)value;
                if (isSelected) {
                    e.setBackground(list.getSelectionBackground());
                    e.setForeground(list.getSelectionForeground());
                }
                else {
                    e.setBackground(list.getBackground());
                    e.setForeground(list.getForeground());
                }
                e.selected = isSelected; e.focussed = cellHasFocus;
                if (isSelected && !cellHasFocus)
                    e.innerBackground = list.getBackground();
                return e;
            }
        }
        
        protected void addButton(PanelButton button) {
            getContentPane().add(button);
            button.addActionListener(this);
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
                        res += b.cmd[ci] instanceof CommandInsert ? (String)cmdv.get(index)      :
                               b.cmd[ci] instanceof LabelInsert   ? (String)entryv.get(index)    :
                                                                    ((StringInsert)b.cmd[ci]).s;
                    }
                    Reply.sendCOMMAND(res);
                    return;
                }
            }
        }
    
        // it seems that this method is never called, even when the window is maximised.
        
        public Dimension getMaximumSize() {
            if (panellayout_tracing)
                System.err.println("getMaximumSize called");
            return super.getMaximumSize();
        }
        
        /**********************************************************************************************
    
            Layout
    
        **********************************************************************************************/
    
        // I made this a LayoutManager2, because that has a maximumLayoutSize method, in the hope that this
        // would affect window maximisation.  It doesn't: the method never seems to be called (in Javish
        // one should say messaged).
        
        protected class PanelWindowLayout implements LayoutManager2 {
    
            /* Called by the Container add methods. Layout managers that don't associate
            * strings with their components generally do nothing in this method.
            */
            public void addLayoutComponent(String s, Component c) { }
    
            /* Adds the specified component to the layout, using the specified constraint object. */
            public void addLayoutComponent(Component comp, Object constraints) { }
    
            /* Returns the alignment along the x axis. This specifies how the component would like
            * to be aligned relative to other components. The value should be a number between 0
            * and 1 where 0 represents alignment along the origin, 1 is aligned the furthest away
            * from the origin, 0.5 is centered, etc.
            */
            public float getLayoutAlignmentX(Container pane) { return (float)0; } // why not?
    
            /* Returns the alignment along the y axis. See above */
            public float getLayoutAlignmentY(Container pane) { return (float)0; } // why not?
    
            /* Invalidates the layout, indicating that if the layout manager has cached information
            * it should be discarded.
            */
            public void invalidateLayout(Container pane) { } // we don't cache
    
            /* Returns the maximum size of this component. */
            public Dimension maximumLayoutSize(Container pane) {
                // crude, for now
                JViewport port = scrollPane.getViewport();
                int width = port.getX()+list.getWidth(), height = port.getY()+list.getHeight();
                if (buttonv.size()!=0) {
                    Dimension d = ((PanelButton)buttonv.get(0)).getPreferredSize();
                    int buttonleading = leading(d);
                    height += d.height+2*buttonleading;
                    int buttonwidth = (buttonv.size()-1)*buttonleading;
                    for (int i=0; i<buttonv.size(); i++)
                        buttonwidth+=((PanelButton)buttonv.get(i)).getPreferredSize().width;
                    width = Math.max(width, buttonwidth);
                }
                if (panellayout_tracing)
                    System.err.println("maximumLayoutSize returns "+width+","+height);
                return new Dimension(width, height);
            }
                    
            /* Called by the Container remove and removeAll methods. Many layout managers
            * do nothing in this method, relying instead on querying the container for its
            * components, using the Container getComponents method.
            */
            public void removeLayoutComponent(Component c) { }
    
            private int buttonpanelheight, buttonpanelwidth;
            private int leading(Dimension d) { return d.height/5; } // empirical
            
            private void preferredButtonPanelSize(Container pane) {
                buttonpanelwidth = 0;
                // two buttons across minimum
    
                if (buttonv.size()==0) {
                    if (panellayout_tracing)
                        System.err.println("preferredButtonPanelSize no buttons");
                    PanelButton b = new PanelButton("Prove",  new Insert[0]);
                    Dimension d = b.getPreferredSize();
                    int buttonleading = leading(d);
                    buttonpanelheight = 0;
                    buttonpanelwidth = d.width*2+1*buttonleading;
                }
                else {
                    if (panellayout_tracing)
                        System.err.println("preferredButtonPanelSize");
                    Dimension d = ((PanelButton)buttonv.get(0)).getPreferredSize();
                    int buttonleading = leading(d);
                    int buttonheight = d.height;
                    int twowidth = 0; // shut up compiler
                    
                    buttonpanelheight = buttonleading;
                    
                    for (int i=0; i<buttonv.size(); i++) {
                        PanelButton button = (PanelButton)buttonv.get(i);
                        d = button.getPreferredSize();
                        
                        if (panellayout_tracing)
                            System.err.println(i+": "+d.width+","+d.height);
    
                        if (i%2==0) {
                            buttonpanelheight += buttonleading+buttonheight;
                            twowidth = d.width;
                        }
                        else
                            twowidth += buttonleading+d.width;
    
                        if ((i+1)%2==0)
                            buttonpanelwidth = Math.max(twowidth, buttonpanelwidth);
                    }
    
                    if (buttonv.size()<2)
                        buttonpanelwidth = (buttonpanelwidth-(buttonv.size()-1)*buttonleading)*2/buttonv.size()+
                            1*buttonleading;
                }
                if (panellayout_tracing)
                    System.err.println("preferredButtonPanelSize = "+buttonpanelwidth+","+buttonpanelheight);
            }
            
            /* Called by the Container getPreferredSize method, which is itself called under
            * a variety of circumstances. This method should calculate and return the ideal
            * size of the container, assuming that the components it contains will be at or
            * above their preferred sizes. This method must take into account the container's
            * internal borders, which are returned by the getInsets method.
            */
    
            public Dimension preferredLayoutSize(Container pane) {
                preferredButtonPanelSize(pane);
                JViewport port = scrollPane.getViewport();
                Dimension preferredSize =
                    new Dimension(Math.max(buttonpanelwidth, Math.min(buttonpanelwidth*4/3,
                                                                      port.getX()+list.getWidth())),
                                  buttonpanelwidth+buttonpanelheight*2/3);
                return preferredSize;
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
                    Dimension d = buttons[0].getPreferredSize();
                    int leading = leading(d), bh = d.height;
                    int width = pane.getWidth(), height=pane.getHeight();
                    if (panellayout_tracing)
                        System.err.println("PanelWindow.layoutContainer "+width+","+height);
                    int y = height-leading-bh, packedwidth = 0;
                    for (int i=0; i<buttons.length; i++) {
                        buttons[i].setSize(buttons[i].getPreferredSize());
                        int bw = buttons[i].getWidth();
                        if (packedwidth==0 || packedwidth+leading+bw<=width) {
                            for (int j=0; j<i; j++)
                                if (buttons[j].getY()==y)
                                    buttons[j].setLocation(buttons[j].getX()-bw-leading,y);
                        }
                        else {
                            for (int j=0; j<i; j++)
                                buttons[j].setLocation(buttons[j].getX(), buttons[j].getY()-bh-leading);
                            packedwidth = 0;
                        }
                        buttons[i].setLocation(width-bw, y);
                        if (packedwidth==0)
                            packedwidth = bw;
                        else
                            packedwidth += bw+leading;
                    }
    
                    if (panellayout_tracing)
                        for (int i=0; i<buttons.length; i++)
                            System.err.println(i+": "+buttons[i].getX()+","+buttons[i].getY()+" "+
                                            buttons[i].getWidth()+","+buttons[i].getHeight());
    
                    // give the scroll pane the rest of the height
                    scrollPane.setBounds(0, 0, width, Math.max(0, buttons[0].getY()-leading));
                    if (panellayout_tracing)
                        System.err.println(scrollPane.getX()+","+scrollPane.getY()+" "+
                                        scrollPane.getWidth()+","+scrollPane.getHeight());
                }
            }
        }
    }

    private void initWindow() {
        window = new PanelWindow();
    }
    
    /**********************************************************************************************

        Static interface for Dispatcher

     **********************************************************************************************/

    private static Vector panelv = new Vector();
    
    public static PanelWindowData spawn (String title, int kind) throws ProtocolError {
        PanelWindowData p = findPanel(title,false);
        if (p!=null) {
            if (p.kind==kind)
                return p;
            else
                throw new ProtocolError ("panel exists with different kind");
        }
        else {
            // it ain't there
            PanelWindowData panel = new PanelWindowData(title, kind);
            panelv.add(panel);
            return panel;
        }
    }

    protected static PanelWindowData findPanel(String title, boolean musthave) throws ProtocolError {
        for (int i=0; i<panelv.size(); i++) {
            PanelWindowData panel = (PanelWindowData)panelv.get(i);
            if (panel.title.equals(title))
                return panel;
        }

        if (musthave)
            throw new ProtocolError("before NEWPANEL");
        else
            return null;
    }

    public static void makePanelsVisible() {
        for (int i=0; i<panelv.size(); i++) {
            PanelWindowData panel = (PanelWindowData)panelv.get(i);
            if (panel.window==null)
                panel.initWindow();
            else {
                // this isn't tested until we are able to add things to the panel ...
                panel.window.resetPanelLayout();
                // but don't reset the size or position ...
            }
            panel.window.setVisible(true);
        }
    }

    public static void cancelPanels() {
        while (panelv.size()!=0) {
            ((PanelWindowData)panelv.get(0)).closeWindow();
            panelv.remove(0);
        }
    }

    public static void emptyPanels() {
        for (int i=0; i<panelv.size(); i++)
            ((PanelWindowData)panelv.get(0)).emptyPanel();
    }

    public static void addEntry(String panel, String entry, String cmd) throws ProtocolError {
        findPanel(panel,true).addEntry(entry,cmd);
    }
    
    public static void addButton(String panel, String button, Insert[] cmd) throws ProtocolError {
        findPanel(panel,true).addButton(button,cmd);
    }

    public static void markEntry(String panel, String cmd /* really */, boolean proved, boolean disproved)
        throws ProtocolError {
        findPanel(panel,true).markEntry(cmd, proved, disproved);
    }
}
