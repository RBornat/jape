//
//	File:	AboutBox.java
//

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class AboutBox extends JFrame
                      implements ActionListener
{
    protected String version;
    protected JPanel textPanel;
    
    public AboutBox() {
	super();
        this.getContentPane().setLayout(new BorderLayout(15, 30));
        this.setFont(new Font ("SansSerif", Font.BOLD, 14));
        
        textPanel = new JPanel(new BorderLayout(15, 15));
        textPanel.add(
            new JLabel("This is the platform-independent interface to the Jape proof engine"), 
                        BorderLayout.NORTH);
        this.getContentPane().add (textPanel, BorderLayout.NORTH, 0);
		
	JButton okButton = new JButton("OK");
        JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 15, 15));
        buttonPanel.add (okButton);
        okButton.addActionListener(this);
        this.getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        this.pack();
    }
	
    public void actionPerformed(ActionEvent newEvent) {
        setVisible(false);
    }
    
    public void setVersion(String version) {
        this.version = version;
        textPanel.add(new JLabel("working with "+version), BorderLayout.CENTER);
    }
	
}