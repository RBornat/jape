import java.util.*;
import javax.swing.*;

/**
        This is a typical post-install class
*/

public class postinstall 
{
  public void run(String install, Properties props)
  { System.err.println("Install:    "+install);
    System.err.println("Properties: "+props);
    JFrame     frame = new JFrame("POST-INSTALL: "+props.getProperty("-app"));
    JComponent text  = new JTextArea("Properties are:"+props.toString().replace('{', '\n').replace('}', '\n').replaceAll("-", "\n-"));
    frame.getContentPane().add(text);
    frame.setDefaultCloseOperation(frame.DISPOSE_ON_CLOSE);
    frame.pack();
    frame.setLocation(50, 50);
    frame.setVisible(true);
  }
}

