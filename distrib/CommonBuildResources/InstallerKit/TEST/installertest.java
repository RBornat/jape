import javax.swing.*;
import javax.swing.border.*;

/**
 * $Revision$
 * $Date$
 * $Name$
*/

public class installertest
{ public static void main(String[] args)
  { final JFrame frame = new JFrame("Simple installer test");
    final JLabel label = new JLabel
    (
       "<html><font  size='+2' ><center color='red'>INSTALLER TEST</center><center color='yellow'>OK</center></font></html>"
    );
    label.setHorizontalAlignment(label.CENTER);
    label.setVerticalAlignment(label.CENTER);
    String name = System.getProperty("user.name");
    String home = System.getProperty("user.home");
    String dir = System.getProperty("user.dir");
    String app = System.getProperty("application.home");
    StringBuffer props = new StringBuffer();
    props.append("Name: "+name+"\n");
    props.append("Home: "+home+"\n");
    props.append("Dir:  "+dir+"\n");
    props.append("App:  "+app+"\n");
    for (int i=0; i<args.length; i++)
        props.append("Arg:  "+args[i]+"\n");
    props.append("URL:  "+installertest.class.getResource(installertest.class.getName()+".class")+"\n");
    final JLabel panel = new JLabel("<html><pre>"+props.toString()+"</pre><html>");
    panel.setHorizontalAlignment(label.CENTER);
    panel.setVerticalAlignment(label.CENTER);
    panel.setBorder(new TitledBorder("Runtime Properties"));
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.getContentPane().add(label, "North");
    frame.getContentPane().add(panel, "Center");
    frame.pack();
    // frame.setSize(200, 200);
    frame.setLocation(100, 100);
    frame.setVisible(true);
  }
}



