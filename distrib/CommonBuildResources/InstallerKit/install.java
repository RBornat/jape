/**
        $Id$

        Here, in a single class, we define the support for installing
        jape (or any other application) on any machine on which java
        is installed. It was designed and written in less than 5
        hours, so there may be some outstanding things to simplify,
        but, for the moment, it will do the job we need doing.

        Our method is to build an (executable) jar file containing
        everything that the application needs, together with a
        primary bootstrap installer class whose main program gets
        executed from the jar.

        The primary bootstrap unpacks the jar in-situ at the
        installation site, before loading and instantiating a
        designated secondary bootstrap that should be written in
        java.

        The point of building an executable jar is that some Windows
        java installations will let one execute a jar file by
        double-clicking on it -- this saves the poor installer from
        having to find out how to get java to work on a command line,
        and/or finding a way of unpacking a jar.

        TO USE
        ------

        Assemble all the components you need for your application to work
        in a directory $(BUILD)
        
                mkdir $(BUILD)/bootstrap
                cp    install.class       $(BUILD)/bootstrap
                java  bootstrap.install   [switches] $(TARGET).jar $(RESOURCEFILENAMES)

        This builds a jar called $(TARGET).jar that can be transported anywhere.

        It is fairly important that the name of this file is not
        changed, for if it is then the installation process is made a
        little more complicated (see below). We could make it
        simpler, but (frankly) it isn't worth it.

        WINDOWS/APPLE INSTALL
        ---------------------
        To install on a Windows (or, I suppose, a Napple) machine, on which java
        has been properly installed (only the jre is needed, not the whole jdk)
        just place$(TARGET).jar in the installation folder you have chosen,
        then doubleclick on it.

        If $(TARGET).jar file has changed its name since it was made,
        then it's probably better to change it back to the original
        and double click on it as above. If for some reason you don't
        want to do that, make a shortcut to the jar file and fix the
        Windows TARGET property of the shortcut so that it's a command
        that takes its own  jar file name as its first argument.
        
        UNIXOID INSTALL
        ---------------
        To install on a Unixoid machine, on which java has been
        properly installed, (only the jre is needed, not the whole jdk),
        just place $(TARGET).jar in the installation folder you have chosen,   
        then
                java -jar $(TARGET).jar

        IF THE $(TARGET).jar file has changed its name (say to
        foo.jar) since it was made, then
        
                java -jar foo.jar foo.jar

        will do the trick. 
                

        TO DO
        -----

        It would be simpler and better if this class were split into
        the bootstrap-builder, and the primary bootstrap installer, but
        (at the moment) the overhead of the builder being present at
        install time is rather small (about 10kb). 
        
*/
package bootstrap;

// FOR THE INSTALL-TIME GUI
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

// FOR JAR UNPACKING
import java.io.*;
import java.util.*;
import java.util.zip.*;

public class install implements ActionListener
{ static String 
         packageName        = "bootstrap"
  ,      bootstrapClassFile = packageName+"/install.class"+" "+packageName+"/install$1.class"
  ,      bootstrapClass     = packageName+".install"
  ,      installerName      = "applicationinstaller"
  ,      installerSource    = packageName+"/"+installerName + ".java"
  ,      installerClassFile = packageName+"/"+installerName + ".class"
  ,      installerClass     = packageName+"."+installerName
  ,      manifestName       = packageName+"/"+installerName + ".mf"
  ,      gifFile            = null
  ,      installgifFile     = null
  ;

  /////////////////////////// CONFIGURE TIME ////////////////////////////
  
  public static void main(String[] args) throws Exception
  { String       installjar = null;
    StringBuffer resources  = new StringBuffer();

    if (args.length>0)
    { 
      
      for (int i=0; i<args.length; i++)
      {
         String arg=args[i];
         if (arg.startsWith("-"))
         { String param = args[++i];
           if (arg.startsWith("-splash")) 
           { gifFile = param; 
             installgifFile =  packageName + "/" + param;
             resources.append(" " + installgifFile);
           }
           else
           {
             System.err.println("Unknown switch: "+arg+" "+param);
             System.err.println("Usage: java bootstrap.install [-splash imagefile] target.jar [resource]*");  
             System.exit(1);
           }
         }
         else
         if (installjar==null && arg.endsWith(".jar")) 
            installjar = arg;
         else
            resources.append(" "+arg);
      }
    }

    if ( installjar == null )
    {
       System.err.println("Usage: java bootstrap.install [-splash imagefile] target.jar [resource]*");  
       System.exit(1);
    }
    else
    {  

       System.err.println("[Bootstrap installer building  "+installjar +"]");  
       
       String[] installer =
       { "package " + packageName + ";"
       , "public class " + installerName
       , "{ public static void main(String[] args)"
       , "  { new " + bootstrapClass + "(args.length==0?"+quote(installjar)+":args[0], "+quote(gifFile)+"); }"
       , "}"
       };
       cat(installerSource, installer);
       
       String[] manifest =
       { "Main-Class: "+installerClass
       };
       cat(manifestName,    manifest);
       
       boolean ok = execute
       ( new String[]
         { "javac " + installerSource
         , gifFile==null?"# no install-time icon":("cp "    + gifFile + " " + installgifFile)
         , "jar -cvfm " + installjar + " " + manifestName + " " + installerClassFile + " " + bootstrapClassFile + resources.toString()
         }
       );

       if (!ok) 
       { System.err.println("[Failed to build the installer]");
         System.exit(1);
       }
       else
         System.err.println("[Bootstrap installer completed  "+installjar +"]");  
    }
  }

  public static String quote(String s) { return s==null?null:"\""+s+"\""; }

  public static void cat(String filename, String[] lines) throws Exception
  {
       PrintWriter out = new PrintWriter(new FileOutputStream(filename));
       for (int i=0; i<lines.length; i++) out.println(lines[i]);
       out.close();
  }
  
  public static void echo(InputStream inp) throws Exception
  { final LineNumberReader i = new LineNumberReader(new InputStreamReader(inp));
    new Thread()
    { public void run()
      { String line = null;
        try
        {
           while ((line = i.readLine()) != null)
           {
              System.err.println(line);
           }
           i.close();
        }
        catch (Exception exn) 
        {
           System.err.println("[Installer exception "+exn+" from subprocess]");
        }
      }
    }.start();
  }
  
  public static boolean execute(String[] lines)
  {
       Runtime sys = Runtime.getRuntime();
       int     status = 0;
       for (int i=0; status == 0 && i<lines.length; i++)
       { System.err.println(lines[i]); 
         System.err.flush();
         if (!lines[i].trim().startsWith("#"))
         try 
         {
            Process p = sys.exec(lines[i]);
            echo(p.getInputStream());
            echo(p.getErrorStream());
            status = p.waitFor();
         }
         catch (Exception exn)
         {
           System.err.println("[Installer exception "+exn+"]");
         }
       }
       return status==0;
  }

  /////////////////////////// INSTALL TIME ////////////////////////////
  
  /** Use the place we were loaded from */
  public static Image getImage(Object host, String localname)
  {  Toolkit tk = Toolkit.getDefaultToolkit();
     return tk.getImage(host.getClass().getResource(localname));
  }

  String jarfilename = null;
  static TextArea progress = null;

  public static void showProgress(String l)
  {
    progress.append(l);
    progress.append("\n");
  }

  public static JPanel column(Component[] cs)
  { JPanel r = new JPanel(new GridLayout(cs.length, 1));
    for (int i=0; i<cs.length; i++) r.add(cs[i]);
    return r;
  }

  public install(String jarfilename, String giffilename)
  {  this.jarfilename  = jarfilename;
     String    caption = "Jape Installer for "+jarfilename;
     Font      font    = new Font("SansSerif", Font.BOLD, 18);
     JFrame    frame   = new JFrame(caption);
     Container content = frame.getContentPane();
     JButton   install = new JButton("Install Jape in this Folder");
     JButton   exit    = new JButton("Exit Now");
     JLabel    label   = new JLabel(caption, SwingConstants.CENTER);
     progress = new TextArea("Installation Progress\n", 10, 50);
     install.addActionListener(this);
     exit.addActionListener(this);
     install.setFont(font);
     exit.setFont(font);
     label.setFont(font);
     
     
     if (giffilename!=null)
     {
       ImageIcon i = new ImageIcon(getImage(this, giffilename));
       JLabel icon = new JLabel(i, SwingConstants.CENTER);
       content.add(icon, BorderLayout.NORTH);
     }
     Component[] buttons = new Component[]
     { label, exit, install
     };
     content.add(progress,        BorderLayout.CENTER);
     content.add(column(buttons), BorderLayout.SOUTH);
     frame.setLocation(new Point(200, 200));
     frame.pack();
     frame.setVisible(true);
  }

  public void actionPerformed(ActionEvent e)
  { String c = e.getActionCommand();
    if (c.startsWith("Install"))
    { 
       try
       { 
         unJar(".", new FileInputStream(jarfilename));
       }
       catch (Exception exn)
       {
          System.err.println(exn);
       }
    }
    else
    if (c.startsWith("Exit"))
    {
       System.exit(0);
    }
  }


  /////////////////////////// JAR UNPACKING ////////////////////////////


  /**
        Ensure that the directories in the given path exist.
  */
  public static void mkDir(String path)
  { File dir = new File(path);
    if (dir.mkdirs()) System.err.println("[made "+path+"]");
  }
  
  /**
        Ensure that the directories in the given path exist.
  */
  public static void mkParentDir(String path)
  { File dir = new File(path);
    mkDir(dir.getParent());
  }
  
  /**
        Unpack the jar file on the input stream into
        the directory named in dir.
  */
  public static void unJar(String rootPath, FileInputStream in)
  throws IOException, ZipException
  { mkDir(rootPath);
    ZipInputStream zip    = new ZipInputStream(in);
    ZipEntry       entry  = null;
    byte[]         buffer = new byte[50*1024];
    while ((entry = zip.getNextEntry())!=null)
    { String path = rootPath + File.separatorChar + entry.getName();
      System.err.println(path);
      showProgress(path);
      if (entry.isDirectory())
      { 
        mkDir(path);
      }
      else
      { mkParentDir(path);
        FileOutputStream out = new FileOutputStream(path);
        int len;
        while ((len = zip.read(buffer, 0, buffer.length)) >=0 )
        { 
          out.write(buffer, 0, len);
        }
        out.close();
        zip.closeEntry();
      }
    }
    zip.close();    
  }

  
}











