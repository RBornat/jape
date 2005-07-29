// The package name may be changed ad lib; the code self-adjusts
package org.jape.bootstrap;

import net.jimmc.jshortcut.DUMMY;
// FOR MAKING THE LINKS
import net.jimmc.jshortcut.JShellLink;

import java.awt.*;
import java.awt.event.*;
// FOR JAR UNPACKING
import java.io.*;
// For post-install class running
import java.lang.reflect.*;
// FOR BOTH PHASES
import java.net.URL;
import java.net.URI;

import java.util.*;
import java.util.jar.*;
import java.util.zip.*;
// FOR THE INSTALL-TIME GUI
import javax.swing.*;
import javax.swing.border.*;


/**
   Bootstrap class for installer-generator and installer.
   @Author Bernard Sufrin
   @Revision $Revision$
   <H2>Licence:</H2>
   <P>
    This program is free software; you can redistribute it and/or modify
    it under the terms of the  GNU General Public License  as published
    by the Free Software Foundation; either version 2 of the License,
    or (at your option) any later version.
   </P>
   <P>
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
    General Public License for more details.
   </P>
   <PRE>
   $Id$
   </PRE>
 */
public class install implements ActionListener
{
  static String  installerClass     = install.class.getName();
  static String  packageName        =
    installerClass.substring(
                             0,
                             installerClass.lastIndexOf('.')
                            );
  static String  packagePath        = substFor(".", "/", packageName);
  static String  bootPath           = "bootstrap";
  static String  installerName      = "install";
  static String  tempPath           = System.getProperty("java.io.tmpdir");
  static String  WINDLL             = "jshortcut.dll";
  static String  manifestName       = tempPath + "/" + installerName + ".mf";
  static String  SplashImage        = null;
  static String  propertiesResource = installerName + ".properties";
  static String  propertiesFile     = tempPath + "/" + propertiesResource;
  static boolean isWindows          =
    System.getProperty("os.name", "Unix")
          .startsWith("Windows");
  static boolean debugging          =
    null != System.getProperty(
                               "EBUG",
                               System.getProperty("DEBUG")
                              );

  /////////////////////////// CONFIGURE TIME ////////////////////////////
  public static String getJar()
  {
    String loc = install.class.getResource("install.class")
                              .getFile();

    // System.err.println("LOC: "+loc);
    int fileLoc = loc.indexOf("!/");
    if (fileLoc < 0)
      return null;
    else
    try
    {  if (debugging) System.err.println(new URI(loc));
       return new URI(loc.substring(0, fileLoc)).getPath();
    }
    catch (Exception ex)
    { ex.printStackTrace();
      return loc.substring(0, fileLoc)
                .replaceAll("%20", " ")
                .replaceAll("file:", "");
    }
  }

  public static String fromJar(String file)
  {
    file = (getJar() + "!/" + file).replace(File.separatorChar, '/');
    if (debugging)
      System.err.println("Retrieving resource from Jar: " + file);

    try
    {
      URI uri = new URI("jar:file", file, null);
      return uri.toString();
    }
    catch (Exception ex)
    { ex.printStackTrace();
      return file;
    }
  }

  /**
     <P>
             Invoked with 0 or 1 arguments this causes the installer resident in the jar file
             from which this class was loaded to be run; invoked with more than one argument
             this causes an installer jar file to be constructed.
     </P>
   */
  public static void main(String[] args) throws Exception
  {
    if (args.length > 1)
    {
      System.err.println("BuildInstaller: $Revision$ ");
      makeInstaller(args);
    }
    else
    {
      Properties props = getProperties();
      if (props == null)
      {
        System.err.println("BuildInstaller $Revision$ needs parameters (please see documentation)");
        System.exit(1);
      }

      new install(props);
    }
  }

  public static boolean runObject(
                                  Object theObject,
                                  String INSTALL,
                                  Properties props
                                 )
  {
    try
    {
      Method m =
        theObject.getClass()
                 .getMethod("run", new Class[]{String.class, Properties.class});
      m.invoke(theObject, new Object[]{INSTALL, props});
      return true;
    }
    catch (Throwable ex)
    {
      showProgress("[RUNNING " + theObject + " caused an exception: " + ex);
      ex.printStackTrace();
      return false;
    }
  }

  private static String withoutPath(String s)
  {
    return new File(s).getName();
  }

  private static String withBootPath(String s)
  {
    return new File(
                    bootPath,
                    new File(s).getName()
                   ).getPath();
  }

  /**
     <P>
       Make a text file (safely) at install time
     </P>
   */
  private static boolean makeFile(String name, String body)
  {
    try
    {
      PrintWriter file = new PrintWriter(new FileWriter(name));
      file.println(body);
      file.close();
      return true;
    }
    catch (Exception ex)
    {
      showProgress("[WARNING: " + ex + "]");
      ex.printStackTrace();
      return false;
    }
  }

  /**
     <P>
        Represents a set of paths
     </P>
   */
  static class FileSet extends TreeSet
  {
    public void append(String name)
    {
      name = name.trim();
      if ("".equals(name))
        return;

      add(name);
    }

    public void appendWithoutPath(String name)
    {
      name = name.trim();
      if ("".equals(name))
        return;

      add(withoutPath(name));
    }

    public String toString()
    {
      Iterator     it = iterator();
      StringBuffer b = new StringBuffer();
      while (it.hasNext())
      {
        b.append(it.next().toString());
        if (it.hasNext())
          b.append(" ");
      }

      return b.toString();
    }

    public String toList()
    {
      Iterator     it = iterator();
      StringBuffer b = new StringBuffer();
      while (it.hasNext())
      {
        b.append(it.next().toString());
        if (it.hasNext())
          b.append(",");
      }

      return b.toString();
    }

    public static FileSet fromList(String listform)
    {
      FileSet result = new FileSet();
      if (listform != null)
      {
        String[] strings = listform.trim()
                                   .split("[,]");
        for (int i = 0; i < strings.length; i++)
          result.add(strings[i]);
      }

      return result;
    }
  }

  private static void makeInstaller(String[] args) throws Exception
  {
    String     installjar    = null;
    String     autoInstall   = null;
    String     autoIcon      = null;
    String     autoExe       = null;
    String     autoBin       = null;
    String     autoJar       = null;
    FileSet    resources     = new FileSet();
    FileSet    syncresources = new FileSet();
    FileSet    bootresources = new FileSet();
    Properties props         = new Properties();
    boolean    sync          = true;
    boolean    boot          = false;
    boolean    delW          = false;
    boolean    delU          = false;
    FileSet    delwindows    = new FileSet();
    FileSet    delunix       = new FileSet();
    if (args[0].endsWith(".jar"))
    {
      installjar = args[0];
      props.setProperty("-jar", installjar);
    }
    else
    {
      System.err.println("[Install: first argument should be a .jar file. ("
                         + args[0] + ")]"
                        );
      System.err.println("[Failed to build the installer]");
      System.exit(1);
    }

    for (int i = 1; i < args.length; i++)
    {
      String arg = args[i];
      if (arg.equals("-sync"))
      {
        sync   = true;
        delW   = delU = boot = false;
      }
      else if (arg.equals("-boot"))
      {
        boot   = true;
        delW   = delU = sync = false;
      }
      else if (arg.equals("-delwindows"))
      {
        boot   = sync = delU = false;
        delW   = true;
      }
      else if (arg.equals("-delunix"))
      {
        boot   = sync = delW = false;
        delU   = true;
      }
      else if (arg.equals("-debug"))
      {
        debugging = true;
      }
      else if (arg.startsWith("+"))
      {
        System.err.println("[Install: + switches are obsolete. (" + arg + ")]");
        System.err.println("[Failed to build the installer]");
        System.exit(1);
      }
      else if (arg.startsWith("-"))
      {
        String param = args[++i].trim();
        sync   = true;
        delW   = delU = boot = false;
        if (arg.equals("-autoinstall"))
        {
          // Write the second-stage installer command files, etc.
          autoInstall = param;
        }
        else if (arg.equals("-autoexe"))
        {
          // Write the second-stage installer command files, etc.
          autoExe = withoutPath(param);
          syncresources.append(param);
          resources.append(autoExe);
          delunix.append(autoExe);
        }
        else if (arg.equals("-autobin"))
        {
          // Write the second-stage installer command files, etc.
          autoBin = withoutPath(param);
          syncresources.append(param);
          resources.append(autoBin);
          delwindows.append(autoBin);
        }
        else if (arg.equals("-autojar"))
        {
          // Write the second-stage installer command files, etc.
          if (!param.endsWith(".jar"))
            param += ".jar";

          autoJar = withoutPath(param);
          syncresources.append(param);
          resources.append(autoJar);
        }
        else if (arg.equals("-autoicon") || arg.equals("-autologo"))
        {
          // Icon to be used in the second-stage installer command files, etc.
          autoIcon = withoutPath(param);
          ;
          syncresources.append(param);
          resources.append(autoIcon);
          //delwindows.append(autoIcon);
          //delunix.append(autoIcon);
        }
        else if (
                 arg.equals("-comment")
                 || arg.equals("-autocmd")
                 || args.equals("-desktop")
                )
        {
          props.setProperty(arg, param);
        }
        else if (arg.equals("-splash"))
        {
          SplashImage = param;
          bootresources.append(SplashImage);
          props.setProperty(
                            "-splash",
                            withBootPath(SplashImage)
                           );
        }
        else if (arg.equals("-splashside"))
        {
          if (
              param.equals("West")
              || param.equals("North")
              || param.equals("East")
             )
            props.setProperty("-splashside", param);
          else
            System.err.println("[WARNING: -splashside should be North, East, or West]");
        }
        else
        {
          props.setProperty(arg, param);
          if (arg.startsWith("-cmdwindows") && param.endsWith(".cmd"))
          {
            resources.appendWithoutPath(param);
            if (sync)
              syncresources.append(param);
          }

          if (arg.startsWith("-class"))
          {
            String classfile = param + ".class";
            syncresources.append(classfile);
            delwindows.append(classfile);
            delunix.append(classfile);
          }
        }
      }
      else
      {
        if (sync)
          syncresources.append(arg);

        if (boot)
          bootresources.append(arg);
        else if (delW)
          delwindows.append(arg);
        else if (delU)
          delunix.append(arg);
        else
          resources.appendWithoutPath(arg);
      }
    }

    // auto-installation
    if (autoInstall != null)
    {
      if ((autoExe == null) && (autoBin == null) && (autoJar == null))
      {
        autoJar = autoInstall + ".jar";
        syncresources.append(autoJar);
        resources.appendWithoutPath(autoJar);
        autoExe = autoBin = autoJar;
      }

      props.setProperty("-autoinstall", autoInstall);
      props.setProperty("-autojar", autoJar);
      if (autoIcon != null)
        props.setProperty("-autoicon", autoIcon);
    }

    // Finalize properties 
    if (!delunix.isEmpty())
      props.setProperty(
                        "-delunix",
                        delunix.toList()
                       );

    if (!delwindows.isEmpty())
      props.setProperty(
                        "-delwindows",
                        delwindows.toList()
                       );

    // Write the props file
    FileOutputStream propfile = new FileOutputStream(propertiesFile);
    props.store(propfile, "Installer properties");
    bootresources.append(propertiesFile);
    propfile.close();
    if (installjar == null)
    {
      System.err.println("Usage: java bootstrap.install [-splash imagefile] target.jar [resource]*");
      System.exit(1);
    }
    else
    {
      System.err.println("[Bootstrap installer building: " + installjar + "]");
      if (debugging)
        System.err.println("[BOOT RESOURCES: " + bootresources + "]");

      if (debugging)
        System.err.println("[SHIP RESOURCES: " + syncresources + "]");

      if (debugging)
      {
        System.err.println("[PROPERTIES ARE AS FOLLOWS]");
        for (Enumeration nm = props.propertyNames(); nm.hasMoreElements();)
        {
          String name = (String) nm.nextElement();
          System.err.println(name + "=" + props.getProperty(name));
        }
      }

      makeFile(manifestName, "Main-Class: " + installerClass);
      Manifest man = new Manifest(new FileInputStream(manifestName));
      System.err.println("[SHIPPING THE FOLLOWING FILES]");
      boolean ok;
      Jar     jar = new Jar(installjar, manifestName);

      // Add the bootstrap material
      String fromJar = getJar();
      if (fromJar == null)
      {  // It ran as a class: the bootstrap components should be accessible here
        jar.add(packagePath);  // Add the installer package
        jar.addDirectory       // Add the Shortcut-generator package
        (
         DUMMY.packagePath(),
         DUMMY.packagePath()
        );
        jar.add(
                new File(WINDLL),   // add the .dll file 
                new File(bootPath)  // ... to the bootstrap path
               );
      }
      else
      {  // It ran from a JAR so we need to acquire the bootstrap components
         // 1. Unpack the very jar it ran from
        unJar(
              tempPath + "/BUILDER",
              new FileInputStream(fromJar),
              false
             );

        // 2. Add the entire JAR content to the jar we're building
        jar.chDir(".");
        jar.addDirectory(new File(tempPath + "/BUILDER"));

        // 3. Add the DLL -- we know it's in the Jar because we put it there
        jar.add(
                new URL(fromJar(WINDLL)),
                new File(bootPath)
               );
      }

      jar.chDir(".");
      for (Iterator i = syncresources.iterator(); i.hasNext();)
      {
        jar.add(new File((String) i.next()));
      }

      jar.chDir(bootPath);
      for (Iterator i = bootresources.iterator(); i.hasNext();)
      {
        jar.add(new File((String) i.next()));
      }

      ok = jar.canClose();
      if (!ok)
      {
        System.err.println("[Failed to build the installer]");
        System.exit(1);
      }
      else
        System.err.println("[Bootstrap installer completed: " + installjar
                           + "]"
                          );
    }
  }

  /**
     Start a new process to echo an input stream
   */
  static void echo(InputStream inp, final boolean toProgress) throws Exception
  {
    final LineNumberReader i = new LineNumberReader(new InputStreamReader(inp));
    new Thread()
      {
        public void run()
        {
          String line = null;
          try
          {
            while ((line = i.readLine()) != null)
            {
              if (toProgress)
                showProgress(line);
              else
                System.err.println(line);
            }

            i.close();
          }
          catch (Exception exn)
          {
            System.err.println("[Installer exception " + exn
                               + " from subprocess]"
                              );
          }
        }
      }.start();
  }

  static boolean execute(String line, boolean toProgress)
  {
    return execute(new String[]{line}, toProgress);
  }

  static boolean execute(String[] lines, boolean toProgress)
  {
    Runtime sys    = Runtime.getRuntime();
    int     status = 0;
    for (int i = 0; (status == 0) && (i < lines.length); i++)
    {  //System.err.println(lines[i]); 
       //System.err.flush();
      if (!lines[i].trim()
                   .startsWith("#"))
        try
        {
          Process p = sys.exec(lines[i]);
          echo(p.getInputStream(), toProgress);
          echo(p.getErrorStream(), toProgress);
          status = p.waitFor();
        }
        catch (Exception exn)
        {
          if (toProgress) showProgress("[Installer exception " + exn + "]");
        }
    }

    if (toProgress)
      showProgress("[STATUS " + status + "]");

    return (status == 0) || (isWindows && (status == 1));
  }

  static boolean execCommand(String[] args)
  {
    Runtime sys    = Runtime.getRuntime();
    int     status = 0;
    try
    {
      Process p = sys.exec(args);
      echo(
           p.getInputStream(),
           true
          );
      echo(
           p.getErrorStream(),
           true
          );
      status = p.waitFor();
    }
    catch (Exception exn)
    {
      //System.err.println("[Installer exception "+exn+"]");
      showProgress("[Installer exception " + exn + "]");
    }

    return (status == 0) || (isWindows && (status == 1));
  }

  /** Ameliorates quotation hell -- perhaps! */
  public static String quoteArg(String val)
  { if (val==null) return null;
    // Normalize quotes
    val   = val.replace('\'', '"');
    // Embed an argument with spaces but no quotes in quotes
    val   = (
              ((val.indexOf(' ') < 0) || (val.indexOf('"') >= 0)) ? val
                                                                  : (
                                                                    "\"" + val
                                                                    + "\""
                                                                  )
            );
    // Remove any doubled-up quotes (shouldnae happen!)
    while (val.indexOf("\"\"") >= 0)
      val = substFor("\"\"", "\"", val);

    return val;
  }

  public static String unQuoteArg(String val)
  {
    return substFor("\"", "", val);
  }

  /**
     Replace a pattern with a replacement in a target string.
   */
  public static String substFor(String pat, String repl, String target)
  {
    if ("".equals(pat))
      return target;

    int          start  = 0;
    int          next   = target.indexOf(pat);
    StringBuffer result = new StringBuffer();
    while (next >= 0)
    {
      result.append(target.substring(start, next));
      result.append(repl);
      start   = next + pat.length();
      next    = target.indexOf(pat, start);
    }

    result.append(target.substring(start));
    return result.toString();
  }

  /////////////////////////// INSTALL TIME ////////////////////////////
  /** Delete the file tree rooted at <code>file</code>.
     <P>
            Permissions of non-directories are respected -- if
            a file isn't writeable then it will not be deleted.
     </P>
     <P>
            Nonempty folders cannot be deleted.
     </P>
   */
  static boolean delete(File file)
  {  //  System.err.println("Deleting: "+file);
    if (file.isDirectory())
    {
      File[] files = file.listFiles();
      for (int i = 0; i < files.length; i++)
        delete(files[i]);
    }

    if (file.canWrite() && file.delete())
    {
      if (debugging)
        System.err.println("[DELETED: " + file + "]");

      return true;
    }
    else
    {
      System.err.println("Could not delete: " + file);
      return false;
    }
  }

  /**
     Remove a file and its empty ancestor directories.
   */
  static void deleteUpwards(File file)
  {
    System.err.println("[PRUNING: " + file + "]");
    while (delete(file))
    {
      file = file.getParentFile();
      if (file.listFiles().length > 0)
        break;
    }
  }

  /**
     Fetch a properties file
   */
  static Properties getProperties()
  {
    try
    {  // System.err.println("Loading properties: "+url);
      String     url = fromJar(bootPath + File.separator + propertiesResource);
      Properties p   = new Properties();
      p.load(new URL(url).openStream());
      return p;
    }
    catch (Exception exn)
    {
      return null;
    }
  }

  String          jarFileName      = null;
  String          jarPath          = null;
  File            install          = null;
  String          installDirName   = null;
  String          installDirSuffix = null;
  String          appName          = null;
  static TextArea progress         = null;
  static int      length           = 0;
  

  static void showProgress(String l)
  {
    if (l == null)
      l = "null";

    if (progress!=null) {
        progress.insert(l + "\n", length);
        length += (l.length() + 1);
        progress.setCaretPosition(length);
    }
  }

  static JComponent row(Component[] cs)
  {
    JComponent r = Box.createHorizontalBox();
    for (int i = 0; i < cs.length; i++)
    {
      if (cs[i] != null)
        r.add(cs[i]);
    }

    return r;
  }

  static Component hGlue()
  {
    return Box.createHorizontalGlue();
  }

  static Component vGlue()
  {
    return Box.createVerticalGlue();
  }

  static Container row(Component a, Component b, Component c)
  {
    return row(new Component[]
               {
                 a,
                 b,
                 c
               }
              );
  }

  static Container row(Component a, Component b, Component c, Component d)
  {
    return row(new Component[]
               {
                 a,
                 b,
                 c,
                 d
               }
              );
  }

  static Container row(
                       Component a,
                       Component b,
                       Component c,
                       Component d,
                       Component e
                      )
  {
    return row(new Component[]
               {
                 a,
                 b,
                 c,
                 d,
                 e
               }
              );
  }

  static Container row(Component a, Component b)
  {
    return row(new Component[]
               {
                 a,
                 b
               }
              );
  }

  static JComponent column(Component[] cs)
  {
    JComponent r = Box.createVerticalBox();
    for (int i = 0; i < cs.length; i++)
    {  //if (i>0) r.addSeparator(new Dimension(10, 0));
      r.add(cs[i]);
    }

    return r;
  }

  static Container col(Component[] cs)
  {
    Container r = new JPanel(new GridLayout(0, 1, 4, 4));
    for (int i = 0; i < cs.length; i++)
    {
      r.add(cs[i]);
    }

    return r;
  }
  
  static String[] splitCmd(String cmd)
  { String[] result = new String[2];
    int sp = cmd.indexOf(' ');
    if (sp==0) 
       { result[0]=cmd; result[1]=null; }
    else
       { result[0]=cmd.substring(0, sp); result[1]=cmd.substring(sp+1); }
    return result;
  }

  final JButton    exit;
  final JButton    installBut;
  JCheckBox        desktop         = null;
  final JTextField installDirField = new JTextField();
  final Properties props;
  String           HOME            = System.getProperty("user.home");
  String           HOMECMD;

  private install(Properties theProps)
  {
    this.props         = theProps;
    appName            = props.getProperty("-app", "Jape");
    jarFileName        = getJar();
    installDirSuffix   = props.getProperty("-installdir", appName);
    installDirName     = new File(installDirSuffix).getAbsolutePath();
    String       splashFileName = props.getProperty("-splash");
    String       tfont   = props.getProperty("-tfont", "Monospaced-PLAIN-14");
    String       bfont   = props.getProperty("-bfont", "SansSerif-PLAIN-14");
    String       caption = "Installer for " + appName;
    Font         tFont   = Font.decode(tfont);  //new Font("SansSerif", Font.BOLD, 18);
    Font         bFont   = Font.decode(bfont);  //new Font("SansSerif", Font.BOLD, 18);
    final JFrame frame   = new JFrame(caption);
    Container    content = frame.getContentPane();
    JButton      choose  =
      new JButton(props.getProperty(
                                    "-choosebutton",
                                    "Choose Installation Folder"
                                   )
                 );
    installBut = new JButton(props.getProperty("-installbutton", "Install"));
    JLabel label = new JLabel(props.getProperty("-label", "Install to: "));
    autoInstall = props.getProperty("-autoinstall");
    if (autoInstall != null)
    {
      HOMECMD = HOME + File.separator + "bin" + File.separator + autoInstall;
      if (isWindows)
        desktop =
          new JCheckBox(props.getProperty("-desktop", "Shortcut on Desktop"));
      else
        desktop =
          new JCheckBox(props.getProperty("-desktop", "Command in " + HOMECMD));
    }

    choose.setActionCommand("Choose:");
    choose.setFont(bFont);
    choose.setRolloverEnabled(true);
    exit =
      new JButton(props.getProperty("-exitbutton", "Exit without installing"));
    exit.addActionListener(this);
    exit.setActionCommand("Exit:");
    exit.setFont(bFont);
    exit.setRolloverEnabled(true);
    installBut.addActionListener(this);
    installBut.setActionCommand("Install:");
    installBut.setFont(bFont);
    installBut.setRolloverEnabled(true);
    try
    {
      installDirField.setText(new File(installDirName).getCanonicalPath());
    }
    catch (java.io.IOException ex)
    {
      installDirField.setText(new File(installDirName).getAbsolutePath());
    }

    label.setFont(bFont);
    progress = new TextArea("", 12, 60);
    progress.setFont(tFont);
    frame.addWindowListener(new WindowAdapter()
      {
        public void windowClosing(WindowEvent e)
        {
          Exit();
        }
      }
                           );
    // props.list(System.err);
    if (splashFileName != null)
    {
      JLabel icon;
      try
      {
        Icon image = new ImageIcon(new java.net.URL(fromJar(splashFileName)));
        icon = new JLabel(image, SwingConstants.CENTER);
      }
      catch (Exception ex)
      {
        icon = new JLabel(
                          ex.toString(),
                          SwingConstants.CENTER
                         );
      }

      String splashside = props.getProperty("-splashside", "North");
      content.add(
                  row(
                      hGlue(),
                      column(new Component[]
                             {
                               vGlue(),
                               icon,
                               vGlue()
                             }
                            ),
                      hGlue()
                     ),
                  splashside
                 );
    }

    Component[] buttons =
      new Component[]
      {
        new JLabel(" "),
        row(
            hGlue(),
            label,
            installDirField,
            hGlue()
           ),
        new JLabel(" "),
        row(
            hGlue(),
            col(new Component[]
                {
                  choose,
                  installBut,
                  exit
                }
               ),
            hGlue()
           ),
        new JLabel(" "),
        row(
            hGlue(),
            (desktop == null) ? hGlue() : (Component) desktop,
            hGlue()
           )
      };
    JComponent  panel = column(buttons);
    panel.setBorder(new CompoundBorder(
                                       new EmptyBorder(5, 5, 5, 5),
                                       new CompoundBorder(
                                                          new TitledBorder(
                                                                           new EtchedBorder(),
                                                                           "Installer"
                                                                          ),
                                                          new EmptyBorder(
                                                                          5,
                                                                          5,
                                                                          5,
                                                                          5
                                                                         )
                                                         )
                                      )
                   );
    content.add(progress, BorderLayout.CENTER);
    content.add(panel, BorderLayout.SOUTH);
    choose.requestFocus();
    frame.setLocation(new Point(50, 50));
    frame.pack();
    frame.setVisible(true);
    showProgress("Ready to install " + appName + " for "
                 + System.getProperty(
                                      "os.name",
                                      "unknown OS type (assumed to be Unixoid)"
                                     )
                );
    showProgress("Installing from " + jarFileName);
    showProgress("Installing to   " + installDirName);
    choose.addActionListener(new ActionListener()
      {
        public void actionPerformed(ActionEvent ev)
        {
          JFileChooser fc = new JFileChooser(".");
          fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
          fc.setDialogType(JFileChooser.SAVE_DIALOG);
          fc.setApproveButtonText(props.getProperty(
                                                    "-setbutton",
                                                    "Change Installation Folder"
                                                   )
                                 );
          if (JFileChooser.APPROVE_OPTION == fc.showSaveDialog(frame))
          {
            installDirName =
                             new File(
                                      fc.getSelectedFile(),
                                      installDirSuffix
                                     ).getAbsolutePath();
            installDirField.setText(installDirName);
            exit.setEnabled(true);
          }
        }
      }
                            );
  }

  boolean installedOK = false;
  String  INSTALL     = null;
  String  autoInstall = null;

  public String postSubst(String s)
  {
    if (s == null)
      return null;
    else if (isWindows)
    {
      s   = substFor(
                     "%DESKTOP-COMMON%",
                     JShellLink.getDirectory("common_desktopdirectory"),
                     s
                    );
      s   = substFor(
                     "%PROGRAMS-COMMON%",
                     JShellLink.getDirectory("common_programs"),
                     s
                    );
      s   = substFor(
                     "%DESKTOP%",
                     JShellLink.getDirectory("desktop"),
                     s
                    );
      s   = substFor(
                     "%PROGRAMS%",
                     JShellLink.getDirectory("programs"),
                     s
                    );
      s   = substFor(
                     "%DOCUMENTS%",
                     JShellLink.getDirectory("personal"),
                     s
                    );
      s   = substFor(
                     "%PROGRAM-FILES%",
                     JShellLink.getDirectory("program_files"),
                     s
                    );
    }
    else
    {
      s   = substFor("%DESKTOP-COMMON%", HOME, s);
      s   = substFor("%PROGRAMS-COMMON%", "/usr/local/bin/", s);
      s   = substFor("%DESKTOP%", HOME + File.separator + "Desktop", s);
      s   = substFor("%PROGRAMS%", HOME + File.separator + "bin", s);
      s   = substFor("%DOCUMENTS%", HOME + File.separator + "Documents", s);
      s   = substFor(
                     "%PROGRAM-FILES%",
                     HOME + File.separator + "PROGRAMFILES",
                     s
                    );
    }

    s   = substFor("%INSTALL%", install.getAbsolutePath(), s);
    s   = substFor("%JAR%", jarPath, s);
    s   = s.replace(File.separatorChar, '/');
    s   = substFor("//", "/", s);
    s   = s.replace('/', File.separatorChar);
    return s;
  }

  public String postSubstProperty(String propname)
  {
    return postSubst(props.getProperty(propname));
  }
  
  public String postSubstPathProperty(String propname)
  {
    String s = postSubst(props.getProperty(propname));
    if (s==null) return null;
    return toPath(s);
  }
  
  public String postSubstCommandProperty(String propname)
  {
    String s = props.getProperty(propname);
    if (s==null) return null;
    return toCommand(s);
  }
  
  public static String toPath(String s)
  { return new File(s).getAbsolutePath();
  }
  
  public String toCommand(String source)
  { StringBuffer t = new StringBuffer(); /* was StringBuilder t = new StringBuilder(); but Java 1.4 no like */
    boolean first = true;
    for (Iterator it = new Command(source).iterator(); it.hasNext(); )
    { t.append(quoteArg((postSubst((String) it.next()))));
      t.append(" ");
    }
    return t.toString().substring(0, t.length()-1);
  }

  public String linkToString(JShellLink l)
  {
    StringBuffer b = new StringBuffer();
    b.append(l.getFolder().replace('/', File.separatorChar) + " ");
    b.append(l.getName());
    b.append(" -to: " + l.getPath());
    b.append(" -start: " + l.getWorkingDirectory());
    b.append(" -args: " + l.getArguments());
    return b.toString();
  }

  public void Install()
  {
    installBut.setEnabled(false);
    if (desktop != null)
      desktop.setEnabled(false);

    installedOK = true;
    try
    {
      exit.setEnabled(false);
      installedOK = true;
      String postClass =
        props.getProperty(isWindows ? "-classwindows" : "-classunix");
      if (postClass == null)
        postClass = props.getProperty("-class");

      installDirName   = installDirField.getText();
      INSTALL          = installDirName.replace('\\', '/');
      if (!INSTALL.endsWith("/"))
        INSTALL += "/";
      
      String autoJar     = props.getProperty("-autojar");
      String autoIcon    = props.getProperty("-autoicon");
      String autoComment = props.getProperty("-comment");
      String autoCmd     = props.getProperty("-autocmd");
      String autoWork    = props.getProperty("-autodir");
      
      install = new File(INSTALL).getAbsoluteFile();
      
      jarPath = autoJar!=null 
              ? new File(install, autoJar).getAbsolutePath()
              : "NOAUTOJARSPECIFIED";

      unJar(
            installDirName,
            new FileInputStream(jarFileName),
            true
           );
           


      // DO NOT USE SUBSTITUTION UNTIL AFTER THIS POINT
      
      String shell       =
        postSubstCommandProperty(isWindows ? "-cmdwindows" : "-cmdunix");
      
      String shell1      =
        postSubstCommandProperty(isWindows ? "-cmdwindows1" : "-cmdunix1");
      
      String explore     = 
        postSubstCommandProperty("-explore");
      
      if (postClass != null)
      {
        Class theClass = Class.forName(postClass);
        showProgress("[Running post-install class " + postClass + "]");
        Object theObject = theClass.newInstance();
        if (runObject(theObject, INSTALL, props))
          showProgress("[Ran " + postClass + " successfully]");
      }

      if (isWindows && (autoInstall != null))
      {
        String windowsrun = postSubstCommandProperty("-windowsrun");
        String cmdPath =
          new File(install, autoInstall + ".cmd").getAbsolutePath();
        String cmdText = windowsrun==null 
                       ? "javaw.exe -jar \"" + jarPath + "\" %*%" 
                       : windowsrun;
        
        if (autoCmd != null)
          makeFile(autoCmd, cmdText);

        autoWork =
          (autoWork == null) ? INSTALL : substFor(
                                                  "%INSTALL%",
                                                  INSTALL,
                                                  autoWork
                                                 );
        String[] cmdargs = splitCmd(cmdText);
        
        JShellLink link = null;
        try
        {
          link = new JShellLink();
          link.setFolder(INSTALL);
          link.setName(autoInstall);
          link.setPath(cmdargs[0]);
          link.setArguments(cmdargs[1]);
          link.setWorkingDirectory(autoWork);
          link.setDescription((autoComment != null) ? autoComment : appName);
          if (autoIcon != null)
          {
            String iconPath = new File(install, autoIcon).getAbsolutePath();
            link.setIconLocation(iconPath);
            // showProgress("[Icon Path: "+iconPath+"]");
          }

          showProgress("[MAKING SHORTCUT " + linkToString(link) + "]");
          link.save();
        }
        catch (Exception ex)
        {
          ex.printStackTrace();
          showProgress("[FAILED TO MAKE SHORTCUT: " + ex + "]");
          installedOK = false;
        }

        if (desktop.isSelected())
          try
          {
            link.setFolder(JShellLink.getDirectory("desktop"));  // ();
            showProgress("[MAKING SHORTCUT " + linkToString(link) + "]");
            link.save();
          }
          catch (Exception ex)
          {
            ex.printStackTrace();
            showProgress("[FAILED TO MAKE DESKTOP SHORTCUT: " + ex + "]");
          }
      }

      if (isWindows)
      {
        for (int i = 1; i <= 20; i++)
        {
          String     shortcut = postSubstPathProperty("-shortcut" + i);
          String     comment = postSubstProperty("-comment" + i);
          String     args    = postSubstCommandProperty("-args" + i);
          String     path    = postSubstPathProperty("-to" + i);
          String     dir     = postSubstPathProperty("-dir" + i);
          String     icon    = postSubstPathProperty("-icon" + i);
          JShellLink cut     = new JShellLink();
          if (shortcut != null)
            try
            {
              if (path == null)
                path = postSubst("%INSTALL%");

              File   f    = new File(shortcut);
              String loc  = f.getParent();
              String name = f.getName();
              if (loc != null)
                cut.setFolder(loc);

              if (name != null)
                cut.setName(name);

              if (path != null)
                cut.setPath(path);

              if (args != null)
                cut.setArguments(args);

              if (dir != null)
                showProgress(dir);

              if (dir != null)
                cut.setWorkingDirectory((dir));
              else
                cut.setWorkingDirectory("");

              if (comment != null)
                cut.setDescription(comment);

              if (icon != null)
                cut.setIconLocation(icon);

              showProgress("[MAKING SHORTCUT (" + i + "): " + linkToString(cut)
                           + "]"
                          );
              cut.save();
            }
            catch (Exception ex)
            {
              ex.printStackTrace();
              showProgress("[FAILED TO MAKE SHORTCUT (" + i + "): "
                           + linkToString(cut) + "  (" + ex + ")]"
                          );
            }
        }
      }

      if (!isWindows && (autoInstall != null))
      {
        String cmdPath = new File(install, autoInstall).getAbsolutePath();
        String unixrun = postSubstCommandProperty("-unixrun");
        String cmdText = unixrun==null 
                       ? "exec java -jar -server \"" + jarPath + "\" \"$@\"" 
                       : unixrun;
        showProgress("[Making Shortcut]");
        installedOK   = installedOK && makeFile(cmdPath, cmdText);
        installedOK =
          installedOK
          && execCommand(new String[]
                         {
                           "/bin/chmod",
                           "+x",
                           cmdPath
                         }
                        );
        if (desktop.isSelected())
        {
          installedOK = installedOK && makeFile(HOMECMD, cmdText);
          installedOK =
            installedOK
            && execCommand(new String[]
                           {
                             "/bin/chmod",
                             "+x",
                             HOMECMD
                           }
                          );
        }

        if (!installedOK)
          showProgress("[FAILED TO MAKE SHORTCUT]");
      }

      if (shell != null)
      {
        shell         = shell.replaceAll("%INSTALL%", INSTALL);
        showProgress("[Running shell command: " + shell + "]");
        installedOK = installedOK && execute(shell, true);
        if (!installedOK)
          showProgress("[Shell command failed]");
      }

      if (shell1 != null)
      {
        shell1 = shell1.replaceAll("%INSTALL%", INSTALL);
        showProgress("[Running shell command: " + shell1 + "]");
        installedOK = installedOK && execute(shell1, true);
        if (!installedOK)
          showProgress("[Shell command failed]");
      }

      if (installedOK)
      {
        showProgress("\n\nTHE INSTALLER WILL CLEAN UP ON EXIT.");
        exit.setEnabled(true);
        exit.setText(props.getProperty("-finished", "Exit (and clean up) now"));
      }
      else
      {
        showProgress("\n\nTHE INSTALLATION WAS NOT SUCCESSFUL.");
        exit.setEnabled(true);
        exit.setText(props.getProperty("-finished", "Abandon the installation"));
      }

      if (isWindows && (explore != null))
      {
        execute(explore, true);
      }
    }
    catch (Exception exn)
    {
      exn.printStackTrace(System.err);
      showProgress("Exception during post-install phase " + exn);
    }
  }

  public void Exit()
  {
    System.err.println("[EXITING]");
    if (installedOK)
    {
      System.err.println("[CLEANING UP: " + INSTALL + "]");
      delete(new File(INSTALL + bootPath));
      deleteUpwards(new File(INSTALL + packagePath));
      deleteUpwards(new File(INSTALL + DUMMY.packagePath()));
      FileSet deletes =
        FileSet.fromList(props.getProperty(
                                           isWindows ? "-delwindows" : "-delunix",
                                           null
                                          )
                        );
      for (Iterator files = deletes.iterator(); files.hasNext();)
      {
        delete(new File(INSTALL + files.next()));
      }
    }
    else
    {
      System.err.println("[NO CLEANUP]");
    }

    System.exit(0);
  }

  public void actionPerformed(ActionEvent e)
  {
    String c = e.getActionCommand();
    if (c.startsWith("Install"))
      Install();
    else if (c.startsWith("Exit"))
      Exit();
  }

  /////////////////////////// JAR UNPACKING ////////////////////////////
  /**
     Ensure that the parent directory exists
   */
  static void mkParentDir(String path)
  {
    File dir    = new File(path);
    File parent = dir.getParentFile();
    if ((parent != null) && !parent.isDirectory())
    {
      parent.mkdirs();
    }
  }

  /**
     Unpack the jar file on the input stream into
     the directory named in rootpath.
   */
  static void unJar(String rootPath, FileInputStream in, boolean progress)
             throws IOException, ZipException
  {
    ZipInputStream zip    = new ZipInputStream(in);
    ZipEntry       entry  = null;
    byte[]         buffer = new byte[50 * 1024];
    while ((entry = zip.getNextEntry()) != null)
    {
      String name   = entry.getName();
      String prefix = rootPath;

      // Special cases
      if (name.endsWith(WINDLL))
      {
        prefix = tempPath;
        System.setProperty("JSHORTCUT_HOME", prefix);
        name = WINDLL;
        if (progress)
          showProgress(name + " ==> " + prefix + File.separator + name);
      }
      else if (name.startsWith("META-INF"))
        continue;
      else if (progress)
        showProgress(name);

      if (!prefix.endsWith(File.separator))
        prefix += File.separator;

      String path = prefix + name;
      if (entry.isDirectory())
      {
        // System.err.println("DIR: "+path);
      }
      else
      {
        // System.err.println("FILE: "+path);
        mkParentDir(path);
        FileOutputStream out = new FileOutputStream(path);
        int              len;
        while ((len = zip.read(buffer, 0, buffer.length)) >= 0)
        {
          out.write(buffer, 0, len);
        }

        out.close();
        zip.closeEntry();
      }
    }

    zip.close();
  }

  /////////////////////////// JAR PACKING ////////////////////////////
  public static class Jar
  {
    static HashSet dirs = new HashSet();

    public void addDirs(String path) throws IOException
    {
      if ((path != null) && !dirs.contains(path))
      {
        addDirs(new File(path).getParent());
        JarEntry entry = new JarEntry(path + "/");
        jar.putNextEntry(entry);
        System.err.println(entry);
        dirs.add(path);
      }
    }

    public Jar(String path) throws FileNotFoundException, IOException
    {
      this(new File(path), null);
    }

    public Jar(String path, String manifest)
        throws FileNotFoundException, IOException
    {
      this(new File(path), new File(manifest));
    }

    public Jar(File file, File man) throws FileNotFoundException, IOException
    {  // There seems to be a bug in the two-argument JarOutputStream constructor
       // It doesn't take lightly to simple manifests!
      jarFile   = file;
      jar       = new JarOutputStream(new FileOutputStream(file));
      jar.putNextEntry(new JarEntry("META-INF"));
      jar.putNextEntry(new JarEntry("META-INF/MANIFEST.MF"));
      System.err.print("META-INF/MANIFEST.MF ");
      write(man);
    }

    public void chDir(File currentDir)
    {
      this.currentDir = currentDir;
    }

    public void chDir(String currentDir)
    {
      this.currentDir = new File(currentDir);
    }

    JarOutputStream jar;
    File            jarFile;
    File            currentDir;  // Current location to which output is mapped
    Exception       problem;

    public void close() throws Exception
    {
      try
      {
        jar.close();
      }
      catch (IOException ex)
      {
        problem = ex;
      }

      if (problem != null)
        throw new Exception(
                            "Problem constructing Jar: "
                            + jarFile.getAbsolutePath(),
                            problem
                           );
    }

    public boolean canClose()
    {
      try
      {
        close();
        return true;
      }
      catch (Exception ex)
      {
        // ex.printStackTrace();
        return false;
      }
    }

    public void add(String path)
    {
      this.add(new File(path));
    }

    public void add(File file)
    {
      add(file, currentDir);
    }

    protected void write(File file) throws IOException
    {
      write(new FileInputStream(file));
    }

    protected void write(InputStream fis) throws IOException
    {
      {
        final byte[] buf = new byte[50 * 1024];
        int          len;
        int          tot = 0;
        while ((len = fis.read(buf)) >= 0)
        {
          jar.write(buf, 0, len);
          tot += len;
        }

        fis.close();
        System.err.println(tot);
      }
    }

    static String normalize(String path)
    {
      path = path.replace(File.separatorChar, '/');
      if (path.startsWith("./"))
        path = path.substring(2);

      return path;
    }

    public void add(java.net.URL url, File dest)
    {
      try
      {
        File   file          = new File(url.getFile());
        File   effective     =
          (dest == null) ? file : new File(
                                           dest,
                                           file.getName()
                                          );
        String effectivePath = normalize(effective.getPath());
        addDirs(new File(effectivePath).getParent());
        JarEntry entry = new JarEntry(effectivePath);

        //entry.setTime(file.lastModified());
        //entry.setSize(file.length());
        entry.setMethod(entry.DEFLATED);
        System.err.print(entry);
        System.err.print(" ");
        jar.putNextEntry(entry);
        write(url.openStream());
      }
      catch (Exception ex)
      {
        problem = ex;
        System.err.println("Problem adding URL: " + url);
      }
    }

    public void addDirectory(File file)
    {
      addDirectory(file, currentDir);
    }

    public void addDirectory(File file, File destDir)
    {
      if (file.isDirectory())
      {
        File[] files = file.listFiles();
        for (int i = 0; i < files.length; i++)
          add(files[i], destDir);
      }
    }

    public void add(File file, File dest)
    {
      try
      {
        File   effective     =
          (dest == null) ? file : new File(
                                           dest,
                                           file.getName()
                                          );
        String effectivePath = normalize(effective.getPath());
        addDirs(new File(effectivePath).getParent());
        if (file.isDirectory())
        {
          File[] files = file.listFiles();
          for (int i = 0; i < files.length; i++)
            add(files[i], effective);
        }
        else
        {
          JarEntry entry = new JarEntry(effectivePath);
          entry.setTime(file.lastModified());
          entry.setSize(file.length());
          entry.setMethod(entry.DEFLATED);
          System.err.print(entry);
          System.err.print(" ");
          jar.putNextEntry(entry);
          write(file);
        }
      }
      catch (Exception ex)
      {
        problem = ex;
        System.err.println("Problem adding file: " + file);
      }
    }
  }
  
  public static class Command extends StreamTokenizer 
  { public Command(String s) 
    { super(new StringReader(s)); 
      resetSyntax();
      whitespaceChars('\000', ' ');
      wordChars('!', '/');
      wordChars('0', '9');
      wordChars('?', '~');
      wordChars('=', '=');
      try
      {
        while (nextToken() != TT_EOF) symbols.add(sval);
      }
      catch (IOException ex) {}
    }
  
    Vector symbols = new Vector ();
  
    public Iterator iterator() { return symbols.iterator(); }
  }
}







