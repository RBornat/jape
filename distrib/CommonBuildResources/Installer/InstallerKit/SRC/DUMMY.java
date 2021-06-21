// Support for locating the ShellLink stuff without having to load it

package net.jimmc.jshortcut;
import java.io.File;
import java.net.URL;

public class DUMMY 
{
  public URL getLocation() 
  { 
    return DUMMY.class.getResource("JShellLink.class"); 
  }
  
  public static File packagePath() 
  {  String Class = DUMMY.class.getName();
     return new File(Class.substring(0, Class.lastIndexOf('.')).replace('.', '/'));
  } 
};
