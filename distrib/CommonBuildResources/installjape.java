import java.io.*;
import bootstrap.*;

/**
        This java class is loaded and instantiated (once) dynamically
        by the generic bootstrap installer when the installation
        files have been unpacked and the install directory has been
        chosen.

        On instantiation it makes a command file that does 
        the final Windows shortcut construction for Jape.

        This complexity is needed because we want to make a shortcut that
        starts the Jape.jar file in the examples directory. Getting
        the various levels of quotation needed to call the shortcut
        binding program correct within a cmd script would be
        utterly dementing.
*/

public class installjape 
{
   static String javaPath    = System.getProperty("java.home");
   static String currentPath = System.getProperty("user.dir");
   static String fsep        = System.getProperty("file.separator");
   static String INSTALL     = System.getProperty("INSTALL");

   static String QUOTEDINSTALLFILE(String path)
   { return "\\\""+INSTALL + fsep + path+"\\\""; }
   
   static String INSTALLFILE(String path)
   { return "\""+INSTALL + fsep + path+"\""; }
   
   /** When run from the command line this makes the right command files for a WIndows installation */
   public static void main(String[] args) throws Exception
   { 
     PrintWriter out = new PrintWriter(new FileOutputStream(INSTALL+fsep+"installjape.cmd"));
     out.println("REM Making a shortcut to Jape");
     out.println("cd \""+INSTALL+"\"");
     out.println("SET PATH=\""+javaPath+"\\bin\";%PATH%");
     out.println("SET PATH=\""+INSTALL+"\";%PATH%");
     out.println("shortcut -t javaw -n jape -d examples -a \"-jar "+QUOTEDINSTALLFILE("Jape.jar")+" -engine "+QUOTEDINSTALLFILE("jape.exe")+"\" -i "+INSTALLFILE("japeicon.ico"));
     out.println("REM You may now remove all files other than examples, Jape.jar, jape.lnk, jape.exe");
     out.close();
     // install.execute(INSTALL+fsep+"installjape.cmd", true);
   }

   /** When instantiated from the installer this makes the right command file */
   public installjape() throws Exception { main(null); }
}







