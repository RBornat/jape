import java.io.*;
import bootstrap.*;

/**
        Makes a command file that does the final Windows shortcut construction for Jape.
        Executes the command file.
*/

public class installjape 
{
   static String javaPath    = System.getProperty("java.home");
   static String currentPath = System.getProperty("user.dir");
   static String fsep        = System.getProperty("file.separator");
   static String INSTALL     = System.getProperty("INSTALL");
   static String dot(String path)
   { return "\\\""+INSTALL + fsep + path+ "\\\""; }
   
   /** When run from the command line this makes the right command files for a WIndows installation */
   public static void main(String[] args) throws Exception
   { 
     PrintWriter out = new PrintWriter(new FileOutputStream(INSTALL+fsep+"installjape.cmd"));
     out.println("REM Making a shortcut to Jape");
     out.println("cd \""+INSTALL+"\"");
     out.println("SET PATH=\""+javaPath+"\\bin\";%PATH%");
     out.println("shortcut -t javaw -n jape -d examples -a \"-jar "+dot("Jape.jar")+" -engine "+dot("jape.exe")+"\" -i japeicon.ico");
     out.println("REM You may now remove all files other than examples, Jape.jar, jape.lnk, jape.exe");
     out.close();
     install.execute(INSTALL+fsep+"installjape.cmd", true);
   }

   /** When instantiated from the installer this makes the right command files */
   public installjape() throws Exception { main(null); }
}






