import java.io.*;

public class installjape 
{
   public static void main(String[] args) throws Exception
   { 
     PrintWriter out = new PrintWriter(new FileOutputStream("jape.cmd"));
     String javaPath = System.getProperty("java.home");
     out.println("SET PATH=\""+javaPath+"\\bin\";%PATH%");
     out.println("SET JAPESERVER=\"java -jar japeserver.jar\"");
     out.println("jape.exe %1%");
     out.close();
   }
}

