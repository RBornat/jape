
/**

        A text area with additional functionality, namely:  logging
        of the material as it appears on a given (dynamically
        generated) input stream.

        We can also append material to the end of the log, and clear the log.

        $Id$
        
*/
import java.io.*;
import java.awt.*;

public class ErrorLog extends TextArea
{   
    private int length;

    /** Output onto the text area */
    public  void print(String str)
    { this.insert(str, length);
      length += str.length();
    }
    
    /** Output a line onto the text area */
    public  void println(String str)
    { this.print(str);
      this.print("\n");
      setFont(Font.decode("monospaced-PLAIN-12"));
    }
    
    /** Clear the log */
    public void clear()
    { 
      replaceRange("", 0, length);
      length = 0;
    }

    /** make a log of the same default size as a TextArea */
    public ErrorLog()
    {  super();
       length = 0;
    }
    
    /** make a log of a specific size */
    public ErrorLog(int rows, int columns) // like the parent, ie. counterconventional
    {  super(rows, columns);
       length = 0;
       setFont(Font.decode("monospaced-PLAIN-12"));
    }

    /** Start a process that copies lines from the given input stream to the log.
        More than one of these processes can be active simultaneously.
    */
    public void echo(final InputStream inp)
    {
       new Thread()
       { public void run()
         {    String           line = null;
              LineNumberReader i    = new LineNumberReader(new InputStreamReader(inp));
              try
              {
                   while ((line = i.readLine()) != null)
                   {
                     println(line);
                   }
                   i.close(); 
              } 
              catch (Exception e) {}
         }
       }.start();
    }

    /** Test by running a command and logging both output streams */
    public static void main (String[] args)
    { Frame    f   = new Frame();
      ErrorLog e   = new ErrorLog(60, 80);
      Runtime  sys = Runtime.getRuntime();
      f.add(e);
      f.pack();
      f.show();
      try 
      {
         Process p = sys.exec(args);
         e.echo(p.getInputStream()); 
         e.echo(p.getErrorStream());
         int status = p.waitFor();
      }
      catch (Exception exn)
      {
        System.err.println("[exception "+exn+"]");
      }
    }
}
