import java.io.*;
import java.util.regex.*;
import java.util.*;

/**
<h1>
      UTF-8 stream editing.
</h1>

<p>
      (C) 2004, Collegium Vigorniensis Oxon. (All wrongs reversed)
</p>

<p>
      This is free software. Do what you want with it.
</p>

*/

public class tr
{
  public static void main(String[] args) throws Exception
  { String inputEncoding  = "UTF-8";
    String outputEncoding = "UTF-8";
    String substEncoding  = "UTF-8";
    String inputFileName  =  null;
    String outputFileName =  null;
    String substFileName  =  null;

    int i=0; 
    while (i<args.length)
    { String arg=args[i++];
      if ("-I".equals(arg))
         inputEncoding = args[i++];
      else
      if ("-O".equals(arg))
         outputEncoding = args[i++];
      else
      if ("-S".equals(arg))
         substEncoding = args[i++];
      else
      if ("-i".equals(arg))
         inputFileName = args[i++];
      else
      if ("-o".equals(arg))
         outputFileName = args[i++];
      else
      if ("-s".equals(arg))
         substFileName = args[i++];
      else
      if ("--".equals(arg))
         break;
      else
      if (arg.startsWith("-"))
         doHelp();
      else
      { i--;
        break;
      }
    }

    int argRest = args.length-i;
    Vector pats = new Vector();
    Vector reps = new Vector();
    boolean wellformed = true;
    

    if (substFileName!=null)
    { LineNumberReader substs = new LineNumberReader(new InputStreamReader(new FileInputStream(substFileName), substEncoding));
      String line = null;
      while ((line=substs.readLine())!=null)
      { line = line.trim();
        if (line.startsWith("#")) continue;
        else 
        if (line.length()>0)
        { char split=line.charAt(0);
          String[] fields = line.split("\\x"+Integer.toHexString(split)); // Ye gods, what we do to avoid work!
          if (fields.length<3) 
          {  wellformed = false;
             System.err.println("Warning: ("+substFileName+":"+substs.getLineNumber()+") incomplete specification: "+line);
          }
          else
          { try
            { pats.add(Pattern.compile(fields[1]));
              reps.add(replacement(fields[2]));
            }
            catch (PatternSyntaxException ex)
            { wellformed = false;
              System.err.println("Warning: ("+substFileName+":"+substs.getLineNumber()+") "+ex.getMessage());
            }
          }
        }
      }
      substs.close();
    }


    if (argRest % 2 != 0)
    { System.err.println("Error: unequal number of patterns and replacements on command line.");
      System.exit(1);    
    }

    try
    {
      for (int p=i; p<argRest; p+=2)
      { pats.add(Pattern.compile(args[p]));
        reps.add(replacement(args[p+1]));
      }
    }
    catch (PatternSyntaxException ex)
    { wellformed = false;
      System.err.println("Warning (command line): "+ex.getMessage());
    }

    if (!wellformed)
    { 
       System.exit(1);
    }
    
    Pattern[] pat = (Pattern[]) pats.toArray(new Pattern[0]);
    String [] rep = (String []) reps.toArray(new String[0]);

    InputStream  in  = System.in;
    OutputStream out = System.out;

    try
    { if (inputFileName!=null)  in  = new FileInputStream(inputFileName);
      if (outputFileName!=null) out = new FileOutputStream(outputFileName);
    }
    catch (Exception ex)
    {
       System.err.println("Error opening files: "+ex.getMessage());
    }
    
    translate(inputEncoding, outputEncoding, in, out, pat, rep);       
  }

  public static void translate
  ( String       inputEncoding
  , String       outputEncoding
  , InputStream  in
  , OutputStream out
  , Pattern[]    pat
  , String[]     rep
  )
  throws UnsupportedEncodingException, IOException
  { translate
    ( new LineNumberReader(new InputStreamReader(in, inputEncoding))
    , new PrintWriter(new OutputStreamWriter(out, outputEncoding))
    , pat
    , rep
    );
  }
  
  public static void translate
  ( LineNumberReader in
  , PrintWriter      out
  , Pattern[]        pat
  , String[]         rep
  )
  throws UnsupportedEncodingException, IOException
  {
    String line;
    while ((line=in.readLine())!=null)
    { 
      for (int i=0; i<pat.length; i++)
          line=pat[i].matcher(line).replaceAll(rep[i]);
      out.println(line);
      out.flush();
    }
    in.close();
    out.close();
  }

  static String replacement(String s)
  throws PatternSyntaxException
  { if (s.indexOf('\\')<0) return s;
    StringBuffer b = new StringBuffer();
    int i=0;
    int l=s.length();
    while (i<l) 
    { char c = s.charAt(i++);
      if (c=='\\')
      { char d=s.charAt(i++);
        switch (d)
        { case 'u':
            if (i+4>l)
            { throw new PatternSyntaxException
              ("Short unicode escape in replacement: ", s, i);
            }
            try
            {
              d = (char) Integer.parseInt(s.substring(i, i+4), 16);
            }
            catch (NumberFormatException ex)
            { throw new PatternSyntaxException
              ("Bad unicode escape (\\u"+s.substring(i, i+4), s, i);
            }
            i+=4;
            break;            
            
          case 'x':
            if (i+2>l)
            { throw new PatternSyntaxException
              ("Short hex escape in replacement: ", s, i);
            }
            try
            {
              d = (char) Integer.parseInt(s.substring(i, i+2), 16);
            }
            catch (NumberFormatException ex)
            { throw new PatternSyntaxException
              ("Bad hex escape (\\x"+s.substring(i, i+2), s, i);
            }
            i+=2;
            break;
            
          case '0':
            if (i+3>l)
            { throw new PatternSyntaxException
              ("Short octal escape in replacement: ", s, i);
            }
            try
            {
              d = (char) Integer.parseInt(s.substring(i, i+3), 8);
            }
            catch (NumberFormatException ex)
            { throw new PatternSyntaxException
              ("Bad octal escape (\\0"+s.substring(i, i+3), s, i);
            }
            i+=3;
            break;
            
          case '\\': d='\\';            break;
          case 't':  d='\t';            break;
          case 'n':  d='\n';            break;
          case 'r':  d='\r';            break;
          case 'f':  d='\f';            break;
          case 'a':  d='\u0007';        break; // alert-bell
          case 'e':  d='\u001B';        break; // escape
          
          default:
            { throw new PatternSyntaxException
              ("Unrecognised escape (\\"+d+")", s, i);
            }
        }
        c=d;
      }
      b.append(c);
    }
    return b.toString();
  }

  static void doHelp()
    { 
      System.err.println("Usage: java tr [-S specenc | -I inputenc | -O outputenc | -i specfile | -i infile | -o outfile] [--] [pat rep]*");
      System.err.println("Examples:");
      System.err.println("      java -jar tr.jar  '(\\D)(\\d\\d*)' '$1\\$$2'  prefix all digit sequences with a $.");
      System.err.println("      java -jar tr.jar  '(\\D)(\\d+)'    '$1\\$$2'  ditto.");
      System.err.println("      java -jar tr.jar  '\\u2ae0' '\\u207b\\u00b9'  translate inverted perpendicular into superscript -1.");
      System.err.println("      java -jar tr.jar  -- -I -O                 occurences of -I into -O.");

      System.exit(0);
    }

}





