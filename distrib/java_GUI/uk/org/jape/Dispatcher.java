//
//  dispatcher.java
//  japeserver
//
//  Created by Richard Bornat on Fri Aug 09 2002.
//  Copyright (c) 2002 Richard Bornat. All rights reserved (for the moment, till I get it copylefted).
//

import java.io.*;
import java.util.*;

public class Dispatcher extends Thread {

    protected BufferedReader in;
    boolean tracing = false;
    AboutBox aboutbox;
    japeserver boss;

    public Dispatcher(japeserver boss, AboutBox aboutbox) {
        super("Dispatcher");
        in = new BufferedReader(new InputStreamReader(System.in));
        this.boss = boss;
        this.aboutbox = aboutbox;
        if (tracing)
            System.err.println("dispatcher initialised");
    }

    private void showcommand(String m, String[] command) {
        System.err.print(m+" [");
        for (int i=0; i<command.length-1; i++)
            System.err.print("\""+command[i]+"\" ");
        System.err.println("\""+command[command.length-1]+"\"]");
    }
    
    public void run() {
        String line;
        try {
            while ((line=in.readLine())!="") {
                if (tracing)
                    System.err.println("dispatcher reads "+line);
                String[] command = japesplit(line);
                if (tracing) {
                    showcommand("which is, being translated,", command);
                }
                String p = command[0];
                int len = command.length;
                if (p.equals("VERSION")&&len==2)
                    aboutbox.setVersion(command[1]);
                else
                if(p.equals("SETINVISCHARS")&&len==9)
                    boss.setinvischars(command[1].charAt(0),command[2].charAt(0),
                        command[3].charAt(0),command[4].charAt(0),
                        command[5].charAt(0),command[6].charAt(0),
                        command[7].charAt(0),command[8].charAt(0));
                else
                if (p.equals("OPERATORSBEGIN")&&len==1)
                    boss.operatorsbegin();
                else
                if (p.equals("OPERATOR")&&len==2)
                    boss.addoperator(command[1]);
                else
                if (p.equals("OPERATORSEND")&&len==1)
                    boss.operatorsend();
                else
                    System.err.println("**** dispatcher doesn't understand "+line);
            }
        } catch (IOException e) {
            System.err.println("japeserver crash: dispatcher fails with IOException "+e);
        }
    }

    private String[] japesplit(String line) {
        // split a line encoded in the japeserver obol form.
        Vector result = new Vector();
        StringBuffer buf   = new StringBuffer();
        int i     = 0;
        boolean quote = false;
        boolean dbquote=false;
        int len = line.length();
        
        while (i<len) {
            char c=line.charAt(i);

            i=i+1;
            switch (c) {
                case '{': {
                    if (quote) {
                        result.add(buf.toString());
                        buf = new StringBuffer();
                        quote = false;
                    }
                    else
                        quote = true;
                    break;
                }
                case '"': {
                    dbquote = !dbquote;
                    break;
                }
                case ' ': {
                    if (!quote) {
                        if (buf.length()!=0)
                        result.add(buf.toString());
                        buf = new StringBuffer();
                    }
                    else
                        buf.append(c);
                    break;
                }
                case '\\': {
                    c=line.charAt(i);
                    i=i+1;
                    if (c=='n')
                        buf.append('\n');
                    else
                    if ('0' <= c && c <= '8') { 
                        int n=((c-'0')*8+line.charAt(i)-'0')*8+line.charAt(i+1)-'0';
                        i=i+2;
                        buf.append((char)n);
                    }
                    else
                        buf.append(c);
                    break;
                }
                default: {
                    buf.append(c);
                    break;
                }
            }
        }
        
        if (buf.length()!=0) {
            result.add(buf.toString());
        }
            
        return ((String[])result.toArray(new String[result.size()]));
    }
}
