//
//  Alert.java
//  japeserver
//
//  Created by Richard Bornat on Tue Sep 03 2002.
//  Copyright (c) 2002 __MyCompanyName__. All rights reserved.
//

import java.util.*;
import javax.swing.*;

public class Alert {
    // oh the ceaseless dance of interface conversions ..
    public static final int Info = JOptionPane.INFORMATION_MESSAGE;
    public static final int Warning = JOptionPane.WARNING_MESSAGE;
    public static final int Error = JOptionPane.ERROR_MESSAGE;
    public static final int Question = JOptionPane.QUESTION_MESSAGE;
    
    private static int messagekind(int severity) throws ProtocolError {
        switch (severity) {
          case 0: return Info;
          case 1: return Warning;
          case 2: return Error;
          case 3: return Question;
          default: throw (new ProtocolError(severity+" should be message severity (0:info, 1:warning, 2:error, 3: question)"));
        }
    }
    
    public static void showInfoMessage(int messagekind, String message) {
        // I don't think this needs invokeLater ...
        JOptionPane.showMessageDialog(null,message,null,messagekind);
    }
    
    // this doesn't deal with fonts yet ... I think we have to make a Component (sigh)
    // and I haven't worked out what to do with defaultbutton ...
    public static void newAlert (Vector buttons, int severity, String message, int defaultbutton) 
    throws ProtocolError {
        if (buttons.size()==1 && ((String)buttons.get(0)).equals("OK")) {
            showInfoMessage(messagekind(severity), message);
        }
        else {
            String s = "can't yet show alert: [";
            for (int i=0; i<buttons.size(); i++) 
                s=s+(i==0?"\"":",\"")+((String)buttons.get(i))+"\"";
            showInfoMessage(Error, s+" "+severity+" \""+message+"\" "+defaultbutton);
        }
    }
}
