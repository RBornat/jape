// 
// $Id$
//
//  Copyleft 2002 Richard Bornat & Bernard Sufrin. Proper GPL text to be inserted
//

public class AboutBox {
    protected static String version = null;
    
    // for the moment we do something dead simple ...
    public static void showAboutBox() {
        Alert.showAlert(Alert.Plain, 
            "This is the platform-independent interface to the Jape proof engine"+
                (version==null ? "" : ", working with "+version));
    }
	
    public static void setVersion(String _version) {
        version = _version;
    }
	
}