/* 
        Copyright © 2003-19 Richard Bornat & Bernard Sufrin
     
	richard@bornat.me.uk
	sufrin@comlab.ox.ac.uk

    This file is part of the Jape GUI, which is part of Jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).
    
*/
package uk.org.jape;

import java.io.File;
import java.io.FilenameFilter;

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import java.util.Vector;

import javax.swing.filechooser.FileFilter;

public class JapeFileFilter extends FileFilter implements FilenameFilter {

    protected String description;
    protected Vector<Pattern> pats = new Vector<Pattern>();
    
    public JapeFileFilter(String description) {
	super();
	this.description = description;
    }
    
    public void addExtension(String extension) {
	addPattern(".*\\."+extension);
    }
    
    public void addPattern(String pattern) {
	try {
	    pats.add(Pattern.compile(pattern));
	} catch (PatternSyntaxException e){
	    Alert.showErrorAlert("bad file filter pattern\n"+e.getMessage());
	}
    }
    
    public boolean accept(File f) { 
	if (f.isDirectory() || pats.size()==0)
	    return true;
	String name = f.getName();
	
	for (int i=0; i<pats.size(); i++) 
	    if (((Pattern)pats.get(i)).matcher(name).matches())
		return true;
	
	return false;
    }
    
    public boolean accept(File dir, String name) {
	return accept(new File(dir, name));
    }
    
    public String getDescription() { 
	return description;
    }
}
