/* 
        Copyright © 2003-17 Richard Bornat & Bernard Sufrin
     
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

public class PosIntHashMap {
    private int[] keys, values;
    private int size, load;
    
    public PosIntHashMap() {
	this((int)127);
    }
    
    public PosIntHashMap(int size) {
	init(size);
    }
    
    private void init(int size) {
	while (!prime(size)) size++;
	keys = new int[size];
	for (int i=0; i<size; i++) keys[i]=-1;
	values = new int[size];
	this.size = size;
	this.load = 0;
    
    }
    
    private boolean prime(int n) {
	int lim = ((int)Math.sqrt((double)n))+1;
	for (int i=2; i<=lim; i++)
	    if (n%i==0) return false;
	return true;
    }
    
    private int hash(int key) {
	return key%size;
    }
    
    public void set(int key, int value) {
	int i = hash(key);
	for (int k=keys[i]; k!=key; k=keys[i=(i+1)%size])
	    if (k<0) {
		keys[i]=key; values[i]=value; load++;
		if (size/load<2) rehash(size*2+1);
		return;
	    }
	
	// already keyed
	values[i]=value;
    }
    
    public void rehash(int size) {
	int[] keys = this.keys;
	int[] values = this.values;
	int len = keys.length;
	
	init(size);
	for (int i=0, k=keys[i]; i<len; k=keys[i++])
	    if (k>=0) 
		set(k, values[i]);
    }
    
    public int get(int key) {
	int i = hash(key);
	for (int k=keys[i]; k!=key; k=keys[i=(i+1)%size])
	    if (k<0) 
		return -1;
	return values[i];
    }
}
