/* 
    $Id$

    Copyright © 2003-5 Richard Bornat & Bernard Sufrin
     
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

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;

import java.util.Vector;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

public class Logger {
    private static final boolean divertToWindow = true;
    
    private static final JTextArea textArea = new JTextArea("", 24, 80);

    private static class ScreenWriter extends Writer {
	// for the moment, the minimum
	public void write(String s) {
	    if (divertToWindow)
		textArea.append(s);
	    else {
		System.err.print(s);
		System.err.flush();
	    }
	}
	public void write(char cs[], int off, int len) {
	    write(new String(cs, off, len));
	}
	public void flush() { }
	public void close() {
	    Alert.showAlert(Alert.Plain, "log window closed?");
	}
    }

    private static Writer screenWriter = new ScreenWriter();
    public static PrintWriter log = new PrintWriter(screenWriter);

    public static interface LineReader {
	public String readLine();
    }

    public static class StreamLog extends Thread {
	public final String id;
	private final LineReader lineReader;
	public StreamLog(String id, LineReader lineReader) {
	    super();
	    this.id = id; this.lineReader = lineReader;
	}
	public void run() {
	    String line = null;
	    try {
		while ((line = lineReader.readLine()) != null)
		    log.println(line);
	    }
	    catch (Exception exn) {
		log.println("StreamLog "+JapeUtils.enQuote(id)+" got exception "+exn);
	    }
	}
    }

    // System.err loopback

    private static final PrintStream savedErr = System.err;
    private static boolean diverted = false;
    
    static {
	class Queue {
	    // the world's worst queue
	    Vector q = new Vector();
	    public synchronized void enqueue(byte[] buf) {
		q.add(buf); notifyAll();
	    }
	    public synchronized byte[] dequeue() throws EOFException {
		while (q.size()==0)
		    try {
			wait();
		    } catch (InterruptedException exn) {
			throw new EOFException();  // and die
		    }
		return (byte[])q.remove(0);
	    }
	    public synchronized int available() {
		int n = 0;
		for (int i=0; i<q.size(); i++)
		    n+=((byte[])q.get(i)).length;
		return n;
	    }
	}

	final Queue queue = new Queue();

	class Outer extends OutputStream {
	    final int BUFSIZ = 1024;
	    private byte [] buf = new byte[BUFSIZ];
	    private int bufindex = 0;
	    private boolean closed = false;
	    public void close() {
		flush(); buf = null; closed = true;
	    }
	    public void flush() {
		if (bufindex!=0) {
		    if (DebugVars.loopback_tracing)
			log.println("Engine.Outer flushes "+bufindex);
		    byte[] qbuf = new byte[bufindex];
		    for (int i=0; i<bufindex; i++)
			qbuf[i] = buf[i]; // until I can find a BLT method
		    queue.enqueue(qbuf);
		    bufindex = 0;
		}
	    }
	    public void write(byte[] b) {
		write(b, 0, b.length);
	    }
	    public void write(byte[] b, int off, int len) {
		if (DebugVars.loopback_tracing)
		    log.println("Engine.Outer writes "+len+" bytes");
		if (!closed) {
		    while (len!=0) {
			int n = Math.min(BUFSIZ-bufindex, len);
			for (int i=0; i<n; i++)
			    buf[bufindex+i] = b[off+i]; // until I can find a BLT method
			bufindex+=n; off+=n; len-=n;
			if (bufindex==BUFSIZ)
			    flush();
		    }
		}
	    }
	    public void write(int b) {
		if (DebugVars.loopback_tracing)
		    log.println("Engine.Outer writes single byte "+b);
		if (bufindex==BUFSIZ)
		    flush();
		buf[bufindex++] = (byte)b;
	    }
	}

	final OutputStream outer = new Outer();

	class Inner extends InputStream {
	    private byte[] buf = null;
	    private int bufindex = 0;
	    public synchronized int available() {
		int n = (buf==null ? 0 : buf.length-bufindex)+queue.available();
		if (DebugVars.loopback_tracing)
		    log.println("Engine.Inner says "+n+" bytes available");
		return n;
	    }
	    private synchronized void fillbuf() throws EOFException {
		if (buf==null || bufindex==buf.length) {
		    buf=queue.dequeue(); bufindex=0;
		    if (DebugVars.loopback_tracing)
			log.println("Engine.Inner dequeues "+buf.length+" bytes");
		}
	    }
	    public synchronized int read() throws EOFException {
		while (buf==null || bufindex==buf.length)
		    fillbuf();
		int c = (int)buf[bufindex++];
		if (DebugVars.loopback_tracing)
		    log.println("Engine.Inner reads single byte "+c);
		return c;
	    }
	    public synchronized int read(byte[] b) {
		return read(b, 0, b.length);
	    }
	    public synchronized int read(byte[] b, int off, int len) {
		try {
		    fillbuf();
		} catch (EOFException e) {
		    if (DebugVars.loopback_tracing)
			log.println("Engine.Inner.read sees EOF");
		    return -1;
		}
		int n = Math.min(buf.length-bufindex, len);
		for (int i=0; i<n; i++)
		    b[off+i] = buf[bufindex+i];
		bufindex+=n;
		if (n==len || available()==0) {
		    if (DebugVars.loopback_tracing)
			log.println("Engine.Inner reads "+n+" bytes");
		    return n;
		}
		else {
		    if (DebugVars.loopback_tracing)
			log.println("Engine.Inner reads "+n+" bytes, and then ...");
		    return n+read(b, off+n, len-n);
		}
	    }
	    public long skip(long n) {
		// no sleeping?	 The spec doesn't say, so I'll implement sleeping
		if (n<=0) {
		    if (DebugVars.loopback_tracing)
			log.println("Engine.Inner skips nothing");
		    return n;
		}
		else {
		    try {
			fillbuf();
		    } catch (EOFException e) {
			if (DebugVars.loopback_tracing)
			    log.println("Engine.Inner.skip sees EOF");
			return 0;
		    }
		    long k = Math.min(buf.length-bufindex, n);
		    bufindex+=k;
		    if (k==n) {
			if (DebugVars.loopback_tracing)
			    log.println("Engine.Inner skips "+k+" bytes");
			return k;
		    }
		    else {
			if (DebugVars.loopback_tracing)
			    log.println("Engine.Inner skips "+k+" bytes, and then ...");
			return k+skip(n-k);
		    }
		}
	    }
	}

	final InputStream inner = new Inner();

	class InLiner implements LineReader {
	    private BufferedReader in;
	    public InLiner(InputStream in) {
		super();
		this.in = new BufferedReader(new InputStreamReader(in));
	    }
	    public String readLine() {
		try {
		    return in.readLine();
		} catch (Exception exn) {
		    return null;
		}
	    }
	}

	Thread inLogger = new StreamLog("System.err loopback", new InLiner(inner));
	inLogger.start();

	if (divertToWindow) {
	    PrintStream errStream = new PrintStream(outer, true);
	    System.setErr(errStream);
	    diverted = true;
	    // System.err.println("this am a test!");
	}
	else
	    System.err.println("Straightout error interface (not diverted to window)");
    }

    public static class LogWindow extends JapeWindow {
	public LogWindow() {
	    super("Jape console log");
	    getContentPane().add(new JScrollPane(textArea));
	    setBar(); // why not?

	    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
	    addWindowListener(new WindowAdapter() {
		public void windowClosing(WindowEvent e) {
		    setVisible(false);
		}
	    });
	}

	public int getBarKind() {
	    return JapeMenu.OTHERWINDOW_BAR;
	}
	
	public void setVisible(boolean state) {
	    if (state)
		{ JapeFont.setComponentFont(textArea, JapeFont.LOGWINDOW);
		  // (Re)calculate size	 (BS)
		  pack();
		}
	    super.setVisible(state);
	    if (state)
		toFront();
	}

	protected boolean servesAsControl() { return false; }
    }

    private static LogWindow logWindow;

    public static void init() {
	logWindow = new LogWindow();
    }

    public static void font_reset() {
	if (logWindow!=null) {
	    boolean visible = logWindow.isVisible();
	    //logWindow.dispose();
	    //init();
	    // Trust me, I'm a programmer (BS)
	    logWindow.setVisible(true);
	    logWindow.setVisible(visible);
	}
    }

    public static void crash(String message, int val) {
	if (diverted) {
	    System.err.flush();
	    System.setErr(savedErr);
	    System.err.println(textArea.getText());
	}
	System.err.println(message);
	System.exit(val);
    }
}



