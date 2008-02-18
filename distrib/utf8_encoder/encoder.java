/**
<h1>
      Konstanz/Laura/UTF file translation.
</h1>

<p>
      This is free software. Do what you want with it.
</p>

<pre>
$Id$	 
</pre>

*/

import java.io.*;
import java.nio.charset.Charset;
// import java.util.regex.*;
import java.util.*;

public class encoder {
    
    static boolean writeBOM = true;
    
    public static void main(String[] args) throws Exception { 
	String inputEncoding  = "K";
	String outputEncoding = "UTF-8";
	String inputFileName  =	 null;
	String outputFileName =	 null;

	int i=0; 
	while (i!=args.length) { 
	    String arg=args[i++];
	    if ("-I".equals(arg)) {
		if (i+2<=args.length) {
		    inputEncoding = args[i++];
		    if (inputFileName==null) 
			inputFileName = args[i++];
		    else {
			System.err.println("too many inputfiles ("+inputFileName+", "+args[i++]+")");
			doHelp();
		    }
		}
		else {
		    System.err.println("-I needs inputencoding and inputfile");
		    doHelp();
		}
	    }
	    else
	    if ("-O".equals(arg)) {
		if (i+2<=args.length) {
		    outputEncoding = args[i++];
		    if (outputFileName==null) 
			outputFileName = args[i++];
		    else {
			System.err.println("too many outputfiles ("+outputFileName+", "+args[i++]+")");
			doHelp();
		    }
		}
		else {
		    System.err.println("-O needs outputencoding and inputfile");
		    doHelp();
		}
	    }
	    else
	    if ("-nobom".equals(arg) || "-noBOM".equals(arg)) 
		writeBOM = false;
	    else
	    if ("-help".equals(arg))
		doHelp();
	    else 
	    if (inputFileName==null) 
		inputFileName = arg;
	    else {
		System.err.println("too many inputfiles ("+inputFileName+", "+arg+")");
		doHelp();
	    }
	}

	String[] inputTranslation=null, outputTranslation=null;
	SortedMap map = Charset.availableCharsets();
	
	if ("K".equals(inputEncoding) || "Konstanz".equals(inputEncoding)) {
	    inputTranslation = Konstanz; inputEncoding = "ISO-8859-1";
	}
	else
	if ("L".equals(inputEncoding) || "Laura".equals(inputEncoding)) {
	    inputTranslation = Laura; inputEncoding = "ISO-8859-1";
	}
	else
	if ("U8".equals(inputEncoding) || "UTF8".equals(inputEncoding)) {
	    inputTranslation = null; inputEncoding = "UTF-8";
	}
	else
	if ("U16".equals(inputEncoding) || "UTF16".equals(inputEncoding)) {
	    inputTranslation = null; inputEncoding = "UTF-16";
	}
	
	if (!map.containsKey(inputEncoding)) {
	    System.err.println("unimplemented input encoding "+inputEncoding);
	    System.exit(2);
	}
	
	if ("K".equals(outputEncoding) || "Konstanz".equals(outputEncoding)) {
	    outputTranslation = Konstanz; outputEncoding = "ISO-8859-1";
	}
	else
	if ("L".equals(outputEncoding) || "Laura".equals(outputEncoding)) {
	    outputTranslation = Laura; outputEncoding = "ISO-8859-1";
	}
	else
	if ("U8".equals(outputEncoding) || "UTF8".equals(outputEncoding)) {
	    outputTranslation = null; outputEncoding = "UTF-8";
	}
	else
	if ("U16".equals(outputEncoding) || "UTF16".equals(outputEncoding)) {
	    outputTranslation = null; outputEncoding = "UTF-16";
	}
	
	if (!map.containsKey(outputEncoding)) {
	    System.err.println("unimplemented output encoding "+outputEncoding);
	    System.exit(2);
	}
	
	InputStream  in	 = System.in;
	OutputStream out = System.out;
	
	try { 
	    if (inputFileName!=null)  in  = new FileInputStream(inputFileName);
	    if (outputFileName!=null) out = new FileOutputStream(outputFileName);
	}
	catch (Exception ex) {
	    System.err.println("Error opening files: "+ex.getMessage());
	    System.exit(2);
	}
	
	translate(inputTranslation, inputEncoding.startsWith("UTF"), 
		  outputTranslation, outputEncoding.startsWith("UTF"),
		  new BufferedReader (new InputStreamReader(in, inputEncoding)), 
		  new PrintStream (out, true, outputEncoding));
    }

    public static void translate(String[] inputTranslation, boolean inUTF,
				 String[] outputTranslation, boolean outUTF,
				 BufferedReader in, PrintStream out)
    throws UnsupportedEncodingException, IOException { 
	String line;
	int linenum = 0;
	
	while ((line=in.readLine())!=null) {
	    linenum++;
	    if (linenum==1 && inUTF && line.startsWith("\ufeff"))
		line = line.substring(1);

	    if (inputTranslation!=null)
		line = translateLine(line, inputTranslation, true, linenum);
	    if (outputTranslation!=null)
		line = translateLine(line, outputTranslation, false, linenum);
	    
	    if (linenum==1 && writeBOM && outUTF)
		out.print("\ufeff");

	    out.println(line);
	}
	in.close();
	out.close();
    }
      
    public static String translateLine(String line, String[] tr, boolean l2r, int linenum) { 
	StringBuffer buf = new StringBuffer();
	int line_i=0 /* , buf_i=0 */;
	int line_lim=line.length(), tr_lim=tr.length;
	int source = l2r ? 0 : 1, dest = 1-source;
	
	while (line_i<line_lim) { 
	    String dest_string = null;
	    int tr_i=0;
	    while (tr_i<tr_lim) {
		if (line.startsWith(tr[tr_i+source],line_i)) {
		    dest_string = tr[tr_i+dest];
		    line_i += tr[tr_i+source].length();
		    break;
		}
		else
		    tr_i+=2;
	    }
	    if (dest_string!=null) {
		buf.append(dest_string);
	    }
	    else {
		char c = line.charAt(line_i++);
		if (c<=0x7f)
		    buf.append(c);
		else {
		    System.err.println((l2r ? "internal error" : "invalid input")+
				       " -- can't translate character 0x"+Integer.toHexString(c)+
				       " (line "+linenum+":"+(line_i+1)+")");
		    buf.append('?');
		}
	    }
	}

	return buf.toString();
    }

    static void doHelp() { 
	System.err.println("Usage: java encoder [-I <inputenc> infile | -O <outputenc> outfile | -nobom | infile]");
	System.err.println("	  (default is Konstanz encoded input from stdin, UTF-8 encoded output to stdout)");
	System.err.println("Examples:");
	System.err.println("	  java	encoder proofs.jp				   output UTF-8 version of Konstanz file proofs.jp to console");
	System.err.println("	  java	encoder -I L proofs.jp				   output UTF-8 version of Laura file proofs.jp to console");
	System.err.println("	  java encoder -I UTF-8 BAN_syntax.j -O K BAN_syntax.jpk   save Konstanz version of BAN_syntax.j in BAN_syntax.jpk");
	
	Iterator it = Charset.availableCharsets().keySet().iterator();
	
	System.err.print("\nAvailable encodings:\nK, Konstanz, L, Laura, ");
	while (it.hasNext()) {
	    System.err.print((String)(it.next()));
	    if (it.hasNext())
		System.err.print(", ");
	}
	System.err.println();
	
	System.exit(0);
    }

    static String [] Konstanz = new String[] {
	"\u0080", "\u2ADF", // ⫟ (perp)
	"\u0081", "\u00C5", // Å
	"\u0082", "\u0393", // Γ
	"\u0083", "\u00C9", // É
	"\u0084", "\u00D1", // Ñ
	"\u0085", "\u00D6", // Ö
	"\u0086", "\u00DC", // Ü
	"\u0087", "\u00E1", // á
	"\u0088", "\u00E0", // à
	"\u0089", "\u00E2", // â
	"\u008a", "\u2AE2", // ⫢ (|≡)
	"\u008b", "\u27E8", // ⟨ (angle bra)
	"\u008c", "\u00E5", // å
	"\u008d", "\u00E7", // ç
	"\u008e", "\u00E9", // é
	"\u008f", "\u00E8", // è

	"\u0090", "\u00EA", // ê
	"\u0091", "\u25C1", // ◁
	"\u0092", "\u00ED", // í
	"\u0093", "\u00EC", // ì
	"\u0094", "\u00EE", // î
	"\u0095", "\u21DD", // ⇝ (should be |~)
	"\u0096", "\u27E9", // ⟩ (angle ket)
	"\u0097", "\u0097", // 
	"\u0098", "\u0098", // 
	"\u0099", "\u0099", // 
	"\u009a", "\u2907", // ⤇ (|⇒)
	"\u009b", "\u21D0", // ⇐
	"\u009c", "\u00FA", // ú
	"\u009d", "\u00F9", // ù
	"\u009e", "\u00FB", // û
	"\u009f", "\u21CC", // ⇌

	"\u00a0", "\u03a4", // Τ
	"\u00a1", "\u00B0", // °
	"\u00a2", "\u00A2", // ¢
	"\u00a3", "\u00A3", // £
	"\u00a4", "\u00A7", // §
	"\u00a5", "\u2022", // •
	"\u00a6", "\u2227", // ∧
	"\u00a7", "\u2286", // ⊆
	"\u00a8", "\u00AE", // ®
	"\u00a9", "\u00A9", // ©
	"\u00aa", "\u2122", // ™
	"\u00ab", "\u00B4", // ´
	"\u00ac", "\u00A8", // ¨
	"\u00ad", "\u2260", // ≠
	"\u00ae", "\u00C6", // Æ
	"\u00af", "\u00D8", // Ø

	"\u00b0", "\u221E", // ∞
	"\u00b1", "\u00B1", // ±
	"\u00b2", "\u2264", // ≤
	"\u00b3", "\u2265", // ≥
	"\u00b4", "\u22B8", // ⊸
	"\u00b5", "\u00B5", // µ
	"\u00b6", "\u2202", // ∂
	"\u00b7", "\u2211", // ∑
	"\u00b8", "\u220F", // ∏
	"\u00b9", "\u03C0", // π
	"\u00ba", "\u222B", // ∫
	"\u00bb", "\u2297", // ⊗
	"\u00bc", "\u2295", // ⊕
	"\u00bd", "\u2126", // Ω
	"\u00be", "\u00E6", // æ
	"\u00bf", "\u00F8", // ø

	"\u00c0", "\u00BF", // ¿
	"\u00c1", "\u00A1", // ¡
	"\u00c2", "\u00AC", // ¬
	"\u00c3", "\u221A", // √
	"\u00c4", "\u0192", // ƒ
	"\u00c5", "\u2248", // ≈
	"\u00c6", "\u2206", // ∆
	"\u00c7", "\u00AB", // «
	"\u00c8", "\u00BB", // »
	"\u00c9", "\u2026", // …
	"\u00ca", "\u00A0", //   (no-break space)
	"\u00cb", "\u00C0", // À
	"\u00cc", "\u00C3", // Ã
	"\u00cd", "\u00D5", // Õ
	"\u00ce", "\u0152", // Œ
	"\u00cf", "\u0153", // œ

	"\u00d0", "\u2013", // – (en dash)
	"\u00d1", "\u2014", // — (em dash)
	"\u00d2", "\u201C", // “
	"\u00d3", "\u201D", // ”
	"\u00d4", "\u2018", // ‘
	"\u00d5", "\u2019", // ’
	"\u00d6", "\u00F7", // ÷
	"\u00d7", "\u25CA", // ◊
	"\u00d8", "\u21A6", // ↦
	"\u00d9", "\u22A5", // ⊥
	"\u00da", "\u2208", // ∈
	"\u00db", "\u21d2", // ⇒
	"\u00dc", "\u2237", // ∷
	"\u00dd", "\u27E6", // ⟦ (semantic bra, [|)
	"\u00de", "\u27E7", // ⟧ (semantic ket, |])
	"\u00df", "\u2229", // ∩

	"\u00e0", "\u214B", // ⅋
	"\u00e1", "\u297D", // ⥽ (right fish tail)
	"\u00e2", "\u25AA", // ▪
	"\u00e3", "\u201E", // „
	"\u00e4", "\u2203", // ∃
	"\u00e5", "\u27DB", // ⟛ (left and right proves)
	"\u00e6", "\u22A6", // ⊦
	"\u00e7", "\u2192", // →
	"\u00e8", "\u2200", // ∀
	"\u00e9", "\u2261", // ≡
	"\u00ea", "\u2194", // ↔
	"\u00eb", "\u2228", // ∨
	"\u00ec", "\u039B", // Λ (Lambda)
	"\u00ed", "\u22A7", // ⊧
	"\u00ee", "\u22A9", // ⊩
	"\u00ef", "\u222A", // ∪

	"\u00f0", "\u27DA", // ⟚ (left and right models)
	"\u00f1", "\u223C", // ∼
	"\u00f2", "\u2135", // ℵ
	"\u00f3", "\u00DB", // Û
	"\u00f4", "\u00D7", // ×
	"\u00f5", "\u2292", // ⊒
	"\u00f6", "\u25A1", // □
	"\u00f7", "\u225C", // ≜
	"\u00f8", "\u207b\u00b9", // was ⫠ prep; now ⁻¹
	"\u00f9", "\u25CF", // ●
	"\u00fa", "\u2283", // ⊃
	"\u00fb", "\u03BB", // λ
	"\u00fc", "\u00B8", // ¸
	"\u00fd", "\u02DD", // ˝
	"\u00fe", "\u0328", //	̨
	"\u00ff", "\u02C7"  // ˇ
    };
    
    static String[] Laura = new String[] {
	"\u0080", "\u2193", // ↓
	"\u0081", "\u2190", // ←
	"\u0082", "\u21d0", // ⇐
	"\u0083", "\u2194", // ↔
	"\u0084", "\u2200", // ∀
	"\u0085", "\u2227", // ∧
	"\u0086", "\u27e6", // ⟦ (semantic bra, [|)
	"\u0087", "\u2987", // ⦇ (Z notation image bra)
	"\u0088", "\u2191", // ↑
	"\u0089", "\u2192", // →
	"\u008a", "\u21d2", // ⇒
	"\u008b", "\u21d4", // ⇔
	"\u008c", "\u2203", // ∃
	"\u008d", "\u2228", // ∨
	"\u008e", "\u27e7", // ⟧ (semantic ket, |])
	"\u008f", "\u2988", // ⤀ (Z notation image ket)
	
	"\u0090", "\u2a1f", // ⨟ (Z notation schema compose)
	"\u0091", "\u2264", // ≤
	"\u0092", "\u2282", // ⊂
	"\u0093", "\u2286", // ⊆
	"\u0094", "\u227a", // ≺
	"\u0095", "\u227b", // ≻
	"\u0096", "\u2208", // ∈
	"\u0097", "\u2261", // ≡
	"\u0098", "\u2982", // ⦂ (Z notation type colon)
	"\u0099", "\u2265", // ≥
	"\u009a", "\u2283", // ⊃
	"\u009b", "\u2287", // ⊇
	"\u009c", "\u227c", // ≼
	"\u009d", "\u227d", // ≽
	"\u009e", "\u2209", // ∉
	"\u009f", "\u2259", // ≙
	
	"\u00a0", "\u22a4", // ⊤
	"\u00a1", "\u22a2", // ⊢
	"\u00a2", "\u27ea", // ⟪ (mathematical double angle bra)
	"\u00a3", "\u27e8", // ⟨ (mathematical angle bra)
	"\u00a4", "\u2219", // ∙
	"\u00a5", "\u2294", // ⊔
	"\u00a6", "\u222a", // ∪
	"\u00a7", "\u21bf", // ↿
	"\u00a8", "\u22a5", // ⊥
	"\u00a9", "\u22a3", // ⊣
	"\u00aa", "\u27eb", // ⟫ (mathematical double angle ket)
	"\u00ab", "\u27e9", // ⟩ (mathematical angle ket)
	"\u00ac", "\u22c4", // ⋄
	"\u00ad", "\u2293", // ⊓
	"\u00ae", "\u2229", // ∩
	"\u00af", "\u21be", // ↾
	
	"\u00b0", "\u2308", // ⌈
	"\u00b1", "\u230a", // ⌊
	"\u00b2", "\u2248", // ≈
	"\u00b3", "\u25b3", // △
	"\u00b4", "\u22B2", // ⊲
	"\u00b5", "\u22B4", // ⊴
	"\u00b6", "\u228f", // ⊏
	"\u00b7", "\u2291", // ⊑
	"\u00b8", "\u2309", // ⌉
	"\u00b9", "\u230b", // ⌋
	"\u00ba", "\u00f7", // ÷
	"\u00bb", "\u25bd", // ▽
	"\u00bc", "\u22B3", // ⊳
	"\u00bd", "\u22B5", // ⊵
	"\u00be", "\u2290", // ⊐
	"\u00bf", "\u2292", // ⊒
	
	"\u00c0", "\u2322", // ⌢
	"\u00c1", "\u21d1", // ⇑
	"\u00c2", "\u219b", // ↛
	"\u00c3", "\u21a6", // ↦
	"\u00c4", "\u2260", // ≠
	"\u00c5", "\u2395", // ⎕ (Bernard says boxsquare, but unicode says apl functional symbol quad)
	"\u00c6", "\u00ac", // ¬
	"\u00c7", "\u2295", // ⊥
	"\u00c8", "\u2323", // ⌣
	"\u00c9", "\u21d3", // ⇓ (downwards double arrow)
	"\u00ca", "\u219a", // ↚
	"\u00cb", "\u2284", // ⊄
	"\u00cc", "\u2218", // ∘
	"\u00cd", "\u2337", // ⌷ (Bernard says boxthin, but unicode says apl functional symbol bsquish quad)
	"\u00ce", "\u2713", // ✓
	"\u00cf", "\u2297", // ⊗
	
	"\u00d0", "\u2225", // ∥
	"\u00d1", "\u228e", // ⊎
	"\u00d2", "\u2205", // ∅
	"\u00d3", "\u21f8", // ⇸ (partialfun, rightwards arrow with vertical stroke)
	"\u00d4", "\u21d5", // ⇕
	"\u00d5", "\u22a0", // ⊠
	"\u00d6", "\u220b", // ∋
	"\u00d7", "\u00d7", // ×
	"\u00d8", "\u03b1", // α
	"\u00d9", "\u03b2", // β
	"\u00da", "\u03b3", // γ
	"\u00db", "\u03b4", // δ
	"\u00dc", "\u03b5", // ε
	"\u00dd", "\u03b6", // ζ
	"\u00de", "\u03b7", // η
	"\u00df", "\u03b8", // θ
	
	"\u00e0", "\u03b9", // ι
	"\u00e1", "\u03ba", // κ
	"\u00e2", "\u03bb", // λ
	"\u00e3", "\u03bc", // μ
	"\u00e4", "\u03bd", // ν
	"\u00e5", "\u03be", // ξ
	"\u00e6", "\u03c0", // π
	"\u00e7", "\u03c1", // ρ
	"\u00e8", "\u03c3", // σ
	"\u00e9", "\u03c4", // τ
	"\u00ea", "\u03c5", // υ
	"\u00eb", "\u03d5", // ϕ
	"\u00ec", "\u03c7", // χ
	"\u00ed", "\u03c8", // ψ
	"\u00ee", "\u03c9", // ω
	"\u00ef", "\u0393", // Γ
	
	"\u00f0", "\u0394", // Δ
	"\u00f1", "\u0398", // Θ
	"\u00f2", "\u039b", // Λ
	"\u00f3", "\u039e", // Ξ
	"\u00f4", "\u03a0", // Π
	"\u00f5", "\u03a3", // Σ
	"\u00f6", "\u03a5", // Υ
	"\u00f7", "\u03a6", // Φ
	"\u00f8", "\u03a8", // Ψ
	"\u00f9", "\u03a9", // Ω
	"\u00fa", "\u03d0", // ϐ
	"\u00fb", "\u03d1", // ϑ
	"\u00fc", "\u03d6", // ϖ
	"\u00fd", "\u03f1", // ϱ
	"\u00fe", "\u03de", // Ϟ
	"\u00ff", "\u03c6"  // φ
    };
}





