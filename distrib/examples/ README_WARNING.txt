******************* WARNING -- EXAMPLES CHANGE *************************

All Jape's example files are now in Unicode (actually UTF-8).

If you have saved proofs (.jp) files, you have to translate them from
their original representation -- probably they are in Konstanz encoding,
unless you were using Bernard's jnj encoding, which used Laura encoding
-- into UTF8.

There's a java program to do the job in this distribution. Look for
encoder.jar in the utf8_encoder directory. There's a README there too.
Basically

java -jar encoder.jar -I K rhubarb.jp -O U8 rhubarbu.jp

will do the job for Konstanz-encoded files, and

java -jar encoder.jar -I L custard.jp -O U8 custardu.jp

will work for Laura.

If you want to put a GUI on the encoder, feel free: the source code is
in the .jar bundle. Send your version back to us, and we'll put it in
the next distribution if we like it enough.

----------------------------------------------

Richard Bornat 
Bernard Sufrin 
27th April 2004