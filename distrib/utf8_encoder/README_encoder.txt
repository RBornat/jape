README file for text encoding

1. Background

Jape used to use custom 8-bit fonts, exploiting the 128 characters coded
0x80 .. 0xff to represent symbols outside the range of ASCII. The
original font we used was called Konstanz; we inherited it from Roy
Dyckhoff, who developed it for MacLogic. I added some
glyphs to support BAN logic and some more to support my own work on
pointer programs.

Bernard Sufrin, exasperated by the Mac-y-ness of Konstanz, introduced a
font called Laura which, in particular, included lots of Z notation
glyphs.

Working in these fonts meant that Jape could draw the symbols it needed
on the screen. On the Mac it was easy to use editors like BBEdit with
the special fonts, and edit in those fonts. On Linux and on Windows it
was never so easy (you may well ask why) and Bernard's inspired hackery,
though it produced many generations of specially-crafted editors, never
seemed to survive more than a couple of versions of python or whatever.

Special fonts were nice, but they were the dickens to work with in Latex
or whatever. I always knew that one day I'd have to change to Unicode.

2. On to Unicode

Jape now works internally in unicode, and its inputs have to be in
unicode too. Unicode is a system of 21-bit characters, large enough to
encompass all the natural languages of the world and all the
mathematical symbols that have been proposed to the Unicode Consortium
(not all of BAN logic has been proposed yet -- get to it, BAN users!).

21-bit characters are a bit of a headache. There are various ways of
representing them. In a Eurocentric program like Jape, it makes sense to
use UTF-8, which represents ASCII in a single byte, and other characters
as 2, 3 or 4-byte sequences. Internally, the Jape engine uses UTF-8.
UTF-16 represents 16-bit characters as a 16-bit value, and the rest as
two 16-bit values. Java uses UTF-16 internally. UTF-32 is obvious.

3. Translation

You can run the java program 'encoder' to translate from Konstanz or
Laura to UTF-8 (or, if you like, to UTF-16 or any other encoding that
your java implementation supports).

It's a command-line program (but you can get the source and hack it if
you want to change that).

java encoder inputfile

will translate inputfile from Konstanz and output it on the console in
UTF-8.

java encoder -I Konstanz inputfile

will do precisely the same thing.

java encoder -I K inputfile -O U8 outputfile

will do the same translation and put the result in outputfile.

java encoder -I L inputfile

will translate from Laura and put the UTF-8 result on the console.

And so on.

java encoder -help

will tell you all the things you can do, and list the encodings you can
use.

-----------------------

Richard Bornat
13th April 2004
