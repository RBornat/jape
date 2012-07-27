README file for text encoding 
$Id$

0. Unless you have been using Jape forever and made some theories
in the old Konstanz or Laura encodings you needn't bother with this.

1. Background

Jape used to use custom 8-bit fonts, exploiting the 128 characters coded
0x80 .. 0xff to represent symbols outside the range of ASCII. The
original font we used was called Konstanz; we inherited it from Roy
Dyckhoff, who developed it for MacLogic. I added some glyphs to support
BAN logic and some more to support my own work on pointer programs.

Bernard Sufrin, exasperated by the Mac-y-ness of Konstanz, introduced a
font called Laura which, in particular, included lots of Z notation
glyphs.

Working in these fonts meant that Jape could draw the symbols it needed
on the screen. On the Mac it was easy to use editors like BBEdit with
the special fonts, and edit in those fonts. For Unix and Windows Bernard
built a couple of editors in Python that edited in these encosings
I never used them so I can't comment on their utility, but Bernard 
swore by them and used them for all his other editing as well.

Special fonts were nice, but they were the dickens to work with in Latex
or whatever. We always knew that one day we'd have to change to Unicode.

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
UTF-16 uses 16-bit values when it can, and otherwise sequences of two
16-bit values. Java uses UTF-16 internally. UTF-32 is obvious.

3. Translation

You can run the java program 'encoder' to translate from Konstanz or
Laura to UTF-8 (or, if you like, to UTF-16 or any other encoding that
your java implementation supports).

It's a command-line program (but you can get the source and hack it if
you want to change that).

        java -jar encoderjar inputfile

will translate inputfile from Konstanz and output it on the console in
UTF-8.

        java -jar encoderjar -I Konstanz inputfile

will do precisely the same thing.

        java -jar encoderjar -I K inputfile -O U8 outputfile

will do the same translation and put the result in outputfile.

        java -jar encoderjar -I L inputfile

will translate from Laura and put the UTF-8 result on the console.

And so on:

        java -jar encoderjar -help

will tell you all the things you can do, and list the encodings you can
use.

4. BOM or no BOM?

It's impossible to tell whether a particular file contains UTF-8/16/32
or ASCII text. So the UTF encodings use a cunning wheeze: a sequence of
bytes at the beginning of the file can declare the encoding. UTF-8 can
start ef, eb, ef; UTF-16 starts fe, ff or ff, fe (depending on the
endian-ness of the encoding); UTF-32 starts 00, 00, fe, ff or ff, fe,
00, 00.

Programs reading UTF text are supposed to detect this marker (called a
byte order marker or BOM) and ignore it. Editors are supposed to use it
to determine whether they are dealing with UTF or ASCII, and hide it
from the user. Not every program is clever enough to do that (Jape is;
so is encoder; javac isn't).

If you are translating Jape files -- .j, .jt, .jp -- from Konstanz or
Laura to UTF, then it's a good idea, so far as I can see, to put a BOM
at the front of the output. For one thing, it means you can double-click
the output file to edit it. Jape notices the BOM, acts on it, and
otherwise ignores it.

Other programs may be more tricky, so there's a switch "-nobom" which
you can use with the encoder program.

    java -jar encoderjar -nobom proofs.jp -O proofs.utf.jp

will translate proofs.jp from Konstanz and produce proofs.utf.jp without
a BOM.

On the other hand, you can use the encoder to put BOMs into files that
are missing them, or take them out, or whatever.

    java -jar encoderjar -I U8 rhubarb.java -O U8 rhubarbnoBOM.java -nobom

will produce a UTF-8 file without a bom, which javac will happily deal
with.

In the other direction

    java -jar encoderjar -I U8 rhubarb.jp -O U8 rhubarb.bom.jp

will make a UTF-8 file with a BOM (whether or not rhubarb.jp had one;
and of course it doesn't multiply BOMs!).

-----------------------

Richard Bornat 13th April 2004

