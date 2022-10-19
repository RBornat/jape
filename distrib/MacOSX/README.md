# README for Jape 9.1.*

(The exact Jape version is shown in the Jape splash screen -- the window which comes up when you start Jape.)

Go to [github](https://github.com/RBornat/jape/releases) and download the latest version: for Macos it's called jape_*N*.dmg, where *N* is the version number. 

Double-click the .dmg file. Copy Jape wherever you like, and copy the examples folder wherever you like **_inside your own folders_ but _not_ in Desktop, or Documents, or Downloads, or Library**. Jape isn't notarised by Apple (I haven't paid the 100$ annual fee), and so Apple won't let it read stuff from those protected folders. The problem is not fixable: there's no point giving Jape Full Disk Access, for example, because it won't work.

## Running Jape

The first time you run Jape, you will find that **double-clicking it doesn't work**. Instead, you must **Ctrl-click (or right-click) on Jape, choose Open from the menu which pops up**, and say you trust Jape. (Again, this is because Jape isn't notarised by Apple.)

Once you've said you trust it, you _can_ double-click Jape to run it. 

## Choose a font

In the distant past there were few fonts that included lots of Unicode characters like the ones that Jape uses for logical connectives. So we found one that seemed to be free(-ish) called *Lucida Sans Unicode*, and we recommended people to download it. But it wasn't as free as we thought (it belongs to Microsoft), and it isn't in modern distributions of macOS. And in any case it was a sans-serif font, so it doesn't show the difference between 1 (digit 1) and l (lower-case L) very well. 

So when you start Jape, go to Preferences in the Jape menu and choose a nice font. On my machine I quite like Cambria Math, but YMMV. Jape will, of course, remember what you choose.

## Choose a theory

You have to open a theory file (.jt extension) through Jape's File menu (File>Open... or File>Open new theory...). The examples directory contains lots of example theories. The one I use in the Proof and Disproof book (see below) is `examples/natural_deduction/I2L.jt` .

## Have fun

The point of Jape is to enable you to play in logic or play with logic. The examples directory contains logic encodings ('theories') that Bernard Sufrin and I have made. The github repository contains manuals in case you want to play with an encoding of your own.

## That book

Oxford University Press let me put a [version online](https://homepages.phonecoop.coop/randj/richard/books/ProofandDisproof.pdf).

## Complaints

To [Jape's issues page on github](https://github.com/RBornat/jape/issues)

Richard Bornat
2021/08/26
