# README for Jape 9.0.*

(The Jape version is shown in the Jape splash screen -- the window which comes up when you start Jape.)

## Getting Jape

Go to [github](https://github.com/RBornat/jape/releases) and download the latest version: for Macos it's called jape_*N*.dmg, where *N* is the version number. 

## Installation

Double-click the .dmg file. Copy Jape.app and the examples folder wherever you like. (But *don't* put the examples file on your Desktop; Jape isn't notarized by Apple yet, and it doesn't like Jape reading stuff from the Desktop.)

## Java version

Jape uses Java to drive its user interfaces (windows etc.). You need Java 9 or later to run it. It's ok to use the latest version (get it from Oracle). If you already have Java you can test the version you have by typing

    java -version
    
in a Terminal window

## Running Jape

The first time you run Jape, you will find that double-clicking it doesn't work, because we (Richard Bornat and Bernard Sufrin) haven't yet registered as Apple developers. Instead, the first time you use it you must Ctrl-press on the Jape application, and choose Open from the pop-up menu, and say you trust us.

After that you can just double-click Jape to run it. 

## Choose a font

In the distant past there were few fonts that included lots of Unicode characters like the ones that Jape uses for logical connectives. So we found one that seemed to be free(-ish) called *Lucida Sans Unicode*, and we recommended people to download it. But it wasn't as free as we thought (it belongs to Microsoft), and it isn't in modern distributions of macOS. And in any case it was a sans-serif font, so it doesn't show the difference between 1 (digit 1) and l (lower-case L) very well. 

So when you start Jape, go to Preferences in the Jape menu and choose a nice font. On my machine I quite like Cambria Math, but YMMV. Jape will, of course, remember what you choose.

## Choose a theory

At present you have to open a theory file (.jt extension) through Jape's File menu (File>Open... or File>Open new theory...). The examples directory contains lots of example theories. The one I use in the Proof and Disproof book (see below) is `examples/natural_deduction/I2L.jt` .

## Have fun

The point of Jape is to enable you to play in logic or play with logic. The examples directory contains logic encodings ('theories') that Bernard Sufrin and I have made. The github repository contains manuals in case you want to play with an encoding of your own.

## That book

Oxford University Press let me put a [version online](https://homepages.phonecoop.coop/randj/richard/books/ProofandDisproof.pdf).

## Complaints

To richard@bornat.me.uk

Richard Bornat
2019/12/04
