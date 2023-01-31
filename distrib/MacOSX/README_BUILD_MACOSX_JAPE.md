How to build Jape on MacOS (April 2018; updated January 2023)
=============================================================

Currently I build MacOS Jape on Catalina (10.15) and Ventura (13.0), using OCaml
4.12.0 and Java 16.0.  I build Linux Jape with the same Ocaml and Java, but on
VirtualBox with a Linux emulator; likewise Windows Jape.

If you want to build Jape for MacOS
-----------------------------------

1.  Install the developer tools (get them from Apple; these days I think you
    have to get them by installing Xcode and then asking Xcode to install them).
    You will also need git, if it isn't in the developer tools.

2.  Make sure ant (installed by the developer tools) is known to the shell. 
    If it isn't, install it with homebrew.
    
3.  You will need OCaml to build the jape ‘engine’ (at time of writing I use 4.12.0)

4.  You will need a lateish Java, because the Jape build mechanism uses jlink and jmod and jre.

5.  In the directory where you found this file (...../distrib/MacOSX):  
        `make local`  
    builds the engine and the GUI interface, bundles them together and puts them
    in ./build/Jape.app;  
        `make run`  
    lets you try it out. If it works, move build/Jape.app where you like.

Contact
-------

Build originally documented by Richard Bornat (richard\@bornat.me.uk); subsequently edited by Richard and by Bernard Sufrin (\@ cs ox ac
uk).
