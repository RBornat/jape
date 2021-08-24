# Building Jape on Windows

(If you have no Unix experience and you don't like typing commands in a terminal window, don't bother. Just get a binary distribution from github.)

It's possible to build Jape on Windows using *cygwin*, which provides a Unix-y mechanism with a terminal window. Search for OCaml for Windows, download it, install it. It will provide you with a shortcut on your desktop which is called OCaml64 (or OCaml32). Double-click that shortcut to get a cygwin shell.

Jape needs OCaml 4.09.0 or later. Get opam, if it isn't there already (run the setup.exe for OCaml64/32 to get new software). Make sure it's opam version 2.0 or later.

The OCaml version I loaded is ` 4.09.0+mingw64c`: `opam switch list` will tell you what's available. `opam update` and `opam upgrade` can be very useful. You will need `ocamlbuild` and `ocamlfind`; opam can help.

You will need Java; Jape needs version 9 or later. Get a JDK (preferable version 11, preferably from AdoptOpenJdk) and install it. You'll have to set JAVA_HOME to point to it.

You will need Apache Ant. Get it from Apache, install it (which is a bit of a pain, but their instructions do work). 

You may have to tinker with `~/.bash_profile` to make it find the right software in the right locations. Sorry.

If typing `javac -version` to cygwin shows you the Java you installed; if typing `ocamlc -version` shows you the OCaml; and if typing `ant` complains that there is no `build.xml` then you're in business. Otherwise you have to tinker with your installation.

Now you need jape source. In cygwin

        git clone https://github.com/RBornat/jape.git
        
(you have to clone it or the versioning doesn't work). Once you've got it, `git checkout <version>` will (I think) let you see the version you want to build.

Then, also in cygwin 

        cd jape/distrib/CommonBuildResources/Windows
        make all
        
It will build `InstallJape.exe`. Move it somewhere convenient and double-click it. You'll find examples and Jape.lnk in that convenient location. Double-click Jape.lnk to run Jape.

Richard Bornat
2021/08/19
        

