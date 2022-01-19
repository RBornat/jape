# How to install Flatpak Jape on Linux

* If you are not using Ubuntu, and you are having trouble installing and/or running the standard Linux installation (`LinuxJape_`*version*`.tgz`) then the Flatpak version may be for you.

* You will need to be confident in using the command line. You will have to put up with the fact that Flatpak Jape can only access files (such as `.jt` and `.jp` files) *in your home directory* -- it can't see files elsewhere in the machine. 

* Please understand that this is an early version of the Flatpak installation, and teething problems are to be expected. Please report problems (or successes!) on [Jape's github issue page](https://github.com/RBornat/jape/issues).

------------------------

By unpacking `FlatpakJape`*version*`.tgz`, you have created a directory `FlatpakJape` containing `README_INSTALL_FLATPAKJAPE_.html` (this file), the `examples` directory, the command file `prepare_flatpak.sh`, and `jape.flatpak`. Follow the steps below to install and run Jape.

## Install Jape

1. Install flatpak, if you don't already have it. The steps you need vary between Linux distros: go to the [Flatpak quick setup page](https://flatpak.org/setup/) to find instructions.

2. Connect to the flathub repo and install some interface files. The commands you need are packaged in `prepare_flatpak.sh`

        sudo prepare_flatpak.sh
   
   (Some distros don't require `sudo`, and `./prepare_flatpak.sh` will work. Let me know about your experience.)
        
3. Install Jape

        flatpak --user install jape.flatpak

4. Install the examples directory

    The `examples` directory needs to be somewhere Jape can see it, *in your home directory*. It doesn't need to be at the top level. Move it to a convenient location.

## Run Jape

        LC_ALL="en_US.UTF-8" flatpak run uk.org.jape	
        
(The `LC_ALL="en_US.UTF-8"` prefix may not be needed on all systems: without it you may get a complaint about `/bin/sh` and `setlocale`. When I installed FlatPak Jape it didn't run first time, but it did work after that. Let me know your experience)

## Uninstall Jape

(If you need to)

        flatpak --user uninstall uk.org.jape

## Problems?

Please notify problems, as usual, to [Jape's github issue page](https://github.com/RBornat/jape/issues).

## Acknowledgements

I build Jape for Linux on Ubuntu. I hoped my installation file would work for other Linuxes, but it turns out that it doesn't. One enterprising user -- Timo Triebensky, see his github page [here](https://github.com/binsky08) -- figured out how to install Jape on NixOS using [Flatpak](https://flatpak.org/), a mechanism which promises to work on lots of different Linux distributions (at time of writing, they claim 33). My installation is based on Timo's work, and his patience in debugging my own attempts.

Richard Bornat
2022/01/09
richard@bornat.me.uk  

