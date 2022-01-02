# Flatpak Jape for Linux

I build Jape for Linux on Ubuntu. I hoped my installation file would work for other Linuxes, but it turns out that it doesn't. 

One enterprising user, Timo Triebensky <timo@binsky.org>, figured out how to install Jape on NixOS using [Flatpak](https://flatpak.org/), a mechanism which promises to work on lots of different Linux distributions (at time of writing, they count 33). What follows is based on Timo's work.

To install Jape via Flatpak, you have to be familiar with the use of the terminal: no point-and-click, at least until you get Jape installed.

1. Install flatpak, if necessary (if your installer isn't called `apt` then adjust as necessary) 

        sudo apt install flatpak

2. Get the flatpak installation of Jape from [Jape's github release page](https://github.com/RBornat/jape/releases) -- it will be `jape_`*version*`.flatpak` where *version* is the version number.

3. From [Jape's github release page](https://github.com/RBornat/jape/releases) get the file `flatpak_prepare.sh`  

4. Execute `flatpak_prepare.sh` to connect to the flathub repo and download some interface files 

        ./flatpak_prepare.sh

5. Install Jape

    `flatpak --user install jape_`*version*`.flatpak`
        
6. Install the examples directory -- get `examples.zip` from [Jape's github release page](https://github.com/RBornat/jape/releases), put it somewhere *in your home directory* and unzip it. (It *must* be in your home directory: flatpak Jape can't see beyond that.)

6. Run Jape

        flatpak run uk.org.jape
        
When I did all this for the first time the last step didn't work first time, but it did work after that. Good luck. 

Notify problems, as usual, to [Jape's github issue page](https://github.com/RBornat/jape/issues).

Richard Bornat
2022/01/02