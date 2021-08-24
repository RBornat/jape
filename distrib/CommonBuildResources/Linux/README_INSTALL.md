# How to install Jape on Linux

By unpacking LinuxJape.tgz, you have created a directory `LinuxJape` containing this file, `installJape.sh` and `.data`. To install Jape, you must run/launch/execute `installJape.sh`. 

`installJape.sh`, when it runs, will setup Jape and leave you with Jape's `examples` and a new control file `runJape.sh`. It will also start Jape.

Once Jape is setup you can move `examples` and `runJape.sh` anywhere you like, and delete the `LinuxJape` directory if you like.

## 1. If Ubuntu is your Linux and Gnome is your GUI (it probably is)

  Gnome, by default, opens `installJape.sh` in the text editor when you double-click it. There are two things you can do to make Gnome run it instead: 

  * *either* use the command line (see below);

  * *or* persuade Gnome to run executable files when you double-click them: 
    1. click on the menu button in a 'Files' window (three bars, just left of the window-minimise button in the top row), and choose 'Preferences'. 
    2. Then select the 'Behaviour' tab in the top row of the Preferences window.
    3. Under **Executable Text Files** choose 'Run them'.
    4. Close the Preferences window
  
    Now `installJape.sh` will run when you double-click it (and so will `runJape.sh` when it's installed). 

    ### Just one more thing  

    *  When Jape runs, it puts a Jape icon in the task bar (left or bottom of the screen) -- right-click the icon and add it to your favourites, so you can click it to run Jape whenever you want to.
  
## 2. If you are happy to use the command line (and why wouldn't you be?)

* open a terminal window;
* change (`cd`) to the directory where you unpacked the .tgz file;
* `LinuxJape/installJape.sh` will install Jape.
  
and then to run Jape: `LinuxJape/runJape.sh&`
        
       
## 3. If you find a better way of running installJape.sh on your Linux
please let me know.

Richard Bornat  
2021/07/28  
richard@bornat.me.uk  
   