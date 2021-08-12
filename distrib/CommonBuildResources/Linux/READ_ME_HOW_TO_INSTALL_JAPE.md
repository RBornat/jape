# How to install Jape on Linux

To install Jape, you must run/launch/execute the file `installJape.sh`. 

`installJape.sh`, when it runs, will do all the work and leave you with an `examples` directory and a new control file `runJape.sh`, and it will start Jape.

## 1. If Ubuntu is your Linux and Gnome is your GUI (it probably is)

  Gnome, by default, opens `installJape.sh` in the text editor when you double-click it. There are two things you can do to make Gnome run it instead: 

  * *either* use the command line (see below);

  * *or* persuade Gnome to run executable files when you double-click them: 
    1. click on the menu button in a 'Files' window (three bars, just left of the window-minimise button in the top row), and choose 'Preferences'. 
    2. Then select the 'Behaviour' tab in the top row of the Preferences window.
    3. Under **Executable Text Files** choose 'Run them'.
    4. Close the Preferences window
  
    Now `installJape.sh` will run when you double-click it (and so will `runJape.sh` when it's installed). 

  **Just one more thing**: when it runs Jape, it puts a Jape icon in the task bar (left or bottom of the screen) -- right-click it and add it to your favourites, so you can click it to run Jape whenever you want to.
  
## 2. If you are happy to use the command line (and why wouldn't you be?)

* open a terminal window;
* change (`cd`) to the directory where you unpacked the installer;
* `./installJape.sh` will install Jape.
  
and then to run Jape, of course, `./runJape.sh`
        
       
## 3. If you find a better way of running installJape.sh on your Linux
please let me know.

Richard Bornat  
2021/07/28  
richard@bornat.me.uk  
   