Windows (2K and XP) Jape installation
=====================================

February 2003 (Revision 3)
=============


You will need
-------------

        A functioning java (1.4 or later) system. Sun's standard jre
        for windows (available at www.javasoft.com) is good enough --
        you don't need the java development kit.

License
-------

    Jape is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software Foundation,
    Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA (or
    look at http://www.gnu.org).


What to do
----------

0. Decide where you want Jape's home to be on your system, and move
   the $(OS)jape.jar file there. 

1. Run the program $(OS)jape.jar by double clicking on it, or by
   whatever other means you normally use to start a java program
   packaged as a .jar file. [See TROUBLESHOOTING note 2 below]

   This will start executing the interactive java installer.

   The installer brings up a Splash Screen on which there are
   three buttons and a log window. 

           If you have downloaded the jar file to your desktop you
           are STRONGLY ADVISED to select a folder before pressing
           the Install button.  You can do this by pressing the
           "Choose Folder" button -- this brings up a folder
           selection dialogue.

2. Press the button labelled "Install".

   This unpacks the examples directory, and leaves the inference
   engine (jape.exe -- architecture dependent), the interface
   server (Jape.jar -- architecture independent), and an
   installation program (installjape.class -- should be architecture
   and OS dependent).  It then runs the installation program, which
   builds and runs a command file (installjape.cmd) which constructs
   a shortcut (jape) that starts the jape program when it is 
   opened/clicked.

3. ONCE YOU ARE SURE THAT YOU CAN RUN JAPE, YOU MAY DELETE THE
   DEBRIS LEFT BY THE INSTALLATION PROCESS.

        del windowsjape.jar japeicon.ico installjape.class bootstrap meta-inf shortcut.exe installjape.cmd
  

   TROUBLESHOOTING: 

   1. If you don't have java installed then this
   will not work. Do whatever you need to do to get java installed
   and on your path. You don't need the whole jdk, just the java
   runtime environment (jre -- obtainable from www.javasoft.com)

   2. If you have installed software (such as WinRAR) that has
   registered .jar files for opening itself, then you will have
   to single-click on $(OS)jape.jar, then use the Open/With 
   menu entry to run the .jar as a java program.

   3. Clicking on the shortcut constructed by the installer is
   equivalent to executing the following command on a command line
   from the Jape installation directory:

        javaw -jar Jape.jar -engine jape.exe examples

   If you have trouble with the shortcut then try this as a last resort.


4. Try running jape then pressing "File/Open New Theory" and
   selecting (for example) the theory file

        examples/sequent_calculus/SCS.jt

   [[Do not press File/Open]]

5. Please let us know that you've succeeded (or failed) by sending a short
   message of triumph (or despair) to

        japeinstall@jape.org.uk

   In case of despair, please send a log of your installation session and
   the attempted run of jape.        

   
Gestures in jape
----------------

Jape is now beginning to conform to common user interface standards.

<letftclick>            formula selection
<shift-leftclick>       add a formula selection, or to cancel an existing selection.

<middleclick>           text selection
<shift-middleclick>     extend an existing text selection
<control-middleclick>   add a text selection

Control-Z               -- undo one proof step
Shift-Control-Z         -- redo last undone proof step
Control-D               -- finish completed proof






