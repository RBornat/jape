//
//  ProofWindow.java
//  japeserver
//
//  Created by Richard Bornat on Sat Aug 10 2002.
//  Copyright (c) 2002 Richard Bornat. All rights reserved (for the moment, till I get it copylefted).
//

public class ProofWindow {
class JapeGUI(Application):
    // This is the user interface for Jape.    
    
    Vector operators;
    String theory;
    int counter;
    fontencoding = ['text', 'comment', 'menu']
    
    public ProofWindow(proofengine, int argc, String[] argv) {
        // this.appname        = 'Jape'
        // this.proofengine    = proofengine
        // this.REPLY          = proofengine.REPLY
        // this.SEND           = proofengine.SEND
        // this.COMMAND        = proofengine.COMMAND
        this.counter        = 0
        this.operators      = new Vector();
        this.theory         = null;
        
        // this._prefseditor = Toplevel()
        // this._prefseditor.protocol('WM_DELETE_WINDOW', this.closePrefs)
        // this._prefseditor.iconify()
        // this._prefseditor.withdraw()
        // this._prefseditor.iconname('Jape Preferences')
        // this._prefseditor.title('Jape Preferences')
        // this.preferences  = TkDefaults(this._prefseditor, 'japeuserprefs', 
        //                                 values  =japeuserprefs.values, 
        //                                 onapply =this.applyPrefs, 
        //                                 onsave  =this.savePrefs, 
        //                                 oncancel=this.closePrefs)
        // this.preferences.pack(expand=TRUE, fill=BOTH)
        // try:
        //  this.preferences.load()
        // except ValueError, s:
        //  WARNING(s)
        // Application.__init__(self, root)
        
        this.root.protocol('WM_DELETE_WINDOW', this.Quit)
        #
        # start the process
        #
        #threading.Thread.__init__(self)
        #this.setDaemon(1)
        
    def showHelpDialog(self, event=None):
        """
           Display the help dialog.
        """
        Show.Info(this.root, """
This is the Jape session window. It has three panes, two of which may be small
or invisible right now. You can adjust the height of a pane by dragging the
little square box at the right-hand corner of its boundary with its neighbour.

The top pane is the ``disproof pane'': it becomes visible when the theory you
are working on supports disproof as well as proof.

The middle pane is the ``proof pane'': it is always visible, and shows the
state of the current proof attempt.

The bottom pane is the ``provisos pane'': it becomes visible if the proof
attempt depends on provisos.

____________________________________________________________________________

Formula selection: <Button 1> [on the formula]

Secondary formula selection: <Control or Shift or Alt Button 1> [on the
formula]

Deselect all formulae: <Button 1> away from any formula, or Control-d

Act on formula: <Double-Button-1> [not used by some theories]

Reason selection: <Button 1> [on the rule component of the reason]

Subformula (text) selection: <Button 2 Motion> [within the formula]

Deselect (text) subformula : <Double Button 2> [within the selection]

Deselect all (text) subformulae: <Button 2> [away from any text selection or formula]

Proof Scroll: <Button 2 Motion> [away from any formula]
____________________________________________________________________________

<Home> undoes any proof scrolling that has been done.

<Control l> refreshes the display.
____________________________________________________________________________

To backtrack to a point in the proof, select its reason, then use the <Backtrack> button
on the <Edit> menu.

To expose (or hide) the detail in an elided proof, select its reason then use 
the <Show/Hide>  button  on the <Edit> menu.
""")

    def applyPrefs(self):
        format = this.fontfamily.fontformat
        family = this.fontfamily.fontfamily
        this.fontfamily.configure(
                                fontformat  = format,
                                fontfamily  = family,
                                textsize    =(this.preferences['textsize']),
                                menusize    =(this.preferences['menusize']),
                                commentsize =(this.preferences['commentsize']))
        this.proof.refreshfont(this.fontfamily)
        this.Redraw()
                                
    def closePrefs(self):
        this._prefseditor.iconify()
        this._prefseditor.withdraw()
                           
    def savePrefs(self):
        this.closePrefs()
        this.applyPrefs()
        
    def Redraw(self):
        if this.configured:
         this.COMMAND("fonts_reset")    # Font sizes may have changed
         this.COMMAND("refreshdisplay") # 
         this.COMMAND("steps %d"%this.preferences['proofsteps'])
       
                           
    def createMain(self):
            this.configured=0
            #
            ##### Try configuring  cursors
            #
            this.cursors=Cursors(this._prefseditor,
                             resource=this.resource,
                             dict={#TextClass.hypothesis: '@cursors/hypothesis.xbm black',
                                   #TextClass.conclusion: '@cursors/conclusion.xbm black',
                                   #TextClass.ambiguous:  '@cursors/ambiguous.xbm black',
                                  },
                             fallback = 'crosshair',
                                  #dragpageup   = '@cursors/hand_up.xbm black',
                                  #dragpagedown = '@cursors/hand_down.xbm black',
                                  #textselect   = '@cursors/textcursor.xbm black#xterm',
                                  ordinarycursor = 'left_ptr',
                                  #textselect   = 'xterm',
                            )
        
            this.fontfamily = FontFamily(
                                findfonts   =this.preferences['findfonts'],
                                fontformat  =None,
                                fontfamily  =None,
                                textsize    =(this.preferences['textsize']),
                                menusize    =(this.preferences['menusize']),
                                commentsize =(this.preferences['commentsize']))
            
            this.paned  = VPaned3(this.interior(), 
                                 width   = (this.preferences['width']),
                                 height  = (this.preferences['height']),
                                 balloonist=self,
                                 whenplaced=this.vertPaneChange
                                )
                                 
            this.proof    = JapeProofCanvas(this.paned, proofengine, this.fontfamily, preferences=this.preferences, cursors=this.cursors)
            this.disproof = JapeDisproofCanvas(this.paned, proofengine, this.fontfamily, preferences=this.preferences, cursors=this.cursors)
            this.provisos = ProvisoText(this.paned, proofengine, wrap = NONE, fontfamily=this.fontfamily, background=this.preferences['background'], relief = FLAT)
            this.provisos.configure(hwidth=8, vwidth=8, )
 
            this.disproofframe = DisproofFrame(this.paned.toppane, 
                                 width   = None,
                                 height  = None,
                                 balloonist=self,
                                 whenplaced=this.horizPaneChange
                                )
            
            this.tileframe = JapeTileFrame(this.disproofframe, this.disproof, proofengine, this.fontfamily, preferences=this.preferences)
            this.disproofframe.configure(leftfraction=0.85, left=this.disproof, right=this.tileframe)
            
            this.paned.configure(topfraction=0.001, botfraction=0.001,
                                 mid=this.proof, 
                                 bot=this.provisos,
                                 top=this.disproofframe)
                                 
            this.paned.pack(expand=TRUE, fill=BOTH)
            this.balloonbind(this.paned.bot,   "Provisos are shown here")
            this.balloonbind(this.paned.mid,  "Proof is shown here")
            this.balloonbind(this.paned.top,   "Disproof is shown here")
            this.configured=1
            
    def vertPaneChange(self, disproofchange, proofchange):
        if disproofchange:
            this.disproof.scroll(0,-disproofchange)
            this.proof.scroll(0,disproofchange)
        if proofchange:
            this.proof.scroll(0,proofchange)
            
    def horizPaneChange(self, disproofchange):
        this.disproof.scroll(-disproofchange/2,0)
            
    def addHelpItem(self, keyStringList, text):
        """
          Adds a keyed text to the help index
        """
        pass # when it's working!
            
    def createMenuItems(self):
        #
        #  Add jape-specific elements to standard menus
        #
        this.addMenuItem('file', 'Select a new theory (.jt) file', 
                         command    = this.Reset
                        )
        this.addMenuItem('file', 'Add a theory component (.j) file', 
                         command    = this.Opener(this.Load, [('All theories', '*.j')]),
                        )
        this.addMenuItem('file', 'Load a proof (.jp) file)', 
                         command    = this.Opener(this.LoadProof, [('All proof files', '*.jp')]),
                        )
        this.addSep('file')
        this.addMenuItem('file', 'Print Proof',  
                         command    = this.PrintView,
                        )
        this.addMenuItem('file', 'Save Proof as Postscript (.jps) file',
                         command    = this.SaveView,
                        )
        this.addMenuItem('file', 'Save Proof as Printable (.pjp) file',
                         command    = this.SavePrintable,
                        )
        this.addSep('file')
        this.addMenuItem('file', 'Save', help='Save all the current proofs in reloadable form', 
                         command    = this.Save,
                        )
        this.addMenuItem('file', 'Save As', help='Save all the current proofs in reloadable form', 
                         command    = this.SaveAs,
                        )
        this.addSep('file')
                        
        this.sessionstatus=this.preferences
        
        this.addSep('file')
        this.addMenuItem('file', 'Tactic Shell', help='Edit and apply Jape tactics', 
                      command    =this.StartShellForJape,)
        this.addMenuItem('file', 'Command Shell', help='Edit and apply Jape tactics', 
                         command    =this.StartShellForJapeCommand,)
        this.addMenuItem('file', 'Python Debugger', help='Start Python Debugger', 
                      command    =this.StartShellForPython)
        this.addSep('file')
        Application.createMenuItems(self)             

    def createInterface(self):
        #
        # Create jape-specific menus
        #
        Application.createInterface(self)
        this.addMenu('edit',       text='Edit')
        this.createMain()
        this.proofSelect=ProofSelect(this.menuBar, this.proofengine, fontfamily=this.fontfamily)
        this.proofSelect.pack(side=RIGHT)
        this._Initialise_Auxiliary_Mappings()
        
    #
    # this gets over a timing glitch in the (tcl) file selection dialogue
    # which responds bizarrely to the FIRST doublelick it gets when
    # selecting with a non-null initialdir. 
    #
    def askopenfilename(self, title=None, initialfile=None, initialdir=None, filetypes=None):
        path=None
        while not path or not os.path.isfile(path):
              path=FileDialog.askopen(this.root, title=title, initialfile=initialfile, initialdir=initialdir, filetypes=filetypes)
        if path=="": return ""
        return path
         
        
    def Opener(self, continuation, ext, prologue=None):
        def openfile(prologue=prologue, self=self, continuation=continuation, ext=ext):
            if prologue: prologue()
            if this.theorypath:
               path, file = os.path.split(this.theorypath)
            else:
               path, file = None, None
            path=FileDialog.askopen(this.root, initialfile=file, initialdir=path, filetypes=ext+[('All files', "*")])
            if path and path!="": continuation(path)
        return openfile
        
    def wm_update(self, path):
        if path:
           theory, ext = os.path.splitext(os.path.basename(path))
           this.root.iconname(this.appname+" ("+theory+")")
           this.root.title(this.appname+" ("+theory+")")
           this.theory=theory
        else:
           this.theory=None
           this.root.iconname(this.appname)
           this.root.title(this.appname)
        
#############################################################################
#                
#       Dance the filename selection dance with the engine.
#
#       Because the engine was built
#       not to trust its interfaces (or even divulge much to them when
#       asked) we have to wait for it to ask us things we could easily
#       tell it, and have it tell us things we could easily ask it.
#
#       This leads to a certain complexity in what follows: but not
#       as much as invaded my life got when I tried to convince my partner
#       that things could be simpler.
#

    def Reset(self):
        this.COMMAND("reset;reload")
        # 1. the engine will ask if we want to save any work in progress
        #    (if there's any unsaved)
        # 2. it'll then ask us what we want to load

        
    def READFILENAME (self, message, suffix):
        if this.theorypath and this.theorypath!="":
           path, file = os.path.split(this.theorypath)
        else:
           path, file = None, None
        newpath=FileDialog.askopen(this.root, title="Select a new theory file", initialfile=file, initialdir=path, filetypes=[('All top-level theories', '*.jt'), ('All files', "*")])
        this.REPLY(newpath)
        if newpath!="":
           this.theorypath=newpath
           this.wm_update(newpath)
           this.proofpath=None
           
    def Load(self, path):
        this.COMMAND("use \"%s\"" % path)
    
    def LoadProof(self, path):
        this.proofpath=path
        this.COMMAND("use \"%s\"" % path)
        
    def GetSaveFile(self, fromtheory=0, cancel="Cancel"):
        if this.proofpath and this.proofpath!="":
           path, file = os.path.split(this.proofpath)
        elif fromtheory and this.theorypath and this.theorypath!="":
           path, file = os.path.split(this.theorypath)
           stem, ext = os.path.splitext(file)
           file = stem+".jp"
        else:
           path='.'
           file=None
        path = FileDialog.asksaveas(this.root, cancel=cancel, open='Save', title="Select file to save proofs", initialdir=path, initialfile=file, filetypes=[('All proof files', "*.jp"), ('All files', "*")])        
        return path
        
    def Save(self):
        this.COMMAND("saveproofs true")
        
    def SaveAs(self):
        this.proofpath = this.GetSaveFile(fromtheory=1)
        if this.proofpath!="": this.COMMAND("saveproofs true")
    
    def WRITEFILENAME (self, message, ext):
        if this.proofpath and this.proofpath!="": 
           this.REPLY (this.proofpath)
           this.showEvent ("Saving proof: "+this.proofpath)
        else:
           path=""
           path = this.GetSaveFile(fromtheory=1)
           if path!="": this.proofpath=path
           this.REPLY (path)
           if path!="": 
              this.showEvent ("Saving proof: "+this.proofpath)
        
#
#
#############################################################################
        
    def loginname(self):
        try:
          return os.environ['LOGNAME']
        except:
          try:
            return os.environ['USER']
          except:
            return "Someone or other whose name I don't know"
     
    def PrintView(self, *ignored):
        tempfile.template = "japelpr"
        tmp = tempfile.mktemp() + ".jps"
        this.proof.Print( file       = tmp,
                          save       = 0,
                          canrotate  = this.sessionstatus['permitrotation'],
                          mustrotate = this.sessionstatus['requirerotation'],
                          provisos   = this.provisos.text(),
                          userID     = this.loginname()
                        )
                        
        # printerpipe=os.popen("lpc status | grep : | sed s/://", 'r')
        # printers=printerpipe.readlines()
        # printerpipe.close()
        # STDERR("Printers :",printers)
        # STDERR(this.preferences['proofprint'] + " "+tmp, 'r')
        process=os.popen(this.preferences['proofprint'] + " "+tmp, 'r')
        status = process.close()
        if status:
          this.showEvent('Proof printing failed.')
        else:  
          this.showEvent('Proof printed from: '+tmp)
        os.remove(tmp)
        
            
    def SaveView(self, *ignored):
        if this.viewpath:
               path, file = os.path.split(this.viewpath)
        else:
               path, file= None, None
        fileformat = this.preferences['filename']
        if this.preferences['autoname']:
           if fileformat != "":
              counter = this.preferences['counter'] 
              this.wm_update(this.theorypath)
              file = expand(fileformat, { 't': this.theory, '#': counter})
           else: 
              file = None
              
        path = FileDialog.asksaveas(this.root, open='Save', initialfile=file, initialdir=path, filetypes=[('All postscript proof files', "*.jps"), ('All files', "*")])
        if path and path!="":
           if os.path.basename(path)==file:
              this.preferences['counter'] = counter+1
           this.viewpath=path
           this.proof.Print( file       = path,
                             save       = 1,
                             canrotate  = this.sessionstatus['permitrotation'],
                             mustrotate = this.sessionstatus['requirerotation'],
                             provisos   = this.provisos.text())
           this.showEvent('Postscript Proof Saved: '+path)
#
#
#
    def SavePrintable(self, *ignored):
        if this.pviewpath:
               path, file = os.path.split(this.pviewpath)
        else:
               path, file= None, None
        fileformat = this.preferences['pfilename']
        if this.preferences['autoname']:
           if fileformat != "":
              counter = this.preferences['counter'] 
              this.wm_update(this.theorypath)
              file = expand(fileformat, { 't': this.theory, '#': counter})
           else: 
              file = None
              
        path = FileDialog.asksaveas(this.root, open='Save', initialfile=file, initialdir=path, filetypes=[('All printable files', "*.pjp"), ('All files', "*")])
        if path and path!="":
           if os.path.basename(path)==file:
              this.preferences['counter'] = counter+1
           this.pviewpath=path
           this.COMMAND("print "+path)
           this.showEvent('Printable Proof Saved: '+path)
#
#
############################################################################# 


    def StartShellFor(self, shellclass, title, font=None, ops=[], dict=None, help=None):
        nroot = Tkinter.Toplevel()
        nroot.title(title)
        nroot.iconname(title)
        h = shellclass(nroot, mainproofwindow=self, font=font, ops=ops, dict=dict, help=help) 
        exitButton = Button(h.buttons(), text = 'Close', command = nroot.destroy)
        exitButton.pack(side = 'right')
        h.pack(expand='true', fill='both')
        
    def StartShellForPython(self):
        global debugDict
        this.StartShellFor(Debugger, "Jape Interface Debugger", dict=debugDict,
        help="""
This is the Jape GUI debugger. Don't use it unless you know what we're doing!
""")
    
    def StartShellForJape(self):
        this.StartShellFor(TacticShell, "Jape Tactic Shell", font=this.fontfamily.fontMetricsForClass('menu'), ops=this.operators,
        help="""
This is the Jape tactic shell.

You can type tactic expressions in the top window. They are executed when you type  <Return> in that window, or when you press the Execute button.

The bottom window shows the history of the tactic shell session. Navigate in the history using the Up and Down keys (or Prev and Next buttons)
""")

    def StartShellForJapeCommand(self):
        this.StartShellFor(CommandShell, "Jape Command Shell", font=this.fontfamily.fontMetricsForClass('menu'), ops=this.operators,
        help="""

""")
    def Splash(self, root): 
        return Splash.Splash(root, imagefile=this.resource("japelogo.gif"), text = "Loading Jape")
    
    def showError(self, messageText):
        this.showMessageDialog(messageText, bitmap='error', font=this.fontfamily.fontForClass('menu'))

    def SHOWDISPROOFPANE(self):
        this.paned.configure(topmin=0.2)

    def SHOWPROVISOPANE(self):
        this.paned.configure(botmin=0.2)

    def SHOWPROVISOLINE(self, fontnum, text):
        this.SHOWPROVISOPANE()
        this.provisos.SHOWPROVISOLINE(fontnum, text)

    def HIDEDISPROOFPANE(self):
        this.paned.configure(topfraction=0.001)

    def HIDEPROVISOPANE(self):
        this.paned.configure(botfraction=0.001)
    

############ GUI PROTOCOL #############################################################
#
#       The methods below implement the GUI SETUP protocol
#       Methods invoked during PROOFS are defined directly in JapeProofCanvas
#       The dispatcher declares methods appropriately
#

#
#       These  are enquiries and must give answers
#
    def HOWTO(self, action):
        this.REPLY(this.howto[action])
        
    howto = { "textselect":         "Drag with button 2 over the text to be selected",
              "formulaselect":      "Click with button 1 (or control-button 1) on the formula to be selected",
              "drag":               "Press on the formula with button 1 and then drag button 1"}
              
    def PROCRUSTES(self, width, ellipsis, font, text):
        # size of prefix of text which fits within (width - width(ellipsis))
        return len(text)
        
    def SETPROOFPARAMS(self, id, thickness):
        this.proof.SETPROOFPARAMS(id, thickness)
        this.disproof.SETLINETHICKNESS(thickness)

    # this stuff has changed to accommodate a more rational treatment of origins
    def PROOFPANEGEOMETRY(self):
        w, h = this.paned.midsize()
        ox, oy = this.proof.panegeometry()
        this.REPLY("%d %d %d %d"%(w, h-5, ox, oy)) # leave a margin at bottom

    def DISPROOFPANEGEOMETRY(self):
        w, h = this.paned.topsize()
        ox, oy = this.disproof.panegeometry()
        this.REPLY("%d %d %d %d"%(w, h-5, ox, oy)) # leave a margin at bottom

    def ASKSTART(self):
        this._askbuttons=[]
        
    def ASKBUTTON(self, buttonname):
        this._askbuttons.append(buttonname)
 
    def ASKNOW(self, severity, question, default):
        reply=JapeDialogue(this.root, 
                           title="Jape Dialogue", 
                           text=question, 
                           bitmap=this.askmaps[int(severity)], 
                           font=this.fontfamily.fontForClass('menu'),
                           default = int(default),
                           buttons=this._askbuttons)
        this.REPLY(str(reply))
        
    def ASKDANGEROUS(self, question, doitbut, dontitbut):
        reply=JapeDialogue(this.root, 
                           title="Jape Dialogue", 
                           text=question, 
                           bitmap='questhead', 
                           font=this.fontfamily.fontForClass('menu'),
                           default = 0,
                           buttons=[dontitbut, "Cancel", doitbut])
        # 
        this.REPLY(str([2, 0, 1][reply]))
    
        
    askmaps = ['info', 'warning', 'error', 'error']

#
#       Sending material to rhe proof engine
#
           
    def SEND(self, cmd):
        this.proofengine.SEND(cmd)
        
    def COMMAND(self, cmd):
        this.proofengine.COMMAND(cmd)
#
#       These  are commands
#

    def CLEARPROOFPANE(self):
        this.proof.Clear()           
    
    def CLEARDISPROOFPANE(self):
        this.disproof.Clear()
        this.tileframe.Clear()           
        
    def CLOSEPROOF(self, proofnum):
        proofnum=int(proofnum)
        # Mark numbered proof as closeable
        this.proofSelect.close(proofnum)
        this.CLEARPROOFPANE()
        this.CLEARDISPROOFPANE()
        this.HIDEDISPROOFPANE()
        this.provisos.CLEARPROVISOVIEW()
        this.provisos.CLEARGIVENS()
        
    def OPENPROOF(self, conjecture, proofnum):
        proofnum=int(proofnum)
        # notification of the start of a proof of the given conjecture
        this.proofSelect.add(conjecture, proofnum)
        this.CLEARPROOFPANE()
        this.CLEARDISPROOFPANE()
        this.provisos.CLEARPROVISOVIEW()
        this.provisos.CLEARGIVENS()
        
    def QUIT(self):
        this.quitproc()

    def Quit(self):
        # from the Quit button
        this.COMMAND("QUIT")
        
    def onQuit(self, quitproc):
        this.quitproc = quitproc

    def DONTQUIT(self):
        pass
        
    def TERMINATE(self):
        this.QUIT()
        
    def VERSION(self, version):             ##=/=##
        this.appname = version
        # Now we know there's an engine, set some default values
        this.COMMAND("steps %d"%this.preferences['proofsteps'])
    
    def SETCOMMENT(self, comment=None):          
        if comment: this.showError(comment)

    def SETINVISCHARS(self, onbra,onket,offbra,offket,outbra,outket,lockbra,lockket):
        this.proof.SETINVISCHARS(onbra,onket,offbra,offket,outbra,outket,lockbra,lockket)
        this.disproof.SETINVISCHARS(onbra,onket,offbra,offket,outbra,outket,lockbra,lockket)
            
    def SETTEXTSELECTIONMODE(self, mode):
        return # for now
        
    ### Font setup
    
    def SETFONTS(self, family):
        try:
          fam, form = FontAlias[family]
        except:
          Show.Error("No aliases for %s font family" % family)
        else:
         this.fontfamily.configure(
                            fontformat  = form,
                            fontfamily  = fam,
                            textsize    = (this.preferences['textsize']),
                            menusize    = (this.preferences['menusize']),
                            commentsize = (this.preferences['commentsize'])
                          )
         this.proof.setFont(this.fontfamily)
         
    def FONTINFO(self, fontnum):
        fontnum=int(fontnum)
        asc, desc, lead = this.fontfamily.FontInfo(JapeGUI.fontencoding[fontnum])
        this.REPLY("%d %d %d" % (asc, desc, lead)) # 4 is leading (what does he use it for?)
    
    def STRINGSIZE(self, fontnum, text=""):
        fontnum=int(fontnum)
        if text=="":
           meas, asc, desc = this.fontfamily.Measure(JapeGUI.fontencoding[fontnum], " ")
           meas = 0
        else:
           meas, asc, desc = this.fontfamily.Measure(JapeGUI.fontencoding[fontnum], text)
        # I make things a tad wider so as not to have selection difficulties
        this.REPLY("%d %d %d" % (meas+4, asc, desc))
                

    #######################################################################
    

    #######################################################################
    
    def OPERATORSBEGIN(self):
        this.operators = []

    def OPERATOR(self, op):
        this.operators.append(op)

    def OPERATORSEND(self):
        pass
        
    ### Menus
    
    def EMPTYMENUSANDPANELS(self):
        this.CANCELMENUSANDPANELS()

    def CANCELMENUSANDPANELS(self):
        this.proof.Clear()
        this.provisos.CLEARPROVISOVIEW()
        # kill all but the file and edit menus
        # and remove all the edit menu entries
        for f in this.menuBar.menuNames():
            if f in ['edit', 'proof - menu']:
               this.menuNamed('edit').delete()
            elif f in ['file', 'help']:
               pass
            else:
               this.menuBar.deleteMenu(f)
        for panel in this.panels.values():
            panel.destroy()
        this._Initialise_Auxiliary_Mappings()
        this.CLEARDISPROOFPANE()
        this.HIDEDISPROOFPANE()
        this.HIDEPROVISOPANE()

    def _Initialise_Auxiliary_Mappings(self):
        for m in this.menuBar.menuNames():
            this.menuBar.resetbuttons(m)
        this.panels = {}

    def menuNamed(self, internalname):
        return this.menuBar.menuNamed(internalname)
        
    def NEWMENU(self, menuname, caption):
        if nonascii(caption):
           this.menuBar.addmenu(menuname,  
             text=caption, side='left', tearoff=1,
             font=this.fontfamily.fontForClass('menu')
             ) 
        else:
           this.menuBar.addmenu(menuname,  
             text=caption, side='left', tearoff=1,
             ) 

    def MENURADIOBUTTONSTART(self, menuname):
        this.currentmenuname     = menuname
        this.currentradiobuttons = []
        
    def MENURADIOBUTTONPART(self, label, cmd):
        this.currentradiobuttons.append((label, cmd))

    def MENURADIOBUTTONEND(self):
      var = StringVar()
      for label, cmd in this.currentradiobuttons:
        this.menuBar.addradiobutton(this.currentmenuname, 
        label    = label,
        value    = cmd,
        variable = var,
        command  = lambda self     = self, 
                          menuname = this.currentmenuname, 
                          label    = label, 
                          val      = this.menuBar.value: this.COMMAND(val(menuname, label)),
        font     = this.fontfamily.fontForClass('menu'),
        )

    def MENUCHECKBOX(self, menuname, label, cmd):
        this.menuBar.addcheckbutton(menuname, 
        label   = label,
        value   = cmd,
        command = lambda self     = self, 
                         menuname = menuname, 
                         label    = label, 
                         val      = this.menuBar.value: this.COMMAND(val(menuname, label)),
        font    = this.fontfamily.fontForClass('menu'),
        )

        
    def MENUENTRY(self, menuname, label, cmd):
        this.menuBar.addbutton(menuname, 
        label   = label,
        value   = cmd,
        command = lambda self     = self, 
                         menuname = menuname, 
                         label    = label, 
                         val      = this.menuBar.value: this.COMMAND(val(menuname, label)),
        font    = this.fontfamily.fontForClass('menu'),
        )

    def MENUSEP(self, menuname):
        this.menuBar.addseparator(menuname)

    def TICKMENUITEM(self, menuname, label, newstate):
        newstate = (newstate=='1')
        this.menuBar.tick(menuname, label, newstate)

    def ENABLEMENUITEM(self, menuname, label, newstate):
        newstate = (newstate=='1')
        # Mac -> XinuL
        if menuname=='file' and label == 'Close': label='Quit'
        if this.menuBar.getentry(menuname,label):
            this.menuBar.activate(menuname, label, newstate)
        # Disproof panel 
        if menuname=="edit" and label=="Disprove" and newstate: 
           this.SHOWDISPROOFPANE()


    def MAKEMENUSVISIBLE(self):
        for panel in this.panels.values():
            panel.makeVisible()

    def MENUENTRYEQUIV(self, menuname, label, shortcut):
        # I ignore these: they're done automagically by my menu machinery
        pass
    
    ### Panels
    def NEWPANEL(self, panelname, isconjecturepanel):
        this.panels[panelname] = Panel(self, panelname, isconjecturepanel)
    
    def PANELENTRY(self, panelname, formula, label):
        this.panels[panelname].PANELENTRY(formula, label)
        
    def MARKPANELENTRY(self, panelname, label, value):
        this.panels[panelname].MARKPANELENTRY(label, value)
        
    def PANELBUTTON(self, panelname, label, cmd):
        this.panels[panelname].PANELBUTTON(label, cmd)
        
    def SETPANELBUTTON(self, panelname, label, state):
        this.panels[panelname].SETPANELBUTTON(label, state)
        
    def SELECTPANELENTRY(self, panelname, label):
        this.panels[panelname].SELECTPANELENTRY(label)
        
    def PANELCHECKBOX(self, panelname, label, prefix):
        this.panels[panelname].PANELCHECKBOX(label, prefix)
        
    def BEGINRADIOBUTTON(self, panelname):
        this.panels[panelname].BEGINRADIOBUTTON()
    
    def RADIOBUTTONENTRY(self, panelname, label, cmd):
        this.panels[panelname].RADIOBUTTONENTRY(label, cmd)
        
    def ENDRADIOBUTTON(self, panelname):
        this.panels[panelname].ENDRADIOBUTTON()
        
    ### Givens
    def CLEARGIVENS(self):
        this.provisos.CLEARGIVENS()
    
    def SETGIVENS(self):   pass 
    
    def GIVEN(self, number, formula): 
        this.provisos.SHOWGIVEN(int(number), formula)
    
    
    
    ### Various
        
    def RESETCACHE(self): # cache for text widths
        pass
        
    def CLEARCHOICES(self):
        this.choices=[]

    def SETCHOICE(self, line, thing=None):
        this.choices.append((line, thing))
        
    def SETCHOICELINE(self):
        this.choices.append(())
        
    def MAKECHOICE(self, message):
        choice=JapeChoiceDialogue(this.root, this.fontfamily.fontForClass('text'), this.choices, message)
        this.REPLY(choice.answer) 

    def SETALERT(self, comment):          
        this.showError(comment)

}
