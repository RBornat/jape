/*
        $Id: IFP_style_menus.j 631 2014-05-30 13:22:08Z sufrin $
*/

UMENU "Proof Style"
    RADIOBUTTON applyconjectures
        "Apply conjectures as theorems" IS all
    AND "Apply only proven theorems"    IS none
    INITIALLY none
    END
    
    CHECKBOX tryresolution "Cut, if necessary, when applying theorems"
    
    CHECKBOX autoselect "Select goal automatically"
END

INITIALISE tryresolution     false
INITIALISE showallproofsteps false
INITIALISE autoselect        true
INITIALISE hidecut           true
INITIALISE hidehyp           true


MENU "View"  
    
    RADIOBUTTON displaystyle IS
        "Box display"   IS box
    AND "Tree display"  IS tree
    INITIALLY box
    END
    
    CHECKBOX showallprovisos      "Show all provisos"
    CHECKBOX showallproofsteps    "Show compressed steps"
    CHECKBOX hideuselesscuts      "Hide unnecessary forward steps"
    
    SEPARATOR
    
    CHECKBOX hidecut              "Hide cuts in box display"
    CHECKBOX hidehyp              "Hide duplicated hyp lines in box display"
    
    SEPARATOR
    
    CHECKBOX hidetransitivity     "Hide transitive steps"
    CHECKBOX hidereflexivity      "Hide reflexive steps"
    
    SEPARATOR
    
    CHECKBOX foldsequents         "Fold long sequents in tree display"
    CHECKBOX multiassumptionlines "Multiple assumptions per line in box display"
    CHECKBOX foldformulae         "Fold long formulae in box display"

    SEPARATOR
    SEPARATOR
    
    CHECKBOX tactictracing "Trace tactic applications" 

END








