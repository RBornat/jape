(* $Id$ *)

(*
  4.1: first public release of version 4.         BAS.
  4.2: boxdraw made much prettier.                BAS.
  4.3: removed infix to allow compilation by
       smlnj-0.93                                 RB
  4.4: made this file a functor, to allow compilation
       by sourcegroup mechanism in smlnj-0.93
       (and changed Title to mixed case)          RB
  4.5: Prompts selecting hypothesis changed slightly
       Added MAKEFRESH parameter forms
       to support ?HIT declarations
       Fixed small bug in treedraw                BAS.
  4.6: Added
      JAPE(SUBGOAL...) tactic
       Made MEMBER and _MAP coherent               BAS.
  4.7: Richard's improvements and some fixes
       concerning debracketing                    BAS.
  4.9: Attempt at better online error reporting
       Symbol table reset now possible            BAS.
  7.1: various things done by Bernard to bring
       Views into early life, better explanation
       etc.
  8.1: multiple-conclusion sequents               RB
  8..9.2: records lost                            RB
  9.3: multi-window proofs                        RB
  *)
  
  (* Big change with 10.1. We have separated the notion of 'release' 
   * from 'RCS version'. So RCS version 10.1 corresponds to release 3.0
   
   3.0: first multi-window release               (RCS 10.1)  RB
   
   *)
  (* experimental comment 
     $Name$
     $Log$
     Revision 1.3  2002/07/04 17:25:57  richard
     minor changes to capitalisation

     Revision 1.2  2002/07/04 11:43:13  richard
     more simple changes to make things compile

     Revision 1.1  2002/07/03 08:09:08  richard
     translated, with a few alterations

     Revision 10.64.2.1  2002/07/01 16:42:04  richard
     capitalisation changes

     Revision 10.64  2002/05/17 18:27:47  richard
     fixed name capture bug in disproof (blush)

     Revision 10.63  2002/01/30 18:34:19  richard
     fixed (?) RULES bug; can now provide arguments ...

     Revision 10.62  2001/11/26 18:46:34  richard
     fixed negative coordinate bug

     Revision 10.61  2001/11/09 14:52:38  richard
     releasable I think.  Naff way of doing separate undos; no proper support for disproof world/line/label deletion

     Revision 10.60  2001/10/29 19:02:31  richard
     minor changes, but now has KEYBOARD specifier

     Revision 10.59  2001/10/12 11:20:45  richard
     first complete disproof engine; seems to be doing the right things.

     Revision 10.58  2001/10/09 08:07:29  richard
     first version which can save and reload proofs with disproof components. Still not treated properly when proof/disproof is finished, though.

     Revision 10.57  2001/09/21 15:47:34  richard
     5_0a7 -- predicate and intuitionistic stuff well on the way; interface more stable.

     Revision 10.56  2001/09/03 08:37:18  richard
     Copying proof & disproof now works well; also does coloured subformulae in disproof window.

     Revision 10.55  2001/08/29 15:07:04  richard
     a version which can do classical propositional disproof

     Revision 10.54  2001/07/30 14:41:18  richard
     5.0a3 -- able to set up classical disproof (but no replies from GUI yet)

     Revision 10.53  2001/07/18 12:37:12  richard
     just beginning to send disproof information to the GUI

     Revision 10.52  2001/07/12 13:27:05  richard
     5.0a1 - first version in which forcing semantics seems to work

     Revision 10.51  2001/07/11 10:54:13  richard
     version 5_0a: can decode forcing semantics (but gets wrong answers ...)

     Revision 10.49  2001/07/04 13:10:20  richard
     version 4_3: subformula/token textselection, and a tidied up menu description interface

     Revision 10.48  2000/09/27 16:32:35  richard
     big checkin, Sept 2000
     version 4_2 and 4_2_1

     Revision 10.47  1999/10/13 20:58:33  richard
     new boxdraw treatment of transitivity and cut;
     new tactics (_RR ALERT)

     Revision 10.46  1998/11/06 15:46:55  sufrin
     Merged bernard's 3.4bcx with richard's 3.4bd

     Revision 10.45  1998/10/27 16:09:06  sufrin
     Added Oracle stuff from jape3.3xd

     Revision 10.44  1998/06/03 16:22:30  richard
     fixed some transitivy / boxdraw bugs

     Revision 10.43  1998/05/27 16:30:25  richard
     first version with hidetransitivity, hidereflexivity

     Revision 10.41  1998/05/20 16:56:46  richard
     new boxdraw, ready for treatment of transitivity, perhaps

     Revision 10.40  1998/04/01 14:54:10  richard
     fixed an assoc law bug in tacticfuns; fixed an identity rule bug in boxdraw/absprooftree

     Revision 10.39  1997/12/22 17:47:29  richard
     new NOTONEOF proviso, new way of unifying lists to allow list weakening

     Revision 10.38  1997/12/15 18:47:03  richard
     slightly improved treatment of proviso failure error messages

     Revision 10.37  1997/12/12 20:22:21  richard
     begun to improve 'proviso failure' diagnostics

     Revision 10.36  1997/12/11 19:11:03  richard
     now stable, I believe, with derived rules

     Revision 10.35  1997/12/03 21:01:59  richard
     first version that seems to work with derived rules, including
     GIVENPANEL and new menu mechanism and GIVEN inside tactics

     Revision 10.34  1997/11/28 18:56:12  richard
     second attempt at derived rules, with GIVENMENUTAC and GIVENPANEL

     Revision 10.33  1997/11/19 13:17:51  richard
     continuing development of derived rules

     Revision 10.32  1997/11/17 22:28:16  richard
     first working derived rules version

     Revision 10.31  1997/11/10 18:39:11  richard
     derived rules

     Revision 10.30  1997/10/30 20:10:43  richard
     added SubstTacs, and used them in saving proofs

     Revision 10.29  1997/10/26 00:38:41  richard
     yet another attempt to make resolution proofs robust (and replayable)

     Revision 10.28  1997/10/23 14:41:21  richard
     changed the OUTFIX/AUXFIX/BRA/SEP/KET stuff in symboltype

     Revision 10.27  1997/10/22 17:58:14  richard
     version 3.2i allows overloaded LEFTFIX/OUTFIX declarations

     Revision 10.26  1997/10/15 17:52:52  richard
     improved error message in selection

     Revision 10.25  1997/10/15 17:44:47  richard
     fixed missed Selection_ exception in tacticfuns

     Revision 10.24  1997/10/10 13:14:21  richard
     spelling of words on hypothesis lines in boxdraw

     Revision 10.23  1997/09/29 17:53:09  richard
     some changes to capitalisation of identifiers;
     some bug fixes in tacticfuns

     Revision 10.22  1997/09/11 16:27:30  richard
     decapitalisation

     Revision 10.21  1997/09/02 18:41:15  richard
     fixed some bugs related to resnum*term -> element change in 3.2a or 3.2b

     Revision 10.20  1997/08/12 17:20:03  richard
     change from resnum*term in proof trees to element

     Revision 10.19  1997/06/04 10:29:23  richard
     multi-version sequents (judgements)

     Revision 10.18  1997/01/27 18:11:03  richard
     version 3.2a

     Revision 10.17  1997/01/09 13:59:54  richard
     now version 2

     Revision 10.13  1996/12/13 14:49:20  richard
     version 3.1e

     Revision 10.12  1996/12/09 18:40:46  richard
     version 3.1d, at last

     Revision 10.11  1996/11/18 14:51:31  richard
     version 3.1c at last!

     Revision 10.10  1996/10/27 18:56:44  richard
     major fix to make reload of proofs get resource numbering right,
     and to make boxdraw of same look sensible.

     Revision 10.9  1996/10/10 19:52:04  richard
     circularities banished;
     proofs come out in correct dependency order;
     WITHSUBSTSEL works again

     Revision 10.8  1996/07/12 11:15:21  richard
     Stage 0 of proper sequent syntax: functors compile but nothing works

     starting version 3.1 -- first version with proper sequent syntax RB
     
     Revision 10.7  1996/06/12 08:43:04  sufrin
     B's sml109 mods.
     made term.sml give freshVIDs starting from name provided RB
     made REPLAY rewrite terms it compares rather than unifies RB

     Revision 10.6  1996/04/19 15:54:42  richard
     version 3_0d established

     Fixed problem in variablebindings functions in term.sml RB
     Revision 10.5  1996/03/29 21:22:57  richard
     version 3_0c

     Revision 10.4  1996/02/25 00:23:04  richard
     version 3_0b

     Revision 10.3  1996/02/21 13:11:49  richard
     version 3_0a

   *)
   
  
   let version = "release 5.0b7" 
   let title = "Jape proof engine: "


