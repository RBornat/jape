(* $Id$ *)

module type Panelkind =
  sig
    type panelkind = TacticPanelkind | ConjecturePanelkind | GivenPanelkind
    type panelbuttoninsert =
      StringInsert of string | LabelInsert | CommandInsert
  end
(* $Id$ *)

module Panelkind : Panelkind =
  struct
    type panelkind = TacticPanelkind | ConjecturePanelkind | GivenPanelkind
    type panelbuttoninsert =
      StringInsert of string | LabelInsert | CommandInsert
  end
