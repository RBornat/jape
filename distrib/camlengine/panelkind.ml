(* $Id$ *)

module type T =
  sig
    type panelkind = TacticPanelkind | ConjecturePanelkind | GivenPanelkind
    type panelbuttoninsert =
      StringInsert of string | LabelInsert | CommandInsert
  end
(* $Id$ *)

module M : T =
  struct
    type panelkind = TacticPanelkind | ConjecturePanelkind | GivenPanelkind
    type panelbuttoninsert =
      StringInsert of string | LabelInsert | CommandInsert
  end
