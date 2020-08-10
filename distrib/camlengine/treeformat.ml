(*
    Copyright (C) 2003-19 Richard Bornat & Bernard Sufrin
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

    This file is part of the jape proof engine, which is part of jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).

*)

module type Fmt =
  sig
    type treelayout and term
    
    (* the complexity of this datatype, and the shenanigans relating to its translation, are all due to 
     * the need to have a simple interpretation of double-clicking on a reason.  Essentially, we have
     * a round-robin of formats in RotatingFilter and we have a toggle of Hide/Expose.
     * RB 22/xii/99
     *)
    type treeformat = TreeFormat of (treeformat_kind * treeformat_filter)
    and treeformat_kind = HideRootFormat | HideCutFormat | SimpleFormat
    and treeformat_filter =
      | DefaultFilter
      | RotatingFilter of (int * (bool * string * int list option) list)
    (* int is rotation position;
     *        bool is Compressed or not;
     *               string is "...%s..." type format for generating reason at this node;
     *                        int list is filtering of subtrees
     *       when we get to the end of the list, we use rawfmtstring,
     *       and we show all children without hiderooting
     *)         

    val treeformatmerge      : treeformat * treeformat -> treeformat
    val string_of_treeformat : treeformat -> string
    val neutralformat        : treeformat

    val format_of_layout : (term -> string) -> (term -> int list) -> treelayout -> treeformat
    val layouts_of_format : treeformat -> treelayout list
    (* because of the use of negative numbers in paths to navigate internal cuts, and the 
     * wierd way that the root of an internal cut is addressed (see prooftree.sml), 
     * DON'T DON'T DON'T do manipulations of the list in a FmtPath.
     *
     * IN PARTICULAR, note that FmtPath [] is NOT NECESSARILY the root path of a tree: use
     * rootPath to give you the correct path instead.
     *
     * The functions parentPath and siblingPath ought to give you the movement you need ...
     * RB 21/i/00
     *)
     
    type fmtpath = FmtPath of int list
    val string_of_fmtpath : fmtpath -> string
  end

module Fmt : Fmt with type treelayout = Treelayout.treelayout
                  and type term = Termtype.term
=
  struct
    open Listfuns
    open Optionfuns
    open Sml
    open Stringfuns
    open Termfuns
    open Treelayout
    
    type treelayout = Treelayout.treelayout
     and term = Termtype.term
    
    (* the complexity of this datatype, and the shenanigans relating to its translation, are all due to 
     * the need to have a simple interpretation of double-clicking on a reason.  Essentially, we have
     * a round-robin of formats in RotatingFilter and we have a toggle of Hide/Expose.
     * RB 22/xii/99
     *)
     
    type treeformat = TreeFormat of (treeformat_kind * treeformat_filter)
    and treeformat_kind = HideRootFormat | HideCutFormat | SimpleFormat
    and treeformat_filter =
        DefaultFilter
      | RotatingFilter of (int * (bool * string * int list option) list)
    (* int is rotation position;
     *        bool is Compressed or not;
     *               string is "...%s..." type format for generating reason at this node;
     *                        int list is filtering of subtrees
     *       when we get to the end of the list, we use rawfmtstring,
     *       and we show all children without hiderooting
     *)         

    let neutralformat = TreeFormat (SimpleFormat, DefaultFilter)
    let string_of_intlist = bracketed_string_of_list string_of_int ","
    let nf_string = string_of_triple string_of_bool enQuote (string_of_option string_of_intlist) ","
    
    let rec string_of_treeformat =
      fun (TreeFormat pair) ->
        "TreeFormat" ^
          string_of_pair string_of_treeformat_kind string_of_treeformat_filter "," pair
    
    and string_of_treeformat_kind =
      function
      | HideRootFormat -> "HideRootFormat"
      | HideCutFormat  -> "HideCutFormat"
      | SimpleFormat   -> "SimpleFormat"
    
    and string_of_treeformat_filter =
      function
      | DefaultFilter        -> "DefaultFilter"
      | RotatingFilter stuff ->
          "RotatingFilter" ^
            string_of_pair string_of_int (bracketed_string_of_list nf_string ",") ","
              stuff
    
    (* if tf1 is hidden, so is the merge *)
    let rec treeformatmerge =
      fun (TreeFormat (tfk1, tff1), TreeFormat (tfk2, tff2)) ->
        let rec tfkmerge =
          function
            SimpleFormat, _ -> tfk2
          | _, _ -> tfk1
        in
        let rec tffmerge =
          function
            DefaultFilter, tff2 -> tff2
          | tff1, DefaultFilter -> tff1
          | RotatingFilter (i1, nfs1), RotatingFilter (i2, nfs2) ->
              let res =
                match nfs1, nfs2 with
                  [true, "%s", None], [false, s, isopt] ->
                    i1, [true, s, isopt]
                | _ ->
                    (* stupid hack to implement Compressed *)
                    let rec redundant nf =
                      List.exists (fun nf' -> nf = nf') nfs1
                    in
                    let nfs2' = redundant <| nfs2 in
                    let i2' = min i2 (List.length nfs2') in
                    (if i1 = List.length nfs1 then i1 + i2' else i1), nfs1 @ nfs2'
              in
              RotatingFilter res
        in
        TreeFormat (tfkmerge (tfk1, tfk2), tffmerge (tff1, tff2))
    
    let rec format_of_layout fmtf tnsf l =
      let rec f_of_l compress (fmt, tns) =
        TreeFormat
          (SimpleFormat,
           RotatingFilter (0, [compress, fmtf fmt, optf tnsf tns]))
      in
      match l with
      | HideRootLayout         -> TreeFormat (HideRootFormat, DefaultFilter)
      | HideCutLayout          -> TreeFormat (HideCutFormat, DefaultFilter)
      | CompressedLayout stuff -> f_of_l true stuff
      | NamedLayout stuff      -> f_of_l false stuff
    
    let term_of_ints tns = Termstore.registerTup (",", term_of_int <* tns)
    let term_of_string s = Termstore.registerLiteral(Termtype.String s)

    let rec layouts_of_format (TreeFormat (tfk, tff) (* as f *)) =
      let rec layout =
        function
          (true, s, isopt) :: nfs ->
            CompressedLayout (ly s isopt) :: layout nfs
        | (false, s, isopt) :: nfs -> NamedLayout (ly s isopt) :: layout nfs
        | [] -> []
      and ly s isopt = term_of_string s, optf term_of_ints isopt in
      let ls =
        match tff with
        | DefaultFilter           -> []
        | RotatingFilter (i, nfs) -> layout (drop i nfs @ take i nfs)
      in
      match tfk with
      | HideRootFormat -> HideRootLayout :: ls
      | HideCutFormat  -> HideCutLayout :: ls
      | SimpleFormat   -> ls

    (* because of the use of negative numbers in paths to navigate internal cuts, and the 
     * wierd way that the root of an internal cut is addressed (see prooftree.sml), 
     * DON'T DON'T DON'T do manipulations of the list in a FmtPath.
     *
     * IN PARTICULAR, note that FmtPath [] is NOT NECESSARILY the root path of a tree: use
     * rootPath to give you the correct path instead.
     *
     * The functions parentPath and siblingPath ought to give you the movement you need ...
     * RB 21/i/00
     *)
    type fmtpath = FmtPath of int list
    (* VisPaths, at the present, are still simple lists of non-negative integers ... *)
    
    let rec string_of_fmtpath = fun (FmtPath p) -> "FmtPath " ^ bracketed_string_of_list string_of_int "," p
  end

(* -------------------------- prooftrees for display, after formatting -------------------------- *)

module type VisFmt =
  sig
    (* VisPaths, at the present, are still simple lists of non-negative integers ... *)
    type vispath = VisPath of int list
    val string_of_vispath : vispath -> string
    type visformat = VisFormat of (bool * bool)
    (* ismultistep, ishiddencut *)
       
    val string_of_visformat : visformat -> string
  end

module VisFmt : VisFmt =
  struct
    open Stringfuns
    open Listfuns
    
    type visformat = VisFormat of (bool * bool)
    (* ismultistep, ishiddencut *)
       
    let string_of_visformat  (VisFormat f) =
      "VisFormat" ^ string_of_pair string_of_bool string_of_bool "," f
        
    type vispath = VisPath of int list

    let string_of_vispath (VisPath p) = "VisPath " ^ bracketed_string_of_list string_of_int "," p
  end
