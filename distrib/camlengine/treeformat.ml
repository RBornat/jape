(*
	$Id$

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
     * a round-robin of formats in RotatingFormat and we have a toggle of Hide/Expose.
     * RB 22/xii/99
     *)
    type treeformat = TreeFormat of (treeformat_kind * treeformat_filter)
    and treeformat_kind = HideRootFormat | HideCutFormat | SimpleFormat
    and treeformat_filter =
        DefaultFormat
      | RotatingFormat of (int * (bool * string * int list option) list)
    (* int is rotation position;
     *        bool is Compressed or not;
     *               string is "...%s..." type format for generating reason at this node;
     *                        int list is filtering of subtrees
     *       when we get to the end of the list, we use rawfmtstring,
     *       and we show all children without hiderooting
     *)         

    val treeformatmerge : treeformat * treeformat -> treeformat
    val treeformatstring : treeformat -> string
    val neutralformat : treeformat

    val layout2format :
      (term -> string) -> (term -> int list) -> treelayout -> treeformat
    val format2layouts : treeformat -> treelayout list
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
    val fmtpathstring : fmtpath -> string
  end

module Fmt : Fmt with type treelayout = Treelayout.treelayout
                  and type term = Term.Funs.term
=
  struct
    open Listfuns
    open Optionfuns
    open Sml
    open Stringfuns
    open Term.Funs
    open Treelayout
    
    type treelayout = Treelayout.treelayout
     and term = Term.Funs.term
    
    (* the complexity of this datatype, and the shenanigans relating to its translation, are all due to 
     * the need to have a simple interpretation of double-clicking on a reason.  Essentially, we have
     * a round-robin of formats in RotatingFormat and we have a toggle of Hide/Expose.
     * RB 22/xii/99
     *)
     
    type treeformat = TreeFormat of (treeformat_kind * treeformat_filter)
    and treeformat_kind = HideRootFormat | HideCutFormat | SimpleFormat
    and treeformat_filter =
        DefaultFormat
      | RotatingFormat of (int * (bool * string * int list option) list)
    (* int is rotation position;
     *        bool is Compressed or not;
     *               string is "...%s..." type format for generating reason at this node;
     *                        int list is filtering of subtrees
     *       when we get to the end of the list, we use rawfmtstring,
     *       and we show all children without hiderooting
     *)         

    let neutralformat = TreeFormat (SimpleFormat, DefaultFormat)
    let intliststring = bracketedliststring string_of_int ","
    let nf_string = triplestring string_of_bool enQuote (optionstring intliststring) ","
    let rec treeformatstring =
      fun (TreeFormat pair) ->
        "TreeFormat" ^
          pairstring treeformat_kindstring treeformat_filterstring "," pair
    and treeformat_kindstring =
      function
        HideRootFormat -> "HideRootFormat"
      | HideCutFormat -> "HideCutFormat"
      | SimpleFormat -> "SimpleFormat"
    and treeformat_filterstring =
      function
        DefaultFormat -> "DefaultFormat"
      | RotatingFormat stuff ->
          "RotatingFormat" ^
            pairstring string_of_int (bracketedliststring nf_string ",") ","
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
            DefaultFormat, tff2 -> tff2
          | tff1, DefaultFormat -> tff1
          | RotatingFormat (i1, nfs1), RotatingFormat (i2, nfs2) ->
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
                    let i2' = min (i2) (List.length nfs2') in
                    (if i1 = List.length nfs1 then i1 + i2' else i1), nfs1 @ nfs2'
              in
              RotatingFormat res
        in
        TreeFormat (tfkmerge (tfk1, tfk2), tffmerge (tff1, tff2))
    let rec layout2format fmtf tnsf l =
      let rec l2f compress (fmt, tns) =
        TreeFormat
          (SimpleFormat,
           RotatingFormat (0, [compress, fmtf fmt, try__ tnsf tns]))
      in
      match l with
        HideRootLayout -> TreeFormat (HideRootFormat, DefaultFormat)
      | HideCutLayout -> TreeFormat (HideCutFormat, DefaultFormat)
      | CompressedLayout stuff -> l2f true stuff
      | NamedLayout stuff -> l2f false stuff
    
    let ints2term tns = Term.Store.registerTup (",", int2term <* tns)
    let term_of_string s = Term.Store.registerLiteral(Term.Type.String s)

    let rec format2layouts =
      fun (TreeFormat (tfk, tff) as f) ->
        let rec layout =
          function
            (true, s, isopt) :: nfs ->
              CompressedLayout (ly s isopt) :: layout nfs
          | (false, s, isopt) :: nfs -> NamedLayout (ly s isopt) :: layout nfs
          | [] -> []
        and ly s isopt = term_of_string s, try__ ints2term isopt in
        let ls =
          match tff with
            DefaultFormat -> []
          | RotatingFormat (i, nfs) -> layout (drop i nfs @ take i nfs)
        in
        match tfk with
          HideRootFormat -> HideRootLayout :: ls
        | HideCutFormat -> HideCutLayout :: ls
        | SimpleFormat -> ls

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
    
    let rec fmtpathstring = fun (FmtPath p) -> "FmtPath " ^ bracketedliststring string_of_int "," p
  end

(* -------------------------- prooftrees for display, after formatting -------------------------- *)

module type VisFmt =
  sig
    (* VisPaths, at the present, are still simple lists of non-negative integers ... *)
    type vispath = VisPath of int list
    val vispathstring : vispath -> string
    type visformat = VisFormat of (bool * bool)
    (* ismultistep, ishiddencut *)
       
    val visformatstring : visformat -> string
  end

module VisFmt : VisFmt =
  struct
    open Stringfuns
    open Listfuns
    
    type visformat = VisFormat of (bool * bool)
    (* ismultistep, ishiddencut *)
       
    let visformatstring  (VisFormat f) =
	  "VisFormat" ^ pairstring string_of_bool string_of_bool "," f
        
    type vispath = VisPath of int list

    let vispathstring (VisPath p) = "VisPath " ^ bracketedliststring string_of_int "," p
  end
