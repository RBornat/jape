(*
    Copyright (C) 2003-17 Richard Bornat & Bernard Sufrin
     
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

type idclass = NoClass
             | FormulaClass
             | VariableClass
             | ConstantClass
             | NumberClass
             | StringClass
             | OperatorClass
             | SubstClass
             | BagClass of idclass
             | ListClass of idclass

let rec catelim_string_of_idclass a1 a2 =
  match a1, a2 with
    NoClass      , tail -> "NoClass" :: tail
  | FormulaClass , tail -> "FormulaClass" :: tail
  | VariableClass, tail -> "VariableClass" :: tail
  | ConstantClass, tail -> "ConstantClass" :: tail
  | NumberClass  , tail -> "NumberClass" :: tail
  | StringClass  , tail -> "StringClass" :: tail
  | OperatorClass, tail -> "OperatorClass" :: tail
  | SubstClass   , tail -> "SubstClass" :: tail
  | BagClass c   , tail -> "BagClass(" :: catelim_string_of_idclass c (")" :: tail)
  | ListClass c  , tail -> "ListClass(" :: catelim_string_of_idclass c (")" :: tail)

let string_of_idclass = Listfuns.stringfn_of_catelim catelim_string_of_idclass

exception ParseIdclass_ of string

(* this is NOT efficient, and I don't care. RB 26/xi/2008 *)
let idclass_of_string s =
	let rec idwords cs w ws =
		let combine w ws =
			match w with
				| [] -> ws
				| _  -> Sml.string_of_chars (List.rev w) :: ws
		in
		match cs with
			| [] -> List.rev (combine w ws)
			| '('::cs -> idwords cs [] ("("::combine w ws) 
			| ')'::cs -> idwords cs [] (")"::combine w ws) 
			| c::cs -> idwords cs (c::w) ws
	in
	let rec idclass_of_rest ws = 
		match List.rev ws with
			| ")" :: ws' -> idclass_of_words (List.rev ws')
			| _ -> raise (ParseIdclass_ s)
	and idclass_of_words ws =
		match ws with
			| ["NoClass"]  -> NoClass
		  | ["FormulaClass"]  -> FormulaClass
		  | ["VariableClass"]  -> VariableClass
		  | ["ConstantClass"]  -> ConstantClass
		  | ["NumberClass"]  -> NumberClass
		  | ["StringClass"]  -> StringClass
		  | ["OperatorClass"]  -> OperatorClass
		  | ["SubstClass"]  -> SubstClass
		  | "BagClass" :: "(" :: ws -> BagClass(idclass_of_rest ws)
		  | "ListClass" :: "(" :: ws -> ListClass(idclass_of_rest ws)
			| _ -> raise (ParseIdclass_ s)
	in
	idclass_of_words (idwords (Sml.chars_of_string s) [] [])
