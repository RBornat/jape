(*
    $Id$

    Copyright (C) 2003-4 Richard Bornat & Bernard Sufrin
     
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
